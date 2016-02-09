#lang racket/base
(require racket/string racket/list)
(require sugar/unstable/string sugar/coerce)

(provide (all-defined-out))

(define css-property-prefixes '("-moz-" "-webkit-" "-o-" "-ms-" ""))

(define (join-css-strings properties)
  (define line-ending ";\n")
  (define out-string (string-join properties line-ending))
  (if (out-string . ends-with? . line-ending) ; might already have the line ending, so don't duplicate it
      out-string
      (string-append out-string line-ending)))

(define (make-css-string p v)
    (string-join (list (->string p) (->string v)) ": "))

(define (make-css-strings property-prefixes property-suffix values)
  ; general function for creating groups of css properties
  ; with browser prefixes and one value
  (define (map-suffix suffix prefixes)
    (map (λ(prefix) (string-append prefix suffix)) prefixes))
  
  
  (define properties (map-suffix property-suffix property-prefixes))
  
  ; if single value provided, convert to list of values
  ; so that it will work with map in the next step
  (when (not (list? values))
    (set! values (make-list (length properties) values)))
  
  (map make-css-string properties values))



; editability can't be handled as pure css because firefox requires extra content-editable attribute.
; does it still? todo: further research, maybe this can be css only.
(define (editable . stuff)
  (define editable-string (make-css-editable))
  `(div ((style ,editable-string)(contenteditable "true")) ,@stuff))

(define (make-css-editable)
  (join-css-strings (list "user-modify: read-write"
                          "-moz-user-modify: read-write"
                          "-webkit-user-modify: read-write-plaintext-only"
                          "outline-style: none")))


(define (make-media-query starting-size ending-size max-width interval [step-size 1])
  (string-join (cons (format "@media all {html {font-size: ~apx;}}" starting-size)
                     (for/list ([size (in-range starting-size (sub1 ending-size) (* -1 step-size))]
                                #:unless (< size ending-size))
                       (format "@media all and (max-width:~apx){html {font-size: ~apx;}}" 
                               (- max-width (* interval (- starting-size size))) size))) "\n"))


(module+ main
  (displayln (make-media-query 15 11 980 60))
  (displayln (make-media-query 15 11 980 60 .5)))

(define (make-css-ot-features feature-tags [feature-values 1])
  ; if single value provided, upconvert to list
  (when (not (list? feature-tags))
    (set! feature-tags (list feature-tags)))
  
  ; same here: convert single value into list
  (when (not (list? feature-values))
    (let ([single-value feature-values])
      (set! feature-values (make-list (length feature-tags) single-value))))
  
  ; use single quotes in the formatter because css string might be used in an inline tag
  ; with form style="[string]" so double quotes are irritating
  (define feature-tag-string (string-join (map (λ(tag value) (format "'~a' ~a" tag value)) feature-tags feature-values) ", "))
  
  ; I hate accommodating old browsers but I'll make an exception because OT support is 
  ; critical to most MB projects
  ; if this comes before new-style -moz- declaration, it will work for all.
  (define feature-tag-string-old-firefox (string-join (map (λ(tag value) (format "'~a=~a'" tag value)) feature-tags feature-values) ", "))
  
  (define feature-tag-property "font-feature-settings")
  
  (join-css-strings (append
                     (make-css-strings '("-moz-") feature-tag-property feature-tag-string-old-firefox)
                     (make-css-strings css-property-prefixes feature-tag-property feature-tag-string))))


(define (make-css-hyphens [value "auto"])
  (join-css-strings (make-css-strings css-property-prefixes "hyphens" value)))

(define (make-css-small-caps)
  (join-css-strings (list "text-transform: lowercase" (make-css-ot-features "c2sc"))))

(define (make-css-caps)
  (join-css-strings (list "text-transform: uppercase" (make-css-ot-features "case"))))

(define (make-css-kerning)
  (join-css-strings (list "text-rendering: optimizeLegibility" (make-css-ot-features "kern"))))


(define (make-css-ligatures)
  (join-css-strings (list "text-rendering: optimizeLegibility" (make-css-ot-features "liga"))))

(define (make-css-background-gradient colors [stops #f] 
                                      #:radial [radial #f] 
                                      #:horizontal [horizontal #f]
                                      #:direction [direction #f])
  ; this doesn't handle old-style webkit syntax. todo: add it? I think I don't care
  
  ; check inputs for failure
  (when (or (not (list? colors)) (< (length colors) 2))
    (error "Not enough colors to make gradient in" colors))
  (when (and stops (< (length stops) (length colors)))
    (error "Not enough stops for given number of colors in" stops))
  
  (when (not stops) ; distribute colors evenly between 0 and 100
    ; new-stops is range of steps incremented properly and rounded to int, then append 100 to end
    (let ([new-stops `(,@(map floor (range 0 100 (/ 100 (sub1 (length colors))))) 100)])
      ; convert to list of percentages
      (set! stops (map (λ(x) (format "~a%" x)) new-stops))))
  
  ; color / percentage pairs separated by commas
  (define color-stop-string (string-join (map (λ(color stop) (format "~a ~a" color stop)) colors stops) ", "))
  
  ; set up gradient options
  (define gradient-type (if radial "radial" "linear"))
  (define gradient-direction (or direction (if horizontal "left" "top")))
  
  ; can't use standard make-css-strings in this case because the prefixes appear in the value,
  ; not in the property (which is always "background")
  (define gradient-strings (map (λ(prefix) (format "background: ~a~a-gradient(~a, ~a)" prefix gradient-type gradient-direction color-stop-string)) css-property-prefixes))
  
  ; just fill with the last color if gradient not available
  (define fallback-string (format "background: ~a" (last colors)))
  
  ; put fallback string at front of list
  (join-css-strings (cons fallback-string gradient-strings)))
