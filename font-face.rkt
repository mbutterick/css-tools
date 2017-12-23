#lang racket/base
(require "core.rkt")
(require net/url-structs net/base64 racket/file racket/format racket/list sugar/unstable/string)
(provide (all-defined-out))

(module+ test (require rackunit))

(require racket/contract sugar)

(define/contract (base64-font-string? x)
  (any/c . -> . boolean?)
  ((->string x) . starts-with? . "data:"))

(module+ test
  (check-true (base64-font-string? "data:foobar"))
  (check-false (base64-font-string? "foobar")))


(define/contract (font-format p)
  (pathish? . -> . (or/c string? #f))
  (case (get-ext (->path p))
    [("eot") "embedded-opentype"]
    [("woff") "woff"]
    [("ttf" "otf") "truetype"] ; yep, in this CSS declaration, otf is considered 'truetype'
    [("svg") "svg"]
    [else #f]))

(module+ test
  (check-equal? (font-format "foo.eot") "embedded-opentype")
  (check-equal? (font-format "foo.woff") "woff")
  (check-equal? (font-format "foo.ttf") "truetype")
  (check-equal? (font-format "foo.otf") "truetype")
  (check-equal? (font-format "foo.svg") "svg")
  (check-false (font-format "foo")))


(define/contract (font-mime-type p)
  (pathish? . -> . (or/c string? #f))
  (case (get-ext (->path p))
    [("eot") "application/vnd.ms-fontobject"]
    [("woff") "application/font-woff"]
    [("ttf") "application/x-font-truetype"]
    [("otf") "application/x-font-opentype"]
    [("svg") "image/svg+xml"]
    [else #f]))

(module+ test
  (check-equal? (font-mime-type "foo.eot") "application/vnd.ms-fontobject")
  (check-equal? (font-mime-type (->url "foo.woff?bar=ino")) "application/font-woff")
  (check-equal? (font-mime-type "foo.ttf") "application/x-font-truetype")
  (check-equal? (font-mime-type "foo.otf") "application/x-font-opentype")
  (check-equal? (font-mime-type "foo.svg") "image/svg+xml")
  (check-false (font-mime-type "foo")))


(define/contract (path->base64-font-string p)
  (pathish? . -> . base64-font-string?)
  (define path (->path p))
  ;; for CSS, base64 encode needs to be done with no line separator
  (format "data:~a;charset=utf-8;base64,~a" (font-mime-type p) (base64-encode (file->bytes path) #"")))

(define (valid-font-style? x)
  (and (string? x) (member x '("normal" "italic" "oblique")) #t))

(module+ test
  (check-true (valid-font-style? "normal"))
  (check-true (valid-font-style? "oblique"))
  (check-false (valid-font-style? "foobar")))

(define (valid-font-weight? x)
  (and (string? x) (member x `("normal" "bold" ,@(map ~a (range 100 1000 100)))) #t))

(module+ test
  (check-true (valid-font-weight? "normal"))
  (check-true (valid-font-weight? "100"))
  (check-true (valid-font-weight? "300"))
  (check-true (valid-font-weight? "900"))
  (check-false (valid-font-weight? "italic"))
  (check-false (valid-font-weight? "1000")))

(define (valid-font-stretch? x)
  (and (string? x) (member x '("normal"
                               "ultra-condensed"
                               "extra-condensed"
                               "condensed"
                               "semi-condensed"
                               "semi-expanded"
                               "expanded"
                               "extra-expanded"
                               "ultra-expanded")) #t))

(module+ test
  (check-true (valid-font-stretch? "normal"))
  (check-true (valid-font-stretch? "extra-condensed"))
  (check-false (valid-font-stretch? "italic"))
  (check-false (valid-font-stretch? "nonsense")))

(define/contract (font-face-declaration font-family 
                                        src-url
                                        #:local [local-name #f]
                                        #:font-style [font-style "normal"] 
                                        #:font-weight [font-weight "normal"]
                                        #:font-stretch [font-stretch "normal"]
                                        #:unicode-range [unicodes #f]
                                        #:base64 [base64? #f])
  ((string? (or/c urlish? base64-font-string?)) 
   (#:font-style valid-font-style?
    #:font-weight valid-font-weight?
    #:font-stretch valid-font-stretch?
    #:unicode-range (or/c #f string?)
    #:base64 boolean?
    #:local (or/c #f string?))
   . ->* . string?)
  (let* ([url (->url src-url)]
         [url-value (if base64? (path->base64-font-string src-url) (->path url))]
         [src (format "url('~a') format('~a')" url-value (font-format src-url))]
         [src (string-append (if local-name (format "local(~v), " local-name) "") src)])
    (string-append "@font-face {\n" 
                   (join-css-strings (append
                                      (map make-css-string 
                                           '(font-family font-style font-weight font-stretch src)
                                           (list font-family font-style font-weight font-stretch src))
                                      (if unicodes
                                          (list (make-css-string 'unicode-range unicodes))
                                          null)))
                   "}")))

(define ffd font-face-declaration)

