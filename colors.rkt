#lang racket/base
(require racket/contract racket/match racket/string)

(require "named-colors.rkt")

(module+ test (require rackunit))


;; Conversion functions between RGB and other color systems.
;; Adaptation of colorsys module in the Python library
;; Original source:
;; http://hg.python.org/cpython/file/2.7/Lib/colorsys.py

;; References:
;; http://en.wikipedia.org/wiki/HLS_color_space


(provide rgb->hsl hsl->rgb 
         rgb->hex hex->rgb 
         hsl->hex hex->hsl)


;; this makes things work on exact numbers by default
(define (string->exact str)
  (string->number str 10 'number-or-false 'decimal-as-exact))

(define unitval? (real-in 0 1))

(define/contract (real->unitval x)
  (real? . -> . unitval?)
  (min (max 0 x) 1))

(module+ test
  (check-equal? (real->unitval -10) 0)
  (check-equal? (real->unitval 10) 1)
  (check-equal? (real->unitval 0.5) 0.5))


(define DEGREE_MAX 360)
(define degree? (real-in 0 DEGREE_MAX))

(module+ test
  (check-false (degree? -1))
  (check-true (degree? 0))
  (check-true (degree? 149.23))
  (check-true (degree? 255))
  (check-false (degree? 361)))


;; like (modulo x 1) but works with floating-point numbers
(define (modulo1 x)
  (- x (floor x)))


(define/contract (unitval->byte u)
  (unitval? . -> . byte?)
  (inexact->exact (floor (* u 255))))

(module+ test 
  (check-equal? (unitval->byte 0) 0)
  (check-equal? (unitval->byte 1) 255))


(define/contract (byte->unitval b)
  (byte? . -> . unitval?)
  (/ b 255))

(module+ test 
  (check-equal? (byte->unitval 0) 0)
  (check-equal? (byte->unitval 255) 1))


(define (degree-string? x)
  (and (string? x) (degree? (string->exact x))))

(define/contract (unitval->degree u)
  (unitval? . -> . degree-string?)
  (format "~a" (exact->inexact (/ (floor (* u DEGREE_MAX 100)) 100))))

(module+ test 
  (check-equal? (unitval->degree 0) "0.0")
  (check-equal? (unitval->degree 0.5) "180.0")
  (check-equal? (unitval->degree 1) "360.0"))

(define (unitval->percentage u)
  (format "~a%" (exact->inexact (/ (floor (* u 100 100)) 100))))


(module+ test
  (check-true (degree-string? "0"))
  (check-true (degree-string? "180"))
  (check-true (degree-string? "360"))
  (check-false (degree-string? "450"))
  (check-false (degree-string? 450))
  (check-false (degree-string? "foo")))


(define/contract (degree-string->unitval d)
  (degree-string? . -> . unitval?)
  (/ (string->exact d) DEGREE_MAX))

(module+ test 
  (check-equal? (degree-string->unitval "0") 0)
  (check-equal? (degree-string->unitval "180") (/ 1 2))
  (check-equal? (degree-string->unitval "360") 1))


(define/contract (trim-unitval-sign x)
  (string? . -> . string?)
  (string-trim x "%" #:left? #f))

(module+ test
  (check-equal? (trim-unitval-sign "50") "50")  
  (check-equal? (trim-unitval-sign "50%") "50")  
  (check-equal? (trim-unitval-sign "%50%") "%50"))

(define/contract (unitval-string? x)
  (any/c . -> . boolean?)
  (and (string? x)
       (equal? #\% (car (reverse (string->list x))))
       ((real-in 0 100) (string->exact (trim-unitval-sign x)))))

(module+ test
  (check-true (unitval-string? "56%"))
  (check-true (unitval-string? "0.00001%"))
  (check-false (unitval-string? 50))
  (check-false (unitval-string? "50"))
  (check-false (unitval-string? "-12%"))
  (check-false (unitval-string? "200%")))


(define/contract (unitval-string->unitval x)
  (unitval-string? . -> . unitval?)
  (/ (string->exact (trim-unitval-sign x)) 100))

(module+ test
  (check-equal? (unitval-string->unitval "50%") (/ 1 2))  
  (check-equal? (unitval-string->unitval "100%") 1)  
  (check-equal? (unitval-string->unitval "0%") 0))


(define (trim-pound-sign x)
  (string-trim x "#" #:right? #f))

(define (make-hex-number x)
  (string->exact (string-append "#x" (trim-pound-sign x))))


(define HEX_DIGITS (string->list "0123456789abcdef"))
(define (hex-digit? x)
  (member x HEX_DIGITS))

(define (base-hex? x)
  (and (string? x) 
       (equal? (substring x 0 1) "#") 
       (andmap hex-digit? (string->list (string-downcase (trim-pound-sign x))))))

(define (short-hex? x) ; like #ddd
  (and (= (string-length x) 4) (base-hex? x)))

(define (long-hex? x) ; like #e802cf
  (and (= (string-length x) 7) (base-hex? x)))

(define (hex? x)  
  (or (short-hex? x) (long-hex? x)))

(define rgb? (list/c unitval? unitval? unitval?))
(define hsl? rgb?)

(define (rgbish? x)
  (ormap (λ(proc) (proc x))
         (list 
          rgb?
          (list/c unitval-string? unitval-string? unitval-string?)
          (list/c byte? byte? byte?)
          hex?
          named-color?)))

(define/contract (rgbish->rgb x)
  (rgbish? . -> . rgb?)
  ;; must handle all possible branches of rgbish
  (cond
    [(rgb? x) x]
    [((list/c unitval-string? unitval-string? unitval-string?) x)
     (map unitval-string->unitval x)]
    [((list/c byte? byte? byte?) x) (map byte->unitval x)]
    [(hex? x) (hex->rgb x)]
    [(named-color? x) (rgbish->rgb (named-color->hex x))]
    [else #f]))


(define (hslish? x)
  (ormap (λ(proc) (proc x))
         (list
          hsl?
          (list/c degree-string? unitval-string? unitval-string?) ; aka css-hsl
          (list/c byte? byte? byte?))))

(define (hslish->hsl x)
  (hslish? . -> . hsl?)
  (cond
    [(hsl? x) x]
    [((list/c degree-string? unitval-string? unitval-string?) x) (cons (degree-string->unitval (car x)) (map unitval-string->unitval (cdr x)))]
    [((list/c byte? byte? byte?) x) (map byte->unitval x)]
    [else #f]))

(define (hsl->css hsl)
  (hslish? . -> . hsl?)
  (match-define (list h s lum) (hslish->hsl hsl))
  (list (unitval->degree h) (unitval->percentage s) (unitval->percentage lum)))

;; convert rgb values into hue value
(define/contract (rgb->h r g b)
  (unitval? unitval? unitval? . -> . unitval?)
  (define maxc (max r g b))
  (define minc (min r g b))
  (if (minc . = . maxc)
      0 ; color is gray. Return now & avoid division by zero below
      (let ()
        (define-values (rc gc bc) 
          (apply values (map (λ(x) (/ (- maxc x) (- maxc minc))) (list r g b))))
        (modulo1 (/ (cond
                      [(r . = . maxc) (- bc gc)]
                      [(g . = . maxc) (+ 2.0 (- rc bc))]
                      [else (+ 4.0 (- gc rc))]) 
                    6.0)))))


(define/contract (rgb->hsl rgb)
  (rgbish? . -> . hsl?)
  (match-define (list r g b) (rgbish->rgb rgb))
  (define maxc (max r g b))
  (define minc (min r g b))
  (define h (rgb->h r g b))
  (define lum (/ (+ maxc minc) 2))
  (define s (/ (- maxc minc) (if (lum . <= . 0.5)
                                 (+ maxc minc)
                                 (- 2.0 maxc minc))))
  (map real->unitval (list h s lum)))

(define/contract (hsl->rgb hsl)
  (hslish? . -> . rgb?)
  (define ONE_THIRD (/ 1 3))
  (define ONE_SIXTH (/ 1 6))
  (define TWO_THIRDS (/ 2 3))
  (match-define (list h s lum) (hslish->hsl hsl))
  
  (define (_v m1 m2 hue)
    (let ([hue (modulo1 hue)])
      (cond
        [(hue . < . ONE_SIXTH) (+ m1 (* (- m2 m1) hue 6.0))]
        [(hue . < . 0.5) m2]
        [(hue . < . TWO_THIRDS) (+ m1 (* (- m2 m1) (- TWO_THIRDS hue) 6.0))]
        [else m1])))
  
  (define m2 (if (lum . <= . 0.5)
                 (* lum (+ 1.0 s))
                 (- (+ lum s) (* lum s))))
  
  (define m1 (- (* 2.0 lum) m2))
  
  (map real->unitval (map (λ(x) (_v m1 m2 x)) 
                          (list (+ h ONE_THIRD) h (- h ONE_THIRD)))))



(define/contract (rgb->hex rgb)
  (rgbish? . -> . hex?)
  ;; make a 2-digit hex string from a number
  (define (hex-format num)
    (define hex (format "~x" num))
    (if (= (string-length hex) 1)
        (string-append "0" hex)
        hex))
  
  (define raw-hex (apply string-append (map hex-format (map unitval->byte (rgbish->rgb rgb)))))
  (define triple-double-pattern #px"^(\\w)\\1(\\w)\\2(\\w)\\3$")
  (define result (regexp-match triple-double-pattern raw-hex))
  (string-append "#" (if result
                         ; cdr result holds the three submatches
                         (apply string-append (cdr result))
                         raw-hex)))

(module+ test
  (check-equal? (rgb->hex '(1 1 1)) "#fff")
  (check-equal? (rgb->hex '(0.8 0.8 0.8)) "#ccc")
  (check-equal? (rgb->hex '(0.01 0.01 0.01)) "#020202"))

(define/contract (hex->long-hex hex)
  (hex? . -> . long-hex?)
  (if (short-hex? hex)
      (let ()
        (match-define (list d1 d2 d3) (cdr (regexp-match #px"^#(\\w)(\\w)(\\w)$" hex)))
        (string-append "#" d1 d1 d2 d2 d3 d3))
      hex))

(define/contract (hex->rgb hex)
  (hex? . -> . rgb?)
  (let ([hex (hex->long-hex hex)])
    (define result (regexp-match #px"^#(\\w\\w)(\\w\\w)(\\w\\w)$" hex))
    (map (compose1 byte->unitval make-hex-number) (cdr result))))

(define hsl->hex (compose1 rgb->hex hsl->rgb))
(define hex->hsl (compose1 rgb->hsl hex->rgb)) 