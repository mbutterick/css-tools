#lang racket/base
(require "core.rkt")

(provide (all-defined-out))

(define (make-css-columns #:count count #:gap [gap #f])
  ; shorthand for css column declaration
  (join-css-strings (list*
                      (make-css-string "column-count" count)
                      (if gap
                          (list (make-css-string "column-gap" gap))
                          null))))

(define (make-css-avoid-column-break-inside)
  ; this gets applied to list items to keep them from breaking across columns
  ; however it doesn't work in Firefox due to bug; workaround is stupid
  (join-css-strings (list
                     (make-css-string "column-break-inside" "avoid")
                     (make-css-string "break-inside" "avoid-column"))))



