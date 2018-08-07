#lang racket/base
(require "core.rkt")

(provide (all-defined-out))

(define (make-css-transition property duration #:timing-function [timing-function #f] #:delay [delay #f])
  (join-css-strings  (filter values (list
                      (make-css-string "transition-property" property)
                      (make-css-string "transition-duration" duration)
                      (and timing-function
                          (make-css-string "transition-timing-function" timing-function))
                      (and delay
                          (make-css-string "transition-delay" delay))))))
