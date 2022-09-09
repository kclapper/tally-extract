#lang racket

(require "../../parser-definitions/main.rkt"
         "type-6.rkt"
         "type-4.rkt")

;; Grammar for the various data sections in a tally
;; and how to format them when put into a list
(define-grammar-rule
  (data ((type-6) (hash `formatter type-6-format
                        `data-values $1))
        ((type-4) (hash `formatter type-4-format
                        `data-values $1))))


(provide (all-defined-out)
         (all-from-out "type-6.rkt")
         (all-from-out "type-4.rkt"))
