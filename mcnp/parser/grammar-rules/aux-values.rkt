#lang racket

(require "../parser-definitions/main.rkt")

;; Grammar rule to find the particle type within a tally.
(define-grammar-rule
  (particle-type ((PARTICLE-M WORD) $2)))

;; Grammar rule to get the units for a tally.
(define-grammar-rule
  (units ((UNITS-M WORD) $2)))

(provide (all-defined-out))
