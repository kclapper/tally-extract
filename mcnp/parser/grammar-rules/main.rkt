#lang racket

(require "../parser-definitions/main.rkt"
         "aux-values.rkt"
         "data/main.rkt")

;; The grammar for reading an MCNP output file.
(define-grammar-rule
  (input-file ((tally) (list $1))
              ((filler) empty)
              ((input-file input-file) (append $1 $2))))

;; The grammar for any type of tally within a MCNP output file.
(define-grammar-rule
  (tally ((SOT NUMBER NPS-M NUMBER NEWLINE) (hash `number $2))
         ((tally EOT) $1)
         ((tally particle-type) (hash-set $1 `type $2))
         ((tally units) (hash-set $1 `units $2))
         ((tally data) (hash-set $1 `data $2))
         ((tally WORD) $1)
         ((tally CELL-COL-M) $1)
         ((tally NUMBER) $1)
         ((tally NEWLINE) $1)))

;; The grammar for filler words between tallys and within tallys.
(define-grammar-rule
  (filler ((WORD) void)
          ((NUMBER) void)
          ((NEWLINE) void)
          ((EOT) void)
          ((NEWLINE) void)
          ((CELL-M) void)
          ((CELL-COL-M) void)
          ((ENERGY-M) void)
          ((PARTICLE-M) void)
          ((NPS-M) void)
          ((SEGMENT-M) void)
          ((SEGMENT-COL-M) void)
          ((LAST-SEG-M) void)
          ((VOLUMES-M) void)
          ((UNITS-M) void)
          ((KEFF-M) void)
          ((WLOSS-M-1) void)
          ((WLOSS-M-2) void)
          ((SOT WORD) void)
          ((SOT NUMBER WORD) void)))

(provide (all-from-out "../parser-definitions/main.rkt")
         (all-from-out "aux-values.rkt")
         (all-from-out "data/main.rkt")
         (all-defined-out))
