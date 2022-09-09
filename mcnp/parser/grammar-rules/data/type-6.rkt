#lang racket

(require "../../parser-definitions/main.rkt")

;; A data section in a type-6 tally.
(define-grammar-rule
  (type-6 ((cell-6 cell-6) (list $1 $2))
          ((type-6 type-6) (append $1 $2))
          ((type-6 cell-6) (append $1 (list $2)))))

;; A cell within a type-6 data section.
(define-grammar-rule
  (cell-6 ((CELL-M NUMBER NEWLINE
            NUMBER NUMBER NEWLINE NEWLINE) (hash `cell# $2
                                                 `value $4
                                                 `err $5))))

;; How to format a type-6 data section when expressed
;; as a list.
(define (type-6-format data-values)
  (append (list (list "cell" "value" "error"))
          (for/list ([row data-values])
            (list (hash-ref row `cell#)
                  (hash-ref row `value)
                  (hash-ref row `err)))))

(provide (all-defined-out))


(module+ test
  (require rackunit)
  (require "../../../../tests/test-utils.rkt")

  (define cell-lines " cell  16500
                 4.49953E-07 0.0070

 cell  16501
                 4.22015E-07 0.0072

 cell  16502
                 7.90535E-08 0.0092

")

  (define cell-parser (make-string-parser (make-parser type-6
                                                       cell-6)))

  (check-equal? (cell-parser cell-lines)
                '(#hash((cell# . 16500) (err . 0.007) (value . 4.49953e-7))
                  #hash((cell# . 16501) (err . 0.0072) (value . 4.22015e-7))
                  #hash((cell# . 16502) (err . 0.0092) (value . 7.90535e-8)))
                "type-6 rules correctly parse cell lines."))
