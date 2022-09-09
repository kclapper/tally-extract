;; Code based heavily on https://docs.racket-lang.org/lex-yacc-example/index.html

#lang racket

(require "parser-definitions/main.rkt"
         "grammar-rules/main.rkt")

(define tally-parser
  (make-parser input-file
               tally
               filler
               particle-type
               units
               data
               type-6
               cell-6
               type-4
               volume-section
               segment-volumes
               segment-volume
               cell-seg-values
               energy-sections
               energy-section
               surface-list
               energy-line))

(define (parse-tallys input-port)
  (port-count-lines! input-port)
  (tally-parser (lambda () (next-token input-port))))

(provide parse-tallys)

(module+ test
  (require rackunit)

  (require "../../tests/test-data.rkt"
           "../../tests/test-utils.rkt")

  (define (remove-keys hash-tables . keys)
    (for/list ([hash-table hash-tables])
      (foldl (lambda (key ht) (hash-remove ht key))
           hash-table
           keys)))

  (define neutron-file-path "../../tests/data/public/fake-out.txt")
  (define modified-neutron-file-expected (remove-keys neutron-file-expected
                                                      `FMF
                                                      `keff))
  (define big-neutron-file-path "../../tests/data/private/outp0")
  (define modified-big-neutron-file-expected (remove-keys big-neutron-file-expected
                                                          `FMF
                                                          `keff))
  (define big-heating-file-path "../../tests/data/private/outp_heating")
  (define modified-big-heating-file-expected (remove-keys big-heating-file-expected
                                                          `FMF
                                                          `keff))
  (define big-segment-file-path "../../tests/data/private/outp_FCn_En_SDn_FSn.txt")
  (define modified-big-segment-file-expected (remove-keys big-segment-file-expected
                                                          `FMF
                                                          `keff))

  (check-equal? (call-with-input-file neutron-file-path parse-tallys)
                modified-neutron-file-expected)

  (if-available big-neutron-file-path
                (check-equal? (call-with-input-file big-neutron-file-path parse-tallys)
                              modified-big-neutron-file-expected))

  (if-available big-heating-file-path
                (check-equal? (call-with-input-file big-heating-file-path parse-tallys)
                              modified-big-heating-file-expected))

  (if-available big-segment-file-path
                (check-equal? (call-with-input-file big-segment-file-path parse-tallys)
                              modified-big-segment-file-expected))
  )
