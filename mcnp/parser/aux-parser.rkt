;; Code based heavily on https://docs.racket-lang.org/lex-yacc-example/index.html

#lang racket

(require racket/hash
         parser-tools/yacc)

(require "./parser-definitions/main.rkt")

(define aux-parser
  (parser
   (start input-file)
   (end EOF)
   (tokens value-tokens marker-tokens aux-marker-tokens)
   (src-pos)
   (error (lambda (a b c d e)
            (begin (printf
                    "a = ~a\nb = ~a\nc = ~a\nd = ~a\ne = ~a\n"
                    a b c d e)
                   (void))))
   (suppress)
   (grammar
    (input-file ((keff) (hash `keff $1))
                ((wloss) (hash `wloss $1))
                ((filler) (hash))
                ((input-file input-file) (hash-union $1 $2)))
    (keff ((KEFF-M NUMBER) $2))
    (wloss ((WLOSS-M-1
             NUMBER
             NUMBER
             NUMBER
             WLOSS-M-2
             NUMBER
             NUMBER
             NUMBER) $7))
    (filler ((WORD) void)
            ((NUMBER) void)
            ((EOT) void)
            ((SOT) void)
            ((NEWLINE) void)
            ((CELL-M) void)
            ((CELL-COL-M) void)
            ((ENERGY-M) void)
            ((PARTICLE-M) void)
            ((SEGMENT-M) void)
            ((NPS-M) void)
            ((WLOSS-M-2) void)
            ((SEGMENT-COL-M) void)
            ((LAST-SEG-M) void)
            ((VOLUMES-M) void)
            ((UNITS-M) void)))))

(define (parse-aux input-port)
  (port-count-lines! input-port)
  (aux-parser (lambda () (next-token input-port))))

(provide parse-aux)

(module+ test
  (require rackunit)

  (require "../../tests/test-utils.rkt"
           "../../tests/test-data.rkt")

  (define neutron-file-path "../../tests/data/public/fake-out.txt")
  (define big-neutron-file-path "../../tests/data/private/outp0")
  (define big-heating-file-path "../../tests/data/private/outp_heating")
  (define big-segment-file-path "../../tests/data/private/outp_FCn_En_SDn_FSn.txt")

  (check-equal? (call-with-input-file neutron-file-path parse-aux)
                '#hash((keff . 1.00333) (wloss . 0.41108)))

  (if-available big-neutron-file-path
                (check-equal? (call-with-input-file big-neutron-file-path parse-aux)
                              '#hash((keff . 1.00333) (wloss . 0.41108))
                              "Aux parser get's keff and wloss from large neutron file."))

  (if-available big-heating-file-path
                (check-equal? (call-with-input-file big-heating-file-path parse-aux)
                              '#hash((keff . 1.0034) (wloss . 0.4111))
                              "Aux parser get's keff and wloss from large heating file."))
  (if-available big-segment-file-path
                (check-equal? (call-with-input-file big-segment-file-path parse-aux)
                              '#hash((keff . 1.00334) (wloss . 0.41108))
                              "Aux parser get's keff and wloss from large segment file.")))
