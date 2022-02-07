#lang racket

(require racket/struct)

(require "parser/main.rkt"
         "parser/aux-parser.rkt")

;; J/MeV conversion factor
(define J/MeV 1.6021773e-13)

(define (file->tallys file-path)
  (define tallys (call-with-input-file file-path parse-tallys))
  (define aux-values (call-with-input-file file-path parse-aux))
  (define keff (hash-ref aux-values `keff))
  (define FMF (calculate-FMF keff (hash-ref aux-values `wloss)))
  (for/list ([tally tallys])
    (hash-set* tally
               `keff keff
               `FMF FMF)))

(define (calculate-FMF keff wloss)
  (if (equal? "N/A" keff)
      "N/A"
      (let ([p 5700000]
            [qavg 201.76]) ;#MeV/fsn
        (/ (/ (* p keff) wloss) (* qavg J/MeV keff)))))

(provide file->tallys
         J/MeV)

(module+ test
  (require rackunit)

  (require "../tests/test-utils.rkt"
           "../tests/test-data.rkt")

  (define neutron-file-path "../tests/data/public/fake-out.txt")
  (define big-neutron-file-path "../tests/data/private/outp0")
  (define big-heating-file-path "../tests/data/private/outp_heating")
  (define big-segment-file-path "../tests/data/private/outp_FCn_En_SDn_FSn.txt")
  (define big-238-file-path "../tests/data/private/outp238_3")

  (check-equal? (file->tallys neutron-file-path)
                neutron-file-expected
                "Fake output data parses correctly.")

  (if-available big-neutron-file-path
                (check-equal? (file->tallys big-neutron-file-path)
                              big-neutron-file-expected
                              "Large neutron file parses correctly."))

  (if-available big-heating-file-path
                (check-equal? (file->tallys big-heating-file-path)
                              big-heating-file-expected
                              "Large heating file parses correctly."))

  (if-available big-segment-file-path
                (check-equal? (file->tallys big-segment-file-path)
                              big-segment-file-expected
                              "Large segment file parses correctly."))

  (if-available big-238-file-path
                (check-equal? (file->tallys big-238-file-path)
                              big-238-file-expected
                              "Large 238 file parses correctly."))
  )
