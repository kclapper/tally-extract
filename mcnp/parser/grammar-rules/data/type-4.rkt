#lang racket

(require "../../parser-definitions/main.rkt")

;; Final output hash table schema:
;;     `(#hash((cell# . )
;;            (segment-volumes . )
;;            (energy-sections . (#hash((cell# . )
;;                                     (facing-surface . ())
;;                                     (energies . (#hash ((energy . )
;;                                                         (value . )
;;                                                         (error . )))))))))

(define-grammar-rule
  (type-4 ((volume-section NEWLINE energy-sections) (add-energy-to-volume-sections $3 $1))))

(define (add-energy-to-volume-sections energy-sections volume-section)
  (foldl add-energy-section volume-section energy-sections))

(define (add-energy-section energy-section volume-section)
  (for/list ([volume volume-section])
    (if (hash-key-equal? `cell# energy-section volume)
        (append-to-hash-list volume `energy-sections energy-section)
        volume)))

(define (hash-key-equal? key hash-1 hash-2)
  (equal? (hash-ref hash-1 key)
          (hash-ref hash-2 key)))

(define-grammar-rule
  (volume-section ((VOLUMES-M NEWLINE CELL-COL-M) empty)
                  ((volume-section NUMBER) (append $1
                                                   (list (hash `cell# $2
                                                               `segment-volumes empty
                                                               `energy-sections empty))))
                  ((volume-section NEWLINE segment-volumes) (add-segments-to-volumes $3 $1))))

(define (add-segments-to-volumes segments volumes)
  (foldl add-segment-to-volumes volumes segments))

(define (add-segment-to-volumes segment volumes)
  (for/list ([volume volumes]
             [i (in-range (length volumes))])
    (append-to-hash-list volume `segment-volumes
                         (cons (first segment)
                               (list-ref (rest segment) i)))))

;; Hash, Key . Lists -> Hash
;; Appends to a list under a key in a hash table, returning a
;; new version of the hash table.
(define (append-to-hash-list ht key . lsts)
  (hash-set ht key (append (hash-ref ht key)
                           lsts)))

(define-grammar-rule
  (segment-volumes ((SEGMENT-M NEWLINE segment-volume) (list $3))
                   ((segment-volumes segment-volume) (append $1
                                              (list $2)))))

(define-grammar-rule
  (segment-volume ((NUMBER cell-seg-values) (append (list $1) $2))
             ((LAST-SEG-M cell-seg-values) (append (list "whole") $2))))

(define-grammar-rule
  (cell-seg-values ((NUMBER NEWLINE) (list $1))
                   ((NUMBER cell-seg-values) (append (list $1) $2))))

(define-grammar-rule
  (energy-sections ((energy-section) (list $1))
                   ((energy-sections NEWLINE NEWLINE energy-section) (append $1
                                                                             (list $4)))
                   ((energy-sections NEWLINE NEWLINE NEWLINE) $1)))

(define-grammar-rule
  (energy-section ((CELL-M NUMBER NEWLINE SEGMENT-COL-M surface-list NEWLINE ENERGY-M NEWLINE) (hash `cell# $2
                                                                                                    `facing-surface $5
                                                                                                    `energies empty))
                  ((energy-section energy-line) (append-to-hash-list $1 `energies
                                                                     $2))))

(define-grammar-rule
  (surface-list ((NUMBER) (list $1))
                ((LAST-SEG-M CELL-M) (list "whole cell"))
                ((surface-list NUMBER) (append $1
                                               (list $2)))))

(define-grammar-rule
  (energy-line ((NUMBER NUMBER NUMBER NEWLINE) (hash `energy $1
                                             `value $2
                                             `error $3))
               ((WORD NUMBER NUMBER) (hash `energy "total"
                                           `value $2
                                           `error $3))))


;; Hash -> List
;; Formats a hash table representing the data values collected in
;; a type 4 tally as a list.
(define (type-4-format data-values)
  (pad-grid `(("volumes")
              ("cell:" ,@(map (lambda (volume) (hash-ref volume `cell#)) data-values))
              ,@(pad-grid `(("segment")
                            ,@(segment-volume-format data-values)
                            ("")
                            ,@(energy-section-format data-values))))))

(define (pad-grid list-grid)
  (define max-length (apply max (map length list-grid)))
  (for/list ([row list-grid])
    (append row
            (for/list ([i (in-range (- max-length (length row)))])
              ""))))

(define (grid-append grid-base grid-addon)
  (for/list ([base grid-base]
             [addon grid-addon])
    `(,@base "          " ,@addon)))

(define (segment-volume-format data-values)
  (foldl (lambda (volume segment-grid)
           (for/list ([segment segment-grid]
                      [value (map cdr (hash-ref volume `segment-volumes))])
             (append segment
                     (list value))))
         (for/list ([segment (hash-ref (first data-values)
                                       `segment-volumes)])
           (list (car segment)))
         data-values))

(define (energy-section-format data-values)
  (for/fold ([grid empty])
            ([volume data-values])
    (append grid
            (let* ([sections (hash-ref volume `energy-sections)])
              (for/fold ([grid (single-energy-section-format (first sections))])
                        ([section (rest sections)])
                (grid-append grid
                             (single-energy-section-format section)))))))

(define (single-energy-section-format section)
  (pad-grid `(("cell" ,(hash-ref section `cell#))
              ("segment:" ,@(hash-ref section `facing-surface))
              ("energy")
              ,@(for/list ([energy (hash-ref section `energies)])
                  (list (hash-ref energy `energy)
                        (hash-ref energy `value)
                        (hash-ref energy `error)))
              (""))))

(provide (all-defined-out))

(module+ test
  (require rackunit)
  (require "../../../../tests/test-utils.rkt")

  (define test-volumes "           volumes
                   cell:     16548        16549        16550        16551
               segment
                 1       8.09738E-01  8.09738E-01  8.09738E-01  8.09738E-01
                 2       4.81056E-01  4.81056E-01  4.81056E-01  4.81056E-01
                 3       4.81056E-01  4.81056E-01  4.81056E-01  4.81056E-01
                 4       4.81056E-01  4.81056E-01  4.81056E-01  4.81056E-01
                 5       4.81056E-01  4.81056E-01  4.81056E-01  4.81056E-01
                 6       4.81056E-01  4.81056E-01  4.81056E-01  4.81056E-01
                 7       4.81056E-01  4.81056E-01  4.81056E-01  4.81056E-01
                 8       4.81056E-01  4.81056E-01  4.81056E-01  4.81056E-01
                 9       4.81056E-01  4.81056E-01  4.81056E-01  4.81056E-01
                10       1.52976E-01  1.52976E-01  1.52976E-01  1.52976E-01
              whole      4.81117E+00  4.81117E+00  4.81117E+00  4.81117E+00
 ")

  (define expected-volumes '(#hash((cell# . 16548)
                                   (energy-sections . ())
                                   (segment-volumes . ((1 . 0.809738)
                                                       (2 . 0.481056)
                                                       (3 . 0.481056)
                                                       (4 . 0.481056)
                                                       (5 . 0.481056)
                                                       (6 . 0.481056)
                                                       (7 . 0.481056)
                                                       (8 . 0.481056)
                                                       (9 . 0.481056)
                                                       (10 . 0.152976)
                                                       ("whole" . 4.81117))))
                             #hash((cell# . 16549)
                                   (energy-sections . ())
                                   (segment-volumes . ((1 . 0.809738)
                                                       (2 . 0.481056)
                                                       (3 . 0.481056)
                                                       (4 . 0.481056)
                                                       (5 . 0.481056)
                                                       (6 . 0.481056)
                                                       (7 . 0.481056)
                                                       (8 . 0.481056)
                                                       (9 . 0.481056)
                                                       (10 . 0.152976)
                                                       ("whole" . 4.81117))))
                             #hash((cell# . 16550)
                                   (energy-sections . ())
                                   (segment-volumes . ((1 . 0.809738)
                                                       (2 . 0.481056)
                                                       (3 . 0.481056)
                                                       (4 . 0.481056)
                                                       (5 . 0.481056)
                                                       (6 . 0.481056)
                                                       (7 . 0.481056)
                                                       (8 . 0.481056)
                                                       (9 . 0.481056)
                                                       (10 . 0.152976)
                                                       ("whole" . 4.81117))))
                             #hash((cell# . 16551)
                                   (energy-sections . ())
                                   (segment-volumes . ((1 . 0.809738)
                                                       (2 . 0.481056)
                                                       (3 . 0.481056)
                                                       (4 . 0.481056)
                                                       (5 . 0.481056)
                                                       (6 . 0.481056)
                                                       (7 . 0.481056)
                                                       (8 . 0.481056)
                                                       (9 . 0.481056)
                                                       (10 . 0.152976)
                                                       ("whole" . 4.81117))))))

  (define volume-parser (make-string-parser (make-parser volume-section
                                                         segment-volumes
                                                         segment-volume
                                                         cell-seg-values)))

  (check-equal? (volume-parser test-volumes)
                expected-volumes
                "volume rules able to parse volume section.")

  (define test-segments "segment
                 1       8.09738E-01  8.09738E-01  8.09738E-01  8.09738E-01
                 2       4.81056E-01  4.81056E-01  4.81056E-01  4.81056E-01
                 3       4.81056E-01  4.81056E-01  4.81056E-01  4.81056E-01
                 4       4.81056E-01  4.81056E-01  4.81056E-01  4.81056E-01
                 5       4.81056E-01  4.81056E-01  4.81056E-01  4.81056E-01
                 6       4.81056E-01  4.81056E-01  4.81056E-01  4.81056E-01
                 7       4.81056E-01  4.81056E-01  4.81056E-01  4.81056E-01
                 8       4.81056E-01  4.81056E-01  4.81056E-01  4.81056E-01
                 9       4.81056E-01  4.81056E-01  4.81056E-01  4.81056E-01
                10       1.52976E-01  1.52976E-01  1.52976E-01  1.52976E-01
              whole      4.81117E+00  4.81117E+00  4.81117E+00  4.81117E+00
 ")
  (define expected-segments '((1 0.809738 0.809738 0.809738 0.809738)
                              (2 0.481056 0.481056 0.481056 0.481056)
                              (3 0.481056 0.481056 0.481056 0.481056)
                              (4 0.481056 0.481056 0.481056 0.481056)
                              (5 0.481056 0.481056 0.481056 0.481056)
                              (6 0.481056 0.481056 0.481056 0.481056)
                              (7 0.481056 0.481056 0.481056 0.481056)
                              (8 0.481056 0.481056 0.481056 0.481056)
                              (9 0.481056 0.481056 0.481056 0.481056)
                              (10 0.152976 0.152976 0.152976 0.152976)
                              ("whole" 4.81117 4.81117 4.81117 4.81117)))

  (define segment-parser (make-string-parser (make-parser segment-volumes
                                                          segment-volume
                                                          cell-seg-values)))

  (check-equal? (segment-parser test-segments)
                expected-segments
                "segment rules able to parse segment section.")

  (define test-energy-section " cell  16548
 segment:         16609
      energy
    1.0000E-03   0.00000E+00 0.0000
    2.0000E-03   1.09121E-10 0.3454
    3.0000E-03   9.89628E-11 0.5220
    4.0000E-03   3.17257E-10 0.4587
    5.0000E-03   3.70969E-10 0.8659
    6.0000E-03   1.46257E-09 0.4166
    7.0000E-03   1.43750E-09 0.4155
    8.0000E-03   6.89455E-10 0.8387
    9.0000E-03   4.43194E-10 0.7882
    1.0000E-02   1.27466E-09 0.5756
    1.1000E-02   3.49482E-09 0.5584
    1.2000E-02   4.92939E-09 0.5076
    1.3000E-02   5.51771E-09 0.4300
    1.4000E-02   9.81851E-09 0.4198
    1.5000E-02   2.93863E-09 0.7799
    1.6000E-02   8.48514E-09 0.4134
    1.7000E-02   7.18644E-09 0.5981
    1.8000E-02   1.69745E-08 0.5042
    1.9000E-02   8.44982E-09 0.4138
    2.0000E-02   1.45100E-08 0.4138
    2.1000E-02   1.05213E-08 0.3741
    2.2000E-02   1.09335E-08 0.3959
    2.3000E-02   4.40955E-09 0.5444
    2.4000E-02   1.06757E-08 0.4700
       total      3.43933E-04 0.0027")

  (define expected-energy-section '#hash((cell# . 16548)
                                         (energies . (#hash((energy . 0.001) (error . 0.0) (value . 0.0))
                                                      #hash((energy . 0.002) (error . 0.3454) (value . 1.09121e-10))
                                                      #hash((energy . 0.003) (error . 0.522) (value . 9.89628e-11))
                                                      #hash((energy . 0.004) (error . 0.4587) (value . 3.17257e-10))
                                                      #hash((energy . 0.005) (error . 0.8659) (value . 3.70969e-10))
                                                      #hash((energy . 0.006) (error . 0.4166) (value . 1.46257e-9))
                                                      #hash((energy . 0.007) (error . 0.4155) (value . 1.4375e-9))
                                                      #hash((energy . 0.008) (error . 0.8387) (value . 6.89455e-10))
                                                      #hash((energy . 0.009) (error . 0.7882) (value . 4.43194e-10))
                                                      #hash((energy . 0.01) (error . 0.5756) (value . 1.27466e-9))
                                                      #hash((energy . 0.011) (error . 0.5584) (value . 3.49482e-9))
                                                      #hash((energy . 0.012) (error . 0.5076) (value . 4.92939e-9))
                                                      #hash((energy . 0.013) (error . 0.43) (value . 5.51771e-9))
                                                      #hash((energy . 0.014) (error . 0.4198) (value . 9.81851e-9))
                                                      #hash((energy . 0.015) (error . 0.7799) (value . 2.93863e-9))
                                                      #hash((energy . 0.016) (error . 0.4134) (value . 8.48514e-9))
                                                      #hash((energy . 0.017) (error . 0.5981) (value . 7.18644e-9))
                                                      #hash((energy . 0.018) (error . 0.5042) (value . 1.69745e-8))
                                                      #hash((energy . 0.019) (error . 0.4138) (value . 8.44982e-9))
                                                      #hash((energy . 0.02) (error . 0.4138) (value . 1.451e-8))
                                                      #hash((energy . 0.021) (error . 0.3741) (value . 1.05213e-8))
                                                      #hash((energy . 0.022) (error . 0.3959) (value . 1.09335e-8))
                                                      #hash((energy . 0.023) (error . 0.5444) (value . 4.40955e-9))
                                                      #hash((energy . 0.024) (error . 0.47) (value . 1.06757e-8))
                                                      #hash((energy . "total") (error . 0.0027) (value . 0.000343933))))
                                         (facing-surface . (16609))))

  (define test-whole-cell-section " cell  16548
 segment:   whole cell
      energy
    1.0000E-03   0.00000E+00 0.0000
    2.0000E-03   1.49108E-10 0.1142
       total      3.43933E-04 0.0027")

  (define expected-whole-cell-section '#hash((cell# . 16548)
                                             (energies . (#hash((energy . 0.001)
                                                                (error . 0.0)
                                                                (value . 0.0))
                                                          #hash((energy . 0.002)
                                                                (error . 0.1142)
                                                                (value . 1.49108e-10))
                                                          #hash((energy . "total")
                                                                (error . 0.0027)
                                                                (value . 0.000343933))))
                                             (facing-surface . ("whole cell"))))

  (define energy-section-parser (make-string-parser (make-parser energy-section
                                                                 surface-list
                                                                 energy-line)))

  (check-equal? (energy-section-parser test-energy-section)
                expected-energy-section
                "energy section rules correctly parse energy section.")

  (check-equal? (energy-section-parser test-whole-cell-section)
                expected-whole-cell-section
                "energy section rules handle whole cell energy section.")

  (define test-multi-energy-sections (string-append test-energy-section
                                                    "\n"
                                                    "\n"
                                                    test-energy-section))

  (define multi-energy-section-parser (make-string-parser (make-parser energy-sections
                                                                       energy-section
                                                                       surface-list
                                                                       energy-line)))

  (check-equal? (multi-energy-section-parser test-multi-energy-sections)
                (list expected-energy-section
                      expected-energy-section)
                "energy section rules correctly parse multiple energy sections.")

  (define test-type-4 (string-append test-volumes
                                     "\n"
                                     test-energy-section))
  (define type-4-parser (make-string-parser (make-parser type-4
                                                         volume-section
                                                         segment-volumes
                                                         segment-volume
                                                         cell-seg-values
                                                         energy-sections
                                                         energy-section
                                                         surface-list
                                                         energy-line)))

  (check-equal? (type-4-parser test-type-4)
                '(#hash((cell# . 16548)
                        (energy-sections . (#hash((cell# . 16548)
                                                   (energies . (#hash((energy . 0.001)
                                                                      (error . 0.0)
                                                                      (value . 0.0))
                                                                #hash((energy . 0.002)
                                                                      (error . 0.3454)
                                                                      (value . 1.09121e-10))
                                                                #hash((energy . 0.003)
                                                                      (error . 0.522)
                                                                      (value . 9.89628e-11))
                                                                #hash((energy . 0.004)
                                                                      (error . 0.4587)
                                                                      (value . 3.17257e-10))
                                                                #hash((energy . 0.005)
                                                                      (error . 0.8659)
                                                                      (value . 3.70969e-10))
                                                                #hash((energy . 0.006)
                                                                      (error . 0.4166)
                                                                      (value . 1.46257e-9))
                                                                #hash((energy . 0.007)
                                                                      (error . 0.4155)
                                                                      (value . 1.4375e-9))
                                                                #hash((energy . 0.008)
                                                                      (error . 0.8387)
                                                                      (value . 6.89455e-10))
                                                                #hash((energy . 0.009)
                                                                      (error . 0.7882)
                                                                      (value . 4.43194e-10))
                                                                #hash((energy . 0.01)
                                                                      (error . 0.5756)
                                                                      (value . 1.27466e-9))
                                                                #hash((energy . 0.011)
                                                                      (error . 0.5584)
                                                                      (value . 3.49482e-9))
                                                                #hash((energy . 0.012)
                                                                      (error . 0.5076)
                                                                      (value . 4.92939e-9))
                                                                #hash((energy . 0.013)
                                                                      (error . 0.43)
                                                                      (value . 5.51771e-9))
                                                                #hash((energy . 0.014)
                                                                      (error . 0.4198)
                                                                      (value . 9.81851e-9))
                                                                #hash((energy . 0.015)
                                                                      (error . 0.7799)
                                                                      (value . 2.93863e-9))
                                                                #hash((energy . 0.016)
                                                                      (error . 0.4134)
                                                                      (value . 8.48514e-9))
                                                                #hash((energy . 0.017)
                                                                      (error . 0.5981)
                                                                      (value . 7.18644e-9))
                                                                #hash((energy . 0.018)
                                                                      (error . 0.5042)
                                                                      (value . 1.69745e-8))
                                                                #hash((energy . 0.019)
                                                                      (error . 0.4138)
                                                                      (value . 8.44982e-9))
                                                                #hash((energy . 0.02)
                                                                      (error . 0.4138)
                                                                      (value . 1.451e-8))
                                                                #hash((energy . 0.021)
                                                                      (error . 0.3741)
                                                                      (value . 1.05213e-8))
                                                                #hash((energy . 0.022)
                                                                      (error . 0.3959)
                                                                      (value . 1.09335e-8))
                                                                #hash((energy . 0.023)
                                                                      (error . 0.5444)
                                                                      (value . 4.40955e-9))
                                                                #hash((energy . 0.024)
                                                                      (error . 0.47)
                                                                      (value . 1.06757e-8))
                                                                #hash((energy . "total")
                                                                      (error . 0.0027)
                                                                      (value . 0.000343933))))
                                                   (facing-surface . (16609)))))
                        (segment-volumes . ((1 . 0.809738)
                                            (2 . 0.481056)
                                            (3 . 0.481056)
                                            (4 . 0.481056)
                                            (5 . 0.481056)
                                            (6 . 0.481056)
                                            (7 . 0.481056)
                                            (8 . 0.481056)
                                            (9 . 0.481056)
                                            (10 . 0.152976)
                                            ("whole" . 4.81117))))
                  #hash((cell# . 16549)
                        (energy-sections . ())
                        (segment-volumes . ((1 . 0.809738)
                                            (2 . 0.481056)
                                            (3 . 0.481056)
                                            (4 . 0.481056)
                                            (5 . 0.481056)
                                            (6 . 0.481056)
                                            (7 . 0.481056)
                                            (8 . 0.481056)
                                            (9 . 0.481056)
                                            (10 . 0.152976)
                                            ("whole" . 4.81117))))
                  #hash((cell# . 16550)
                        (energy-sections . ())
                        (segment-volumes . ((1 . 0.809738)
                                            (2 . 0.481056)
                                            (3 . 0.481056)
                                            (4 . 0.481056)
                                            (5 . 0.481056)
                                            (6 . 0.481056)
                                            (7 . 0.481056)
                                            (8 . 0.481056)
                                            (9 . 0.481056)
                                            (10 . 0.152976)
                                            ("whole" . 4.81117))))
                  #hash((cell# . 16551)
                        (energy-sections . ())
                        (segment-volumes . ((1 . 0.809738)
                                            (2 . 0.481056)
                                            (3 . 0.481056)
                                            (4 . 0.481056)
                                            (5 . 0.481056)
                                            (6 . 0.481056)
                                            (7 . 0.481056)
                                            (8 . 0.481056)
                                            (9 . 0.481056)
                                            (10 . 0.152976)
                                            ("whole" . 4.81117)))))
                "type-4 parses correctly.")

  (define test-type-4-for-format "           volumes
                   cell:     16548        16549        16550        16551
               segment
                 1       8.09738E-01  8.09738E-01  8.09738E-01  8.09738E-01
              whole      4.81117E+00  4.81117E+00  4.81117E+00  4.81117E+00

  cell  16548
 segment:         16609
      energy
    1.0000E-03   0.00000E+00 0.0000
    2.0000E-03   1.09121E-10 0.3454
       total      3.43933E-04 0.0027")

  (define expected-type-4-format '(("volumes" "" "" "" "")
                                   ("cell:" 16548 16549 16550 16551)
                                   ("segment" "" "" "" "")
                                   (1 0.809738 0.809738 0.809738 0.809738)
                                   ("whole" 4.81117 4.81117 4.81117 4.81117)
                                   ("" "" "" "" "")
                                   ("cell" 16548 "" "" "")
                                   ("segment:" 16609 "" "" "")
                                   ("energy" "" "" "" "")
                                   (0.001 0.0 0.0 "" "")
                                   (0.002 1.09121e-10 0.3454 "" "")
                                   ("total" 0.000343933 0.0027 "" "")
                                   ("" "" "" "" "")))

  (check-equal? (type-4-format (type-4-parser test-type-4-for-format))
                expected-type-4-format
                "type-4 formats correctly."))
