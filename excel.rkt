#lang racket

(require racket/format)

(require simple-xlsx)

(require "mcnp/main.rkt")

;; Writes data from a list of tallys into an xlsx object.
(define (add-tallys xlsx tally-list)
  (for ([item tally-list])
    (add-tally xlsx item))
  xlsx)

;; Writes data from a tally into a new sheet in an xlsx object.
(define (add-tally xlsx input-tally)
  (define sheet_name (~a "tally_" (hash-ref input-tally `number)))
  (send xlsx
        add-data-sheet
        #:sheet_name sheet_name
        #:sheet_data (tally->list input-tally))
  xlsx)

;; Takes a tally and returns a list representing cells on an Excel sheet.
(define (tally->list input-tally)
  (define (tally-ref key)
    (hash-ref input-tally key))
  (let* ([number (tally-ref `number)]
         [units (tally-ref `units)]
         [type (tally-ref `type)]
         [data (tally-ref `data)]
         [FMF (tally-ref `FMF)]
         [keff (tally-ref `keff)]
         [top-rows (list (list "tally" "units" "type" "FMF" "J/MeV" "keff")
                         (list number units type FMF J/MeV keff))])
    (append-symmetric top-rows
                      ((hash-ref data `formatter) (hash-ref data `data-values)))))

;; Combines the top rows and data rows such that the overall
;; 2-D list dimensions are symmetric.
(define (append-symmetric right-rows left-rows)
  (cond [(empty? right-rows) left-rows]
        [(empty? left-rows) right-rows]
        [else (append (pad-left right-rows
                                (+ 1 (length (first left-rows))))
                      (pad-right left-rows
                                 (+ 1 (length (first right-rows)))))]))

;; Pad's a list with empty strings at the beginning.
(define (pad-left rows pad#)
  (for/list ([row rows])
    (append (for/list ([i (in-range pad#)])
              "")
            row)))

;; Pad's a list with empty strings at the end.
(define (pad-right rows pad#)
  (for/list ([row rows])
    (append row
            (for/list ([i (in-range pad#)])
              ""))))

;; Writes data from a list of tallys into an Excel file at a given path.
(define (write-tallys tallys path)
  (write-xlsx-file (add-tallys (new xlsx%) tallys) path))

(provide write-tallys)

(module+ test
  (require rackunit)

  (require "tests/test-utils.rkt"
           "mcnp/parser/grammar-rules/main.rkt")

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Sample data
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define sample-output-xlsx "tests/data/public/fake-out.xlsx")

  (define single-sheet-sample-output-xlsx "tests/data/public/single sheet fake-out.xlsx")


  (define sample-tally `#hash((data . #hash((formatter . ,type-6-format)
                                            (data-values . (#hash((cell# . 16500) (err . 0.007) (value . 4.49953e-7))
                                                             #hash((cell# . 16501) (err . 0.0072) (value . 4.22015e-7))
                                                             #hash((cell# . 16502) (err . 0.0092) (value . 7.90535e-8))
                                                             #hash((cell# . 16503) (err . 0.008) (value . 2.0526e-7))
                                                             #hash((cell# . 16504) (err . 0.0073) (value . 1.44381e-7))
                                                             #hash((cell# . 16505) (err . 0.0048) (value . 3.72449e-7))
                                                             #hash((cell# . 16506) (err . 0.0093) (value . 7.71863e-8))))))
                              (number . 36)
                              (type . "neutrons")
                              (units . "mev/gram")
                              (keff . 1.00333)
                              (FMF . 428946239037291650.0)))
  (define sample-tally-2 `#hash((data . #hash((formatter . ,type-6-format)
                                              (data-values . (#hash((cell# . 16500) (err . 0.007) (value . 4.49953e-7))
                                                               #hash((cell# . 16501) (err . 0.0072) (value . 4.22015e-7))
                                                               #hash((cell# . 16502) (err . 0.0092) (value . 7.90535e-8))
                                                               #hash((cell# . 16503) (err . 0.008) (value . 2.0526e-7))
                                                               #hash((cell# . 16504) (err . 0.0073) (value . 1.44381e-7))
                                                               #hash((cell# . 16505) (err . 0.0048) (value . 3.72449e-7))
                                                               #hash((cell# . 16506) (err . 0.0093) (value . 7.71863e-8))))))
                                (number . 361)
                                (type . "neutrons")
                                (units . "mev/gram")
                                (keff . 1.00333)
                                (FMF . 428946239037291650.0)))
  (define sample-tallys (list sample-tally sample-tally-2))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Tests
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (check xlsx-list-equal?
         (with-readable-version (add-tallys (new xlsx%) sample-tallys)
                                xlsx->list)
         (xlsx-file->list sample-output-xlsx)
         "Write a list of tallys to an xlsx object.")

  (check xlsx-list-equal?
         (with-readable-version (add-tally (new xlsx%) sample-tally)
                                xlsx->list)
         (xlsx-file->list single-sheet-sample-output-xlsx)
         "Write a single tally to an xlsx object."))
