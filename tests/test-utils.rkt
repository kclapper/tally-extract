#lang racket

(require simple-xlsx)

(require "../mcnp/parser/parser-definitions/main.rkt")

;; Let's you define an optional test that runs if a file is available.
(define-syntax-rule (if-available path test)
  (when (file-exists? path) test))

;; Takes a readable xlsx object and returns it's data in list form.
;; It's a multidimensional list, (sheet, row, column).
(define (xlsx->list xlsx)
  (for/list ([sheet (get-sheet-names xlsx)])
    (load-sheet sheet xlsx)
    (get-sheet-rows xlsx)))

;; From a path to an Excel file, return a list of all cells from the file.
(define (xlsx-file->list path)
  (with-input-from-xlsx-file path xlsx->list))

;; Turns a writable xlsx object into a readable one.
(define (with-readable-version xlsx fn)
  (define temp (make-temporary-file "rkttmp~a.xlsx"))
  (write-xlsx-file xlsx temp)
  (with-input-from-xlsx-file temp fn))

;; When an Excel file opens for the first time, it will round some of
;; the values entered by this program when creating the file. To make
;; make the tests useful, this function checks numerical values to be
;; within 0.01% of the same value.
(define (xlsx-list-equal? actual expected)
  (andmap (lambda (actual-sheet expected-sheet)
            (andmap (lambda (actual-row expected-row)
                      (andmap (lambda (actual-cell expected-cell)
                                (cond [(number? expected-cell)
                                       (< (/ (- actual-cell expected-cell)
                                             expected-cell)
                                          0.00001)]
                                      [else (equal? actual-cell expected-cell)]))
                              actual-row
                              expected-row))
                    actual-sheet
                    expected-sheet))
          actual
          expected))

  ;; Takes a parser and creates a string parsing function.
  (define (make-string-parser parser)
    (lambda (test-string)
      (call-with-input-string test-string
                              (lambda (input-port)
                                (port-count-lines! input-port)
                                (parser (lambda () (next-token input-port)))))))

(provide if-available
         xlsx-list-equal?
         with-readable-version
         xlsx->list
         xlsx-file->list
         make-string-parser)

(module+ test
  (require rackunit)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Sample data
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define small-sample-xlsx "./data/public/small.xlsx")
  (define small-sample-xlsx-as-list (list (list (list "This" "is" "a" "test")
                                                (list "" "" "" "")
                                                (list "" "" "" "")
                                                (list 22029 "" "" "")
                                                (list "" "" "" "")
                                                (list "" "" "" "")
                                                (list "" "" "" "")
                                                (list "" "" "" "")
                                                (list "" "" "" "")
                                                (list "" "" "" 83))
                                          (list (list "This" "is" "a" "test")
                                                (list "" "" "" "")
                                                (list "" "" "" "")
                                                (list 42 "" "" ""))))

  (define sample-xlsx-object-data (list (list "This" "is" "test" "data")
                                           (list "" "" "" "")
                                           (list 4329 4950 6083 2299)))
  (define sample-xlsx-object (let ([xlsx (new xlsx%)]
                                   [sheet_data sample-xlsx-object-data])
                               (send xlsx
                                     add-data-sheet
                                     #:sheet_name "test"
                                     #:sheet_data sheet_data)
                               xlsx))
  (define sample-xlsx-object-as-list (list sample-xlsx-object-data))

  (define slightly-different-xlsx-list-1 (list (list (list "This" "is" "test" "data")
                                                     (list "" "" "" "")
                                                     (list 428946239037292000 4950 6083 2299))))
  (define slightly-different-xlsx-list-2 (list (list (list "This" "is" "test" "data")
                                                     (list "" "" "" "")
                                                     (list 428946239037291650 4950 6083 2299))))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Tests
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (check-equal? (with-input-from-xlsx-file small-sample-xlsx xlsx->list)
                small-sample-xlsx-as-list
                "Create cell list from readable xlsx object.")

  (check-equal? (xlsx-file->list small-sample-xlsx)
                small-sample-xlsx-as-list
                "Create cell list from xlsx file path.")

  (check-equal? (with-readable-version sample-xlsx-object xlsx->list)
                sample-xlsx-object-as-list
                "Able to convert writable xlsx object to readable.")

  (check xlsx-list-equal?
         sample-xlsx-object-as-list
         sample-xlsx-object-as-list
         "Check xlsx-list-equal? compares the same list with itself correctly.")

  (check xlsx-list-equal?
         slightly-different-xlsx-list-1
         slightly-different-xlsx-list-2
         "Check xlsx-list-equal? compares slightly different lists correctly."))
