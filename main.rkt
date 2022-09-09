#lang racket

(require racket/cmdline)

(require "mcnp/main.rkt"
         "excel.rkt")

(define (make-default-output-name)
  (define args (vector->list (current-command-line-arguments)))
  (cond [(= (length args) 1)
         (string-append (first args) ".xlsx")]))

(define out-file (make-parameter (make-default-output-name)))

(command-line #:program "tally-extract"
              #:usage-help "Used to get tally data from MCNP output files."
              #:once-each
              [("-o" "-O" "--output") output-file
                                      "Excel file to store tally data. Defaults to <input-file>.xlsx"
                                      (out-file output-file)]
              #:args (input-file)

              (write-tallys (file->tallys input-file)
                            (out-file)))
