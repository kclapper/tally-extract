;; Code based heavily on https://docs.racket-lang.org/lex-yacc-example/index.html

#lang racket

(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre))

(define-tokens value-tokens (WORD
                             NUMBER))

(define-empty-tokens marker-tokens (SOT
                                    EOT
                                    CELL-M
                                    CELL-COL-M
                                    PARTICLE-M
                                    UNITS-M
                                    SEGMENT-M
                                    SEGMENT-COL-M
                                    LAST-SEG-M
                                    VOLUMES-M
                                    ENERGY-M
                                    NPS-M
                                    NEWLINE
                                    EOF))

(define-empty-tokens aux-marker-tokens (KEFF-M
                                        WLOSS-M-2
                                        WLOSS-M-1))

(define next-token
  (lexer-src-pos
   ((eof) (token-EOF))
   (#\newline (token-NEWLINE))
   ((:+ (:- whitespace #\newline)) (return-without-pos (next-token input-port)))
   ("1tally" (token-SOT))
   ((:>= 50 "=") (token-EOT))
   ("nps =" (token-NPS-M))
   ("cell" (token-CELL-M))
   ("cell:" (token-CELL-COL-M))
   ("segment" (token-SEGMENT-M))
   ("segment:" (token-SEGMENT-COL-M))
   ("whole" (token-LAST-SEG-M))
   ("volumes" (token-VOLUMES-M))
   ("energy" (token-ENERGY-M))
   ("tally for" (token-PARTICLE-M))
   ("units" (token-UNITS-M))
   ("final result" (token-KEFF-M))
   ("prompt fission" (token-WLOSS-M-1))
   ("loss to fission" (token-WLOSS-M-2))
   ((:: (:? "-") (:+ numeric) (:? (:: #\. (:* numeric) (:? (:: "E" (:or "+" "-") (:+ numeric)))))) (token-NUMBER (string->number lexeme)))
   ((:+ (:or alphabetic symbolic numeric punctuation)) (token-WORD lexeme))))

(define (test-lex port)
  (port-count-lines! port)
  (letrec ([one-line
            (lambda ()
              (let ([result (next-token port)])
                (unless (equal? (position-token-token result) `EOF)
                  (printf "~a\n" result)
                  (one-line))))])
    (one-line)))

(define (my-lex-test str)
  (test-lex (open-input-string str)))

(provide my-lex-test
         value-tokens
         marker-tokens
         aux-marker-tokens
         next-token)
