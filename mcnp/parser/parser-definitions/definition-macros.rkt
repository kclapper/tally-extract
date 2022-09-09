#lang racket

(require (for-syntax syntax/parse
                     racket/list)
         parser-tools/yacc
         "lexer.rkt")

;; Define a grammar rule to be used in a make-parser. This definition
;; can be in a separate file or module so long as the definition is imported.
(define-syntax (define-grammar-rule stx)
  (syntax-parse stx
    [(make-grammar-rule (rule-name ((rules/tokens ...+) expr) ...+))
     #'(define-syntax rule-name #'(rule-name ((rules/tokens ...) expr) ...))]))

;; Create's a parser-tools parser using grammar rules defined with
;; make-grammar-rule.
(define-syntax (make-parser stx)
  (syntax-parse stx
    [(make-parser . pre-defined-rules)
     #:with grammar-rules (datum->syntax stx
                                         `(grammar ,@(expand-pre-defined-rules #'pre-defined-rules)))
     #:with (_ (start-rule ((_ ...) _) ...) . _) #'grammar-rules
     #'(parser (start start-rule)
               (end EOF)
               (tokens value-tokens
                       marker-tokens
                       aux-marker-tokens)
               (src-pos)
               (error (lambda (a b c d e)
                        (begin (printf
                                "Parse Error\n-----------\nValid token? = ~a\nToken name = ~a\nToken value = ~a\nStart pos = ~a\nEnd pos = ~a\n"
                                a b c d e)
                               (void))))
               (suppress)
               grammar-rules)]))

;; Expands grammar rules defined using make-grammar-rule
(define-for-syntax (expand-pre-defined-rules rule-stx)
  (syntax-parse rule-stx
    [(one-rule)
     #:with expanded-rule (syntax-local-value #'one-rule)
     (cons (syntax->datum #'expanded-rule) empty)]
    [(first-rule . rest-rules)
     #:with expanded-rule (syntax-local-value #'first-rule)
     (cons (syntax->datum #'expanded-rule) (expand-pre-defined-rules #'rest-rules))]))

(provide define-grammar-rule
         make-parser)

(module+ test
  (require rackunit
           syntax/macro-testing)

  ;; Parses a string with a given parser, used for evaluating the
  ;; results of a test parser.
  (define (parse-string-with-parser str parser)
    (call-with-input-string str
                            (lambda (input-port)
                              (parser (lambda () (next-token input-port))))))

  (test-case "Test grammar rule definitions."
    (define-grammar-rule (sum ((NUMBER) $1)
                              ((sum NUMBER) (+ $1 $2))))
    (define-grammar-rule (wordy-sum ((WORD sum) $2)))

    (check-equal? (parse-string-with-parser "1 2" (make-parser sum))
                  3
                  "Check single rule")

    (check-equal? (parse-string-with-parser "cherries 1 2" (make-parser wordy-sum
                                                                        sum))
                  3
                  "Check multiple rules"))

  ;; Test grammar rules defined in separate modules
  (module grammar-rule-1 racket
    (require (submod ".." ".."))
    (define-grammar-rule
      (remote-sum ((NUMBER) $1)
                  ((remote-sum NUMBER) (+ 1 2))))
    (provide remote-sum))

  (module grammar-rule-2 racket
    (require (submod ".." ".."))
    (define-grammar-rule
      (remote-wordy-sum ((WORD remote-sum) $2)))
    (provide remote-wordy-sum))

  (require 'grammar-rule-1
           'grammar-rule-2)

  (check-equal? (parse-string-with-parser "1 2" (make-parser remote-sum))
                3
                "Check imported rule definition with single rule.")

  (check-equal? (parse-string-with-parser "cherries 1 2" (make-parser remote-wordy-sum
                                                                      remote-sum))
                3
                "Check imported rule definitions with multiple rules.")
)
