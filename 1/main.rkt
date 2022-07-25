#lang racket/base

(module reader syntax/module-reader
  1/lang
  #:read my-read
  #:read-syntax my-read-syntax
  #:language-info #(1/language-info get-language-info #f)
  #:info (λ (key default f)
          (define (fallback) (f key default))
           (case key
             [(drracket:submit-predicate)
              (let ([string-trim (dynamic-require 'racket/string 'string-trim)]
                    [port->string (dynamic-require 'racket/port 'port->string)])
                (λ (ip _)
                  (define str (string-trim (port->string ip) " " #:repeat? #t))
                  (define l (string-length str))
                  (or (= l 0)
                      (string=? (substring str (- l 1) l) "\n"))))]
             [(drracket:indentation)
              (dynamic-require '1/indent 'indent)]
             [(drracket:default-extension) "1"]
             [(drracket:default-filters)
              `(["Исходники программ" "*.1"])]
             [(color-lexer)
              (let ([racket-lexer (dynamic-require 'syntax-color/racket-lexer 'racket-lexer)])
                (λ (in)
                  (define-values (str type r1 r2 r3) (racket-lexer in))
                  (if (or (equal? str "истина") (equal? str "ложь"))
                      (values str 'constant r1 r2 r3)
                      (values str type r1 r2 r3))))]
             [else (fallback)]))
  (require 1/reader))