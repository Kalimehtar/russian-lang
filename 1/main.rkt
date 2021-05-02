#lang racket/base

(module reader syntax/module-reader
  1/dict
  #:read my-read
  #:read-syntax my-read-syntax
  #:language-info #(1/language-info get-language-info #f)
  #:info (λ (key default f)
          (define (fallback) (f key default))
           (case key
             [(drracket:submit-predicate)
              (λ (ip _)
                (define str ((dynamic-require 'racket/string 'string-trim) ((dynamic-require 'racket/port 'port->string) ip) " " #:repeat? #t))
                (define l (string-length str))
                (or (= l 0)
                    (string=? (substring str (- l 1) l) "\n")))]
             [(drracket:indentation)
              (dynamic-require '1/indent 'indent)]
             [else (fallback)]))
  (require 1/reader))