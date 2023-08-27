#lang racket/base

;; для командной строки
(require 1/lang)
(provide (all-from-out 1/lang))

(module reader syntax/module-reader
  #:language '1/lang
  #:read my-read
  #:read-syntax my-read-syntax
  #:language-info #(1/language-info get-language-info #f)
  #:info get-info-proc
  (require 1/reader)
  (provide get-info-proc)
  (define (get-info-proc key default make-default)
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
              (dynamic-require '1/lexer 'racket-lexer)]
             [else (make-default key default)])))

(module configure-runtime racket/base
  (require 1/runtime-config))

(module configure-expand racket/base
  (require 1/expand-config)
  (provide enter-parameterization
	   exit-parameterization))
