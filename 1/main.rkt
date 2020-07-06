#lang racket/base

(module reader syntax/module-reader
  1/dict
  #:read my-read
  #:read-syntax my-read-syntax
  #:language-info #(1/language-info get-language-info #f)
  (require 1/reader)
  (define (get-info key default f)
    (define (fallback) (f key default))
    (case key
      [(drracket:indentation)
       (dynamic-require '1/indent 'indent)]
      [else (fallback)])))