#lang racket/base
(provide get-language-info)
(define (get-language-info data)
  (lambda (key default)
    (case key
      [(configure-runtime)
       '(#(1/private/runtime-config runtime-config! #f))]
      [else default])))