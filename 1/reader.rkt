#lang racket
(define-syntax (подключить stx)
  (syntax-case stx ()
    [(_ x)
     #`(require
         #,(datum->syntax #'x
                          `(file ,(path->string (collection-file-path (syntax-e #'x) "1")))))]))

(подключить "чтец.rkt")
(provide my-read my-read-syntax)
