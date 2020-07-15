#lang racket
(define-syntax (подключить stx)
  (syntax-case stx ()
    [(_ x)
     #`(require
         #,(datum->syntax #'x
                          `(file ,(path->string (collection-file-path (syntax-e #'x) "1")))))]))
(define-syntax (предоставить stx)
  (syntax-case stx ()
    [(_ x)
     #`(provide (all-from-out
                 #,(datum->syntax #'x
                                  `(file ,(path->string
                                           (collection-file-path (syntax-e #'x) "1"))))))]))


(подключить "словарь.rkt")
(предоставить "словарь.rkt")
