#lang racket/base
(require (for-syntax racket/base))
(provide синоним)

(define-syntax (синоним stx)
  (syntax-case stx ()
    [(_ старый новый)
     #'(define-syntax новый (make-rename-transformer #'старый))]))
