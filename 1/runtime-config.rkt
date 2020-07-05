#lang racket/base
(require (file "чтец.rkt"))
(provide configure)
(define (configure data)
  (current-read-interaction my-read-syntax))