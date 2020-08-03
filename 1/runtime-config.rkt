#lang racket/base
(require 1/reader)
(provide configure)
(define (configure data)
  (current-read-interaction my-read-syntax))