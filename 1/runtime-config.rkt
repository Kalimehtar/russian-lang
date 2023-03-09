#lang racket/base
(require 1/reader 1/russify)
(provide configure)
(define (configure data)
  (current-output-port (russian-port (current-output-port)))
  (current-error-port (russian-port (current-error-port)))
  (current-read-interaction my-read-syntax))