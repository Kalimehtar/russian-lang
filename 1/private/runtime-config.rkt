#lang racket/base
(require 1/reader 1/russify racket/interaction-info)
(provide install-runtime-config! runtime-config! parameters)

(define parameters
  (list current-interaction-info
        current-read-interaction
	current-output-port
	current-error-port))

(define (install-runtime-config!)
  (current-interaction-info '#((submod 1/main reader)
                               get-interaction-info
                               #f))
  (define old-output (current-output-port))
  (current-output-port (russian-port (current-output-port)))
  (current-error-port (russian-port (current-error-port)))
  (current-read-interaction 
    (lambda (src in)
      (when (terminal-port? in)
        (flush-output old-output))
      (my-read-syntax src in))))

(define (runtime-config! _) (install-runtime-config!))
