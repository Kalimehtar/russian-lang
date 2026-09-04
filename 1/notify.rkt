#lang racket/base
(require racket/runtime-path)
(provide show-notification)

(define-runtime-path win-impl "private/notify-win.rkt")
(define-runtime-path linux-impl "private/notify-linux.rkt")

(define show-notification
  (if (eq? (system-type) 'windows)
      (dynamic-require win-impl 'show-notification)
      (dynamic-require linux-impl 'show-notification)))
