#lang racket/base
(require (prefix-in win: "private/notify-win.rkt")
         (prefix-in linux: "private/notify-linux.rkt"))
(provide show-notification)

(define show-notification
  (if (eq? (system-type) 'windows)
      win:show-notification
      linux:show-notification))
