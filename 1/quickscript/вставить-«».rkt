#lang racket/base

(require quickscript)

(define-script вставить-«
  #:label "Вставить «"
  #:shortcut #\б
  #:shortcut-prefix (ctl)
  (λ (str) "«"))

(define-script вставить-»
  #:label "Вставить »"
  #:shortcut #\ю
  #:shortcut-prefix (ctl)
  (λ (str) "»"))
