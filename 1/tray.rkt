#lang racket/gui
(require (prefix-in win: "private/tray-win.rkt")
         (prefix-in gtk: "private/tray-gtk.rkt"))
(provide tray% make-icon)

(define-values (tray% make-icon)
  (if (eq? (system-type) 'windows)
      (values win:tray% win:make-icon)
      (values gtk:tray% gtk:make-icon)))
