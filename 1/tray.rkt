#lang racket/gui
(require racket/runtime-path)
(provide tray% make-icon)

(define-runtime-path win-impl "private/tray-win.rkt")
(define-runtime-path gtk-impl "private/tray-gtk.rkt")

(define-values (tray% make-icon)
  (if (eq? (system-type) 'windows)
      (values (dynamic-require win-impl 'tray%)
              (dynamic-require win-impl 'make-icon))
      (values (dynamic-require gtk-impl 'tray%)
              (dynamic-require gtk-impl 'make-icon))))
