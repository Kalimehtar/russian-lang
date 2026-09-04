#lang racket/gui
;; Shown tray icons must keep the eventspace from going idle:
;; (yield eventspace) returns when no frames are shown, so a
;; compiled GUI exits after hide-to-tray.
(require ffi/unsafe
         ffi/unsafe/define
         ffi/winapi)

(provide tray-keep-alive)

(define shown 0)
(define keeper #f)

(define user32 (ffi-lib "user32.dll" #:fail (λ () #f)))
(define-ffi-definer define-user32 user32
  #:default-make-fail make-not-available)
(define-user32 ShowWindow
  (_fun #:abi winapi _pointer _int32 -> _int32))
(define SW_HIDE 0)

(define (cloak-keeper)
  (define h (and keeper (send keeper get-handle)))
  (when (and h user32)
    (ShowWindow h SW_HIDE)))

(define (ensure-keeper)
  (unless keeper
    (set! keeper
          (new frame%
               [label ""]
               [width 1]
               [height 1]
               [x -32000]
               [y -32000]
               [style '(no-caption no-resize-border
                        no-system-menu)])))
  (unless (send keeper is-shown?)
    (send keeper show #t)
    (cloak-keeper)))

(define (tray-keep-alive on?)
  (set! shown (max 0 (+ shown (if on? 1 -1))))
  (if (positive? shown)
      (ensure-keeper)
      (when (and keeper (send keeper is-shown?))
        (send keeper show #f))))
