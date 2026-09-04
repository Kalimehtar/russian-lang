#lang racket/gui
;; Linux tray via GtkStatusIcon (pystray _gtk.py).
(require ffi/unsafe
         ffi/unsafe/define
         racket/draw
         "tray-keep-alive.rkt")

(provide tray% make-icon)

(define gtk-lib
  (ffi-lib "libgtk-3" '("0" "") #:global? #t
           #:fail (λ () #f)))
(define gobj-lib
  (ffi-lib "libgobject-2.0" '("0" "")
           #:fail (λ () #f)))
(define pixbuf-lib
  (or (ffi-lib "libgdk_pixbuf-2.0" '("0" "")
               #:fail (λ () #f))
      gtk-lib))

(define-ffi-definer define-gtk gtk-lib
  #:default-make-fail make-not-available)
(define-ffi-definer define-gobj gobj-lib
  #:default-make-fail make-not-available)
(define-ffi-definer define-pixbuf pixbuf-lib
  #:default-make-fail make-not-available)

(define-gtk gtk_init_check
  (_fun (_pointer = #f) (_pointer = #f) -> _bool))
(define-gtk gtk_status_icon_new (_fun -> _pointer))
(define-gtk gtk_status_icon_set_visible
  (_fun _pointer _bool -> _void))
(define-gtk gtk_status_icon_set_from_file
  (_fun _pointer _path -> _void))
(define-gtk gtk_status_icon_set_from_icon_name
  (_fun _pointer _string -> _void))
(define-gtk gtk_status_icon_set_title
  (_fun _pointer _string -> _void))
(define-gtk gtk_status_icon_set_tooltip_text
  (_fun _pointer _string -> _void))
(define-gobj g_object_ref_sink (_fun _pointer -> _pointer))
(define-gobj g_object_unref (_fun _pointer -> _void))
(define-gobj g_signal_connect_data
  (_fun _pointer _string _fpointer _pointer _pointer _int
        -> _ulong))
(define-pixbuf gdk_pixbuf_new_from_file
  (_fun _path (_pointer = #f) -> _pointer))
(define-pixbuf gdk_pixbuf_savev
  (_fun _pointer _path _string _pointer _pointer _pointer
        -> _bool))

(define sym-names
  '((app . "application-x-executable")
    (err . "dialog-error")
    (quest . "dialog-question")
    (warning . "dialog-warning")
    (inform . "dialog-information")
    (winlogo . "start-here")
    (blank . "image-missing")))

(define id->sym
  (make-hash
   '((32512 . app)
     (32513 . err)
     (32514 . quest)
     (32515 . warning)
     (32516 . inform)
     (32517 . winlogo)
     (32518 . blank))))

(define (system-icon-id? v)
  (and (exact-integer? v) (<= 32512 v 32518)))

(define (label->string lb)
  (cond
    [(string? lb) lb]
    [(bytes? lb)
     (define n (bytes-length lb))
     (define cut
       (if (and (> n 0) (zero? (bytes-ref lb (sub1 n))))
           (sub1 n)
           n))
     (bytes->string/utf-8 (subbytes lb 0 cut) #\?)]
    [else ""]))

(define (file-icon? v)
  (or (path? v) (string? v)))

(define (save-pixbuf-png src tmp)
  (with-handlers ([exn:fail? (λ (_) #f)])
    (define pb (gdk_pixbuf_new_from_file src))
    (and pb
         (begin0
           (gdk_pixbuf_savev pb tmp "png" #f #f #f)
           (g_object_unref pb)))))

(define (file->png-temp src)
  (define tmp (make-temporary-file "racket-tray-~a.png"))
  (define bm
    (with-handlers ([exn:fail? (λ (_) #f)])
      (read-bitmap src)))
  (cond
    [(and bm (send bm ok?))
     (send bm save-file tmp 'png)
     tmp]
    [(save-pixbuf-png src tmp)
     tmp]
    [else
     (with-handlers ([exn:fail? void]) (delete-file tmp))
     (error 'make-icon
            "failed to load icon\n  path: ~e" src)]))

(define (make-icon path)
  (file->png-temp (path->complete-path path)))

(define (normalize-icon v)
  (cond
    [(symbol? v)
     (if (assoc v sym-names) v 'inform)]
    [(system-icon-id? v)
     (hash-ref id->sym v 'inform)]
    [(file-icon? v) v]
    [else 'inform]))

(define (icon-name v)
  (define p (assoc v sym-names))
  (if p (cdr p) "dialog-information"))

(define gtk-ok? #f)
(define (ensure-gtk)
  (unless gtk-lib
    (error 'tray% "libgtk-3 not found"))
  (unless gobj-lib
    (error 'tray% "libgobject-2.0 not found"))
  (unless gtk-ok?
    (unless (gtk_init_check)
      (error 'tray% "Gtk could not be initialised"))
    (set! gtk-ok? #t)))

(define next-uid 1)
(define id-handlers (make-hash))

(define (click-event left? right?)
  (new mouse-event%
       [event-type (if left? 'left-down 'right-down)]
       [x 0]
       [y 0]
       [left-down left?]
       [right-down right?]
       [middle-down #f]))

(define (dispatch-tray uid left?)
  (define info (hash-ref id-handlers uid #f))
  (define handle
    (and info (weak-box-value (vector-ref info 1))))
  (when handle
    (with-handlers
        ([exn:fail?
          (λ (e) (log-error "tray: ~a" (exn-message e)))])
      (handle (click-event left? (not left?))))))

(define (queue-tray uid left?)
  (define info (hash-ref id-handlers uid #f))
  (define es (and info (vector-ref info 0)))
  (define thunk (λ () (dispatch-tray uid left?)))
  (if es
      (parameterize ([current-eventspace es])
        (queue-callback thunk))
      (queue-callback thunk)))

(define (on-activate _icon user)
  (queue-tray (cast user _pointer _intptr) #t))

(define (on-popup _icon _button _time user)
  (queue-tray (cast user _pointer _intptr) #f))

(define activate-fn
  (function-ptr on-activate
                (_fun #:atomic? #t _pointer _pointer -> _void)))
(define popup-fn
  (function-ptr on-popup
                (_fun #:atomic? #t
                      _pointer _uint _uint32 _pointer
                      -> _void)))

(define (connect-signals icon uid)
  (define data (cast uid _intptr _pointer))
  (g_signal_connect_data icon "activate" activate-fn data #f 0)
  (g_signal_connect_data icon "popup-menu" popup-fn data #f 0))

(define (make-tray-finalizer icon uid fs-box shown-box)
  (λ (_)
    (when (unbox shown-box)
      (tray-keep-alive #f)
      (set-box! shown-box #f))
    (hash-remove! id-handlers uid)
    (define p (unbox fs-box))
    (when p
      (with-handlers ([exn:fail? void]) (delete-file p))
      (set-box! fs-box #f))
    (when icon
      (with-handlers ([exn:fail? void])
        (gtk_status_icon_set_visible icon #f)
        (g_object_unref icon)))))

(define tray%
  (class object%
    (init-field
     [label ""]
     [icon 'inform]
     [callback (λ (snd ev) (void))])
    (super-new)
    (ensure-gtk)
    (define current-callback callback)
    (define show-flag #f)
    (define shown-box (box #f))
    (define icon-key (normalize-icon icon))
    (define fs-box (box #f))
    (define uid next-uid)
    (set! next-uid (add1 next-uid))
    (define status
      (g_object_ref_sink (gtk_status_icon_new)))
    (unless status
      (error 'tray% "gtk_status_icon_new failed"))
    (gtk_status_icon_set_visible status #f)
    (connect-signals status uid)
    (define eventspace (current-eventspace))
    (define (on-click ev)
      (current-callback this ev))
    (hash-set! id-handlers uid
               (vector eventspace (make-weak-box on-click)))
    (define (remove-fs-icon)
      (define p (unbox fs-box))
      (when p
        (with-handlers ([exn:fail? void]) (delete-file p))
        (set-box! fs-box #f)))
    (define (update-fs-icon src)
      (remove-fs-icon)
      (set-box! fs-box (file->png-temp src)))
    (define (update-icon)
      (cond
        [(symbol? icon-key)
         (remove-fs-icon)
         (gtk_status_icon_set_from_icon_name
          status (icon-name icon-key))]
        [else
         (update-fs-icon icon-key)
         (gtk_status_icon_set_from_file
          status (unbox fs-box))]))
    (define (update-title)
      (define s (label->string label))
      (gtk_status_icon_set_title status s)
      (gtk_status_icon_set_tooltip_text status s))
    (update-icon)
    (update-title)
    (register-finalizer this
                        (make-tray-finalizer status uid fs-box shown-box))
    (define/public (show x)
      (define want (and x #t))
      (cond
        [(eq? want show-flag)
         (when want
           (update-icon)
           (update-title))]
        [want
         (update-icon)
         (update-title)
         (gtk_status_icon_set_visible status #t)
         (set! show-flag #t)
         (set-box! shown-box #t)
         (tray-keep-alive #t)]
        [else
         (gtk_status_icon_set_visible status #f)
         (set! show-flag #f)
         (set-box! shown-box #f)
         (tray-keep-alive #f)]))
    (define/public (is-shown?) show-flag)
    (define/public (get-icon) icon-key)
    (define/public (set-icon ic)
      (set! icon-key (normalize-icon ic))
      (update-icon)
      (when show-flag (update-title)))
    (define/public (get-label) label)
    (define/public (set-label lb)
      (set! label lb)
      (update-title))
    (define/public (new-callback x)
      (set! current-callback x))))
