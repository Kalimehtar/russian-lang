#lang racket/base
;; Taskbar balloon tip, as in plyer win/libs/balloontip.py
;; (shown from a thread, as in win/notification.py).
(require ffi/unsafe
         ffi/unsafe/define
         ffi/winapi)

(provide show-notification)

(define user32 (ffi-lib "user32.dll" #:fail (λ () #f)))
(define shell32 (ffi-lib "shell32.dll" #:fail (λ () #f)))
(define kernel32 (ffi-lib "kernel32.dll" #:fail (λ () #f)))
(define-ffi-definer define-user32 user32
  #:default-make-fail make-not-available)
(define-ffi-definer define-shell32 shell32
  #:default-make-fail make-not-available)
(define-ffi-definer define-kernel32 kernel32
  #:default-make-fail make-not-available)

(define WS_OVERLAPPED 0)
(define LR_LOADFROMFILE 16)
(define LR_DEFAULTSIZE #x40)
(define IDI_APPLICATION 32512)
(define IMAGE_ICON 1)
(define NIM_ADD 0)
(define NIM_DELETE 2)
(define NIM_SETVERSION 4)
(define NIF_ICON 2)
(define NIF_TIP 4)
(define NIF_INFO #x10)
(define NIIF_USER 4)
(define NIIF_LARGE_ICON #x20)
(define NOTIFYICON_VERSION_4 4)

(define WNDPROC
  (_fun #:abi winapi
        _pointer _uint32 _uintptr _intptr
        -> _intptr))

(define-user32 LoadIconW
  (_fun #:abi winapi _pointer _intptr -> _pointer))
(define-user32 LoadImageW
  (_fun #:abi winapi
        _pointer _string/utf-16 _uint32 _int32 _int32 _uint32
        -> _pointer))
(define-user32 DestroyIcon
  (_fun #:abi winapi _pointer -> _bool))
(define-user32 RegisterClassExW
  (_fun #:abi winapi _pointer -> _uint16))
(define-user32 UnregisterClassW
  (_fun #:abi winapi _pointer _pointer -> _bool))
(define-user32 CreateWindowExW
  (_fun #:abi winapi
        _uint32 _pointer _pointer _uint32
        _int32 _int32 _int32 _int32
        _pointer _pointer _pointer _pointer
        -> _pointer))
(define-user32 DestroyWindow
  (_fun #:abi winapi _pointer -> _bool))
(define-user32 UpdateWindow
  (_fun #:abi winapi _pointer -> _bool))
(define-user32 DefWindowProcW
  (_fun #:abi winapi
        _pointer _uint32 _uintptr _intptr
        -> _intptr))
(define-kernel32 GetModuleHandleW
  (_fun #:abi winapi _pointer -> _pointer))
(define-kernel32 GetLastError
  (_fun #:abi winapi -> _uint32))
(define-shell32 Shell_NotifyIconW
  (_fun #:abi winapi _uint32 _pointer -> _bool))

(define-cstruct _GUID
  ([data1 _uint32]
   [data2 _uint16]
   [data3 _uint16]
   [data4 (_array _uint8 8)]))

(define-cstruct _WNDCLASSEXW
  ([cbSize _uint32]
   [style _uint32]
   [lpfnWndProc _fpointer]
   [cbClsExtra _int32]
   [cbWndExtra _int32]
   [hInstance _pointer]
   [hIcon _pointer]
   [hCursor _pointer]
   [hbrBackground _pointer]
   [lpszMenuName _pointer]
   [lpszClassName _pointer]
   [hIconSm _pointer]))

(define-cstruct _NOTIFYICONDATAW
  ([cbSize _uint32]
   [hWnd _pointer]
   [uID _uint32]
   [uFlags _uint32]
   [uCallbackMessage _uint32]
   [hIcon _pointer]
   [szTip (_array _uint16 128)]
   [dwState _uint32]
   [dwStateMask _uint32]
   [szInfo (_array _uint16 256)]
   [uVersion _uint32]
   [szInfoTitle (_array _uint16 64)]
   [dwInfoFlags _uint32]
   [guidItem _GUID]
   [hBalloonIcon _pointer]))

(define (malloc-utf16 str)
  (define n (string-length str))
  (define p (malloc (* 2 (add1 n)) 'atomic))
  (for ([i (in-range n)])
    (ptr-set! p _uint16 i (char->integer (string-ref str i))))
  (ptr-set! p _uint16 n 0)
  p)

(define (write-utf16-z! dest max-units str)
  (define ptr (if (cpointer? dest) dest (array-ptr dest)))
  (define src (if (string? str) str ""))
  (define len (min (string-length src) (sub1 max-units)))
  (for ([i (in-range max-units)])
    (ptr-set! ptr _uint16 i
              (if (< i len)
                  (let ([c (char->integer
                            (string-ref src i))])
                    (if (<= c #xFFFF) c (char->integer #\?)))
                  0))))

(define (as-string v)
  (cond
    [(string? v) v]
    [(bytes? v) (bytes->string/utf-8 v #\?)]
    [(path? v) (path->string v)]
    [else ""]))

(define count-sem (make-semaphore 1))
(define notify-count 0)
(define lingering null)
(define (linger v)
  (set! lingering (cons v lingering)))
(define (next-id)
  (dynamic-wind
   (λ () (semaphore-wait count-sem))
   (λ ()
     (begin0 notify-count
       (set! notify-count (add1 notify-count))))
   (λ () (semaphore-post count-sem))))

(define (icon-path v)
  (define s (as-string v))
  (if (equal? s "") #f s))

(define (timeout-seconds v)
  (and (real? v) (positive? v) v))

(define (call-def-window-proc hwnd msg wp lp)
  (DefWindowProcW hwnd msg wp lp))

(define (make-wndproc)
  (function-ptr call-def-window-proc WNDPROC))

(define (fill-balloon! nid hwnd uid hicon balloon-icon
                       app-name title message)
  (define flags
    (bitwise-ior NIF_TIP NIF_INFO
                 (if hicon NIF_ICON 0)))
  (define info-flags
    (if balloon-icon
        (bitwise-ior NIIF_USER NIIF_LARGE_ICON)
        0))
  (set-NOTIFYICONDATAW-cbSize!
   nid (ctype-sizeof _NOTIFYICONDATAW))
  (set-NOTIFYICONDATAW-hWnd! nid hwnd)
  (set-NOTIFYICONDATAW-uID! nid uid)
  (set-NOTIFYICONDATAW-uFlags! nid flags)
  (set-NOTIFYICONDATAW-uCallbackMessage! nid 0)
  (set-NOTIFYICONDATAW-hIcon! nid hicon)
  (write-utf16-z! (NOTIFYICONDATAW-szTip nid) 128 app-name)
  (set-NOTIFYICONDATAW-dwState! nid 0)
  (set-NOTIFYICONDATAW-dwStateMask! nid 0)
  (write-utf16-z! (NOTIFYICONDATAW-szInfo nid) 256 message)
  (set-NOTIFYICONDATAW-uVersion! nid NOTIFYICON_VERSION_4)
  (write-utf16-z!
   (NOTIFYICONDATAW-szInfoTitle nid) 64 title)
  (set-NOTIFYICONDATAW-dwInfoFlags! nid info-flags)
  (set-NOTIFYICONDATAW-hBalloonIcon! nid balloon-icon))

(define (alloc-nid)
  (define n (ctype-sizeof _NOTIFYICONDATAW))
  (define p (cast (malloc n 'atomic-interior)
                  _pointer
                  _NOTIFYICONDATAW-pointer))
  (memset p 0 n)
  (set-NOTIFYICONDATAW-cbSize! p n)
  p)

(define (balloon-tip title message app-name icon timeout)
  (define uid (next-id))
  (define class-name
    (string-append "AdinaNotify" (number->string uid)))
  (define class-z (malloc-utf16 class-name))
  (define hinst (GetModuleHandleW #f))
  (unless hinst
    (error 'show-notification
           "GetModuleHandleW failed"))
  (define wndproc (make-wndproc))
  (define wc
    (make-WNDCLASSEXW
     (ctype-sizeof _WNDCLASSEXW) 0
     (cast wndproc _pointer _fpointer)
     0 0 hinst #f #f #f #f class-z #f))
  (define atom (RegisterClassExW wc))
  (unless (not (zero? atom))
    (error 'show-notification
           "RegisterClassExW failed"))
  (define hwnd
    (CreateWindowExW 0 class-z #f WS_OVERLAPPED
                     0 0 0 0
                     #f #f hinst #f))
  (unless hwnd
    (UnregisterClassW class-z hinst)
    (error 'show-notification
           "CreateWindowExW failed"))
  (UpdateWindow hwnd)
  (define path (icon-path icon))
  (define balloon-icon
    (and path
         (LoadImageW #f path IMAGE_ICON 0 0
                     (bitwise-ior LR_LOADFROMFILE
                                  LR_DEFAULTSIZE))))
  (when (and path (not balloon-icon))
    (DestroyWindow hwnd)
    (UnregisterClassW class-z hinst)
    (error 'show-notification
           "failed to load icon\n  path: ~e" path))
  (define hicon
    (or balloon-icon
        (LoadIconW #f IDI_APPLICATION)))
  (define nid (alloc-nid))
  (fill-balloon! nid hwnd uid hicon balloon-icon
                 (as-string app-name)
                 (as-string title)
                 (as-string message))
  (unless (Shell_NotifyIconW NIM_ADD nid)
    (when balloon-icon (DestroyIcon balloon-icon))
    (DestroyWindow hwnd)
    (UnregisterClassW class-z hinst)
    (error 'show-notification "Shell_NotifyIconW NIM_ADD failed"))
  (unless (Shell_NotifyIconW NIM_SETVERSION nid)
    (Shell_NotifyIconW NIM_DELETE nid)
    (when balloon-icon (DestroyIcon balloon-icon))
    (DestroyWindow hwnd)
    (UnregisterClassW class-z hinst)
    (error 'show-notification
           "Shell_NotifyIconW NIM_SETVERSION failed"))
  (define (cleanup)
    (Shell_NotifyIconW NIM_DELETE nid)
    (when balloon-icon (DestroyIcon balloon-icon))
    (DestroyWindow hwnd)
    (UnregisterClassW class-z hinst)
    (void wndproc))
  (define wait (timeout-seconds timeout))
  (cond
    [(not timeout)
     (linger (list wndproc hwnd nid balloon-icon
                   class-z hinst))]
    [(not wait)
     (cleanup)]
    [else
     (sleep wait)
     (cleanup)]))

(define (show-notification title message app-name icon timeout)
  (thread
   (λ ()
     (with-handlers
         ([exn:fail?
           (λ (e)
             (log-error "notify: ~a" (exn-message e)))])
       (balloon-tip title message app-name icon timeout))))
  (void))
