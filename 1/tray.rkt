#lang racket/gui
(require ffi/unsafe
         ffi/unsafe/define
         ffi/winapi)

(provide tray% make-icon)

(define-ffi-definer define-user32 (ffi-lib "user32.dll"))
(define-ffi-definer define-shell32 (ffi-lib "shell32.dll"))
(define-ffi-definer define-kernel32 (ffi-lib "kernel32.dll"))

(define NIM_ADD 0)
(define NIM_MODIFY 1)
(define NIM_DELETE 2)
(define NIF_MESSAGE 1)
(define NIF_ICON 2)
(define NIF_TIP 4)
(define NIF_FLAGS (bitwise-ior NIF_MESSAGE NIF_ICON NIF_TIP))

(define IMAGE_ICON 1)
(define LR_LOADFROMFILE #x10)
(define SM_CXSMICON 49)
(define SM_CYSMICON 50)
(define WM_APP #x8000)
(define WM_TRAY (+ WM_APP 1))
(define WM_LBUTTONUP #x0202)
(define WM_LBUTTONDBLCLK #x0203)
(define WM_RBUTTONUP #x0205)
(define WM_RBUTTONDBLCLK #x0206)
(define WM_MBUTTONUP #x0208)
(define WM_MBUTTONDBLCLK #x0209)
(define WM_CONTEXTMENU #x007B)
(define NIN_SELECT #x0400)
(define NIN_KEYSELECT #x0401)
(define ERROR_CLASS_ALREADY_EXISTS 1410)
(define HWND_MESSAGE (cast -3 _intptr _pointer))
(define WS_POPUP #x80000000)
(define TRAY_TIP_LEN 64)

(define WNDPROC
  (_fun #:abi winapi
        _pointer _uint32 _uintptr _intptr
        -> _intptr))

(define-user32 GetCursorPos
  (_fun #:abi winapi _pointer -> _bool))
(define-user32 GetSystemMetrics
  (_fun #:abi winapi _int32 -> _int32))
(define-user32 LoadIconW
  (_fun #:abi winapi _pointer _intptr -> _pointer))
(define-user32 LoadImageW
  (_fun #:abi winapi
        _pointer _string/utf-16 _uint32 _int32 _int32 _uint32
        -> _pointer))
(define-user32 DestroyIcon
  (_fun #:abi winapi _pointer -> _bool))
(define-user32 RegisterClassW
  (_fun #:abi winapi _pointer -> _uint16))
(define-user32 CreateWindowExW
  (_fun #:abi winapi
        _uint32 _pointer _pointer _uint32
        _int32 _int32 _int32 _int32
        _pointer _pointer _pointer _pointer
        -> _pointer))
(define-user32 DefWindowProcW
  (_fun #:abi winapi
        _pointer _uint32 _uintptr _intptr
        -> _intptr))
(define-shell32 Shell_NotifyIconW
  (_fun #:abi winapi _uint32 _pointer -> _bool))
(define-kernel32 GetModuleHandleW
  (_fun #:abi winapi _pointer -> _pointer))
(define-kernel32 GetLastError
  (_fun #:abi winapi -> _uint32))

(define-cstruct _POINT
  ([x _int32]
   [y _int32]))

(define-cstruct _NOTIFYICONDATA
  ([cbSize _uint32]
   [hWnd _pointer]
   [uID _uint32]
   [uFlags _uint32]
   [uCallbackMessage _uint32]
   [hIcon _pointer]
   [szTip (_array _uint16 64)]))

(define-cstruct _WNDCLASSW
  ([style _uint32]
   [lpfnWndProc _fpointer]
   [cbClsExtra _int32]
   [cbWndExtra _int32]
   [hInstance _pointer]
   [hIcon _pointer]
   [hCursor _pointer]
   [hbrBackground _pointer]
   [lpszMenuName _pointer]
   [lpszClassName _pointer]))

(define sym-icon
  '((app . 32512)
    (err . 32513)
    (quest . 32514)
    (warning . 32515)
    (inform . 32516)
    (winlogo . 32517)
    (blank . 32518)))

(define id->sym
  (make-hash (map (λ (p) (cons (cdr p) (car p))) sym-icon)))

(define (system-icon-id? v)
  (and (exact-integer? v) (<= 32512 v 32518)))

(define (malloc-utf16 str)
  (define n (string-length str))
  (define p (malloc (* 2 (add1 n)) 'atomic))
  (for ([i (in-range n)])
    (ptr-set! p _uint16 i (char->integer (string-ref str i))))
  (ptr-set! p _uint16 n 0)
  p)

(define (write-utf16-z! dest max-units str)
  (define ptr (if (cpointer? dest) dest (array-ptr dest)))
  (define len (min (string-length str) (sub1 max-units)))
  (for ([i (in-range max-units)])
    (ptr-set! ptr _uint16 i
              (if (< i len)
                  (let ([c (char->integer (string-ref str i))])
                    (if (<= c #xFFFF) c (char->integer #\?)))
                  0))))

(define (label->string lb)
  (cond
    [(string? lb) lb]
    [(bytes? lb)
     (define n (bytes-length lb))
     (define cut
       (if (and (> n 0) (zero? (bytes-ref lb (sub1 n))))
           (sub1 n)
           n))
     (bytes->string/locale (subbytes lb 0 cut) #\?)]
    [else ""]))

(define (as-pointer v)
  (cond
    [(not v) #f]
    [(cpointer? v) v]
    [(exact-integer? v) (cast v _intptr _pointer)]
    [else #f]))

(define (cursor-position)
  (define pt (make-POINT 0 0))
  (if (GetCursorPos pt)
      (values (POINT-x pt) (POINT-y pt))
      (values 0 0)))

(define (lparam->mouse-event lp)
  (define msg (bitwise-and lp #xFFFF))
  (define-values (type left right middle)
    (cond
      [(memv msg (list WM_LBUTTONUP WM_LBUTTONDBLCLK
                       NIN_SELECT NIN_KEYSELECT))
       (values 'enter #t #f #f)]
      [(memv msg (list WM_RBUTTONUP WM_RBUTTONDBLCLK
                       WM_CONTEXTMENU))
       (values 'enter #f #t #f)]
      [(memv msg (list WM_MBUTTONUP WM_MBUTTONDBLCLK))
       (values 'enter #f #f #t)]
      [else (values #f #f #f #f)]))
  (and type
       (let-values ([(x y) (cursor-position)])
         (new mouse-event%
              [event-type type]
              [x x]
              [y y]
              [left-down left]
              [right-down right]
              [middle-down middle]))))

(define class-name-z (malloc-utf16 "RacketTrayMsgWnd"))
(define tray-hwnd #f)
(define tray-wndproc-keep #f)
(define next-uid 1)
(define id-handlers (make-hash))

(define (dispatch-tray uid lp)
  (define info (hash-ref id-handlers uid #f))
  (define handle (and info (weak-box-value (vector-ref info 1))))
  (when handle
    (with-handlers
        ([exn:fail?
          (λ (e) (log-error "tray: ~a" (exn-message e)))])
      (handle lp))))

(define (wndproc hwnd msg wparam lparam)
  (cond
    [(= msg WM_TRAY)
     (define uid (bitwise-and wparam #xFFFFFFFF))
     (define lp lparam)
     (define info (hash-ref id-handlers uid #f))
     (define es (and info (vector-ref info 0)))
     (define thunk (λ () (dispatch-tray uid lp)))
     (if es
         (parameterize ([current-eventspace es])
           (queue-callback thunk))
         (queue-callback thunk))
     0]
    [else
     (DefWindowProcW hwnd msg wparam lparam)]))

(define (ensure-message-window)
  (or tray-hwnd
      (let ()
        (define hinst (GetModuleHandleW #f))
        (define proc (function-ptr wndproc WNDPROC))
        (set! tray-wndproc-keep proc)
        (define wc
          (make-WNDCLASSW 0 (cast proc _pointer _fpointer) 0 0
                          hinst #f #f #f #f class-name-z))
        (define atom (RegisterClassW wc))
        (unless (or (not (zero? atom))
                    (= (GetLastError) ERROR_CLASS_ALREADY_EXISTS))
          (error 'tray% "RegisterClassW failed"))
        (define hwnd
          (or (CreateWindowExW 0 class-name-z #f 0 0 0 0 0
                               HWND_MESSAGE #f hinst #f)
              (CreateWindowExW 0 class-name-z #f WS_POPUP
                               -32000 -32000 0 0
                               #f #f hinst #f)))
        (unless hwnd
          (error 'tray% "CreateWindowExW failed"))
        (set! tray-hwnd hwnd)
        hwnd)))

(define (make-tray-finalizer nid uid owned-box)
  (λ (_)
    (Shell_NotifyIconW NIM_DELETE nid)
    (hash-remove! id-handlers uid)
    (define h (unbox owned-box))
    (when h
      (DestroyIcon h)
      (set-box! owned-box #f))))

(define (alloc-nid)
  (define n (ctype-sizeof _NOTIFYICONDATA))
  (define p (cast (malloc n 'atomic-interior)
                  _pointer
                  _NOTIFYICONDATA-pointer))
  (memset p 0 n)
  (set-NOTIFYICONDATA-cbSize! p n)
  p)

(define (fill-nid! nid hwnd uid hicon tip)
  (set-NOTIFYICONDATA-cbSize! nid (ctype-sizeof _NOTIFYICONDATA))
  (set-NOTIFYICONDATA-hWnd! nid hwnd)
  (set-NOTIFYICONDATA-uID! nid uid)
  (set-NOTIFYICONDATA-uFlags! nid NIF_FLAGS)
  (set-NOTIFYICONDATA-uCallbackMessage! nid WM_TRAY)
  (set-NOTIFYICONDATA-hIcon! nid hicon)
  (write-utf16-z! (NOTIFYICONDATA-szTip nid) TRAY_TIP_LEN tip))

(define (normalize-icon v)
  (cond
    [(symbol? v)
     (define p (assoc v sym-icon))
     (if p (cdr p) 32516)]
    [else v]))

(define (icon->hicon v)
  (cond
    [(system-icon-id? v) (LoadIconW #f v)]
    [(as-pointer v)]
    [else (LoadIconW #f 32516)]))

(define (small-icon-size)
  (define cx (GetSystemMetrics SM_CXSMICON))
  (define cy (GetSystemMetrics SM_CYSMICON))
  (values (if (zero? cx) 16 cx)
          (if (zero? cy) 16 cy)))

(define (make-icon path)
  (define-values (cx cy) (small-icon-size))
  (define h (LoadImageW #f path IMAGE_ICON cx cy LR_LOADFROMFILE))
  (unless h
    (error 'make-icon "failed to load icon\n  path: ~e" path))
  h)

(define tray%
  (class object%
    (init-field
     [label ""]
     [icon 32516]
     [callback (λ (snd ev) (void))])
    (super-new)
    (define current-callback callback)
    (define show-flag #f)
    (define icon-key (normalize-icon icon))
    (define owned-box (box #f))
    (define hicon (icon->hicon icon-key))
    (unless (system-icon-id? icon-key)
      (set-box! owned-box hicon))
    (define uid next-uid)
    (set! next-uid (add1 next-uid))
    (define nid (alloc-nid))
    (define hwnd (ensure-message-window))
    (define eventspace (current-eventspace))
    (define (on-lparam lp)
      (define ev (lparam->mouse-event lp))
      (when ev (current-callback this ev)))
    (hash-set! id-handlers uid (vector eventspace (make-weak-box on-lparam)))
    (define (refresh-nid)
      (fill-nid! nid hwnd uid hicon (label->string label)))
    (define (notify cmd)
      (Shell_NotifyIconW cmd nid))
    (define (adopt-icon v)
      (define old (unbox owned-box))
      (set! icon-key (normalize-icon v))
      (set! hicon (icon->hicon icon-key))
      (define new-owned
        (if (system-icon-id? icon-key) #f hicon))
      (set-box! owned-box new-owned)
      (when (and old (not (eq? old hicon)))
        (DestroyIcon old)))
    (refresh-nid)
    (register-finalizer this (make-tray-finalizer nid uid owned-box))
    (define/public (show x)
      (define want (and x #t))
      (refresh-nid)
      (cond
        [(eq? want show-flag)
         (when want (notify NIM_MODIFY))]
        [want
         (set! show-flag #t)
         (unless (notify NIM_ADD)
           (notify NIM_MODIFY))]
        [else
         (notify NIM_DELETE)
         (set! show-flag #f)]))
    (define/public (is-shown?) show-flag)
    (define/public (get-icon)
      (or (hash-ref id->sym icon-key #f) icon-key))
    (define/public (set-icon ic)
      (adopt-icon ic)
      (refresh-nid)
      (when show-flag (notify NIM_MODIFY)))
    (define/public (get-label) label)
    (define/public (set-label lb)
      (set! label lb)
      (refresh-nid)
      (when show-flag (notify NIM_MODIFY)))
    (define/public (new-callback x)
      (set! current-callback x))))
