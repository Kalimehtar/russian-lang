#lang racket/base
;; C++ compile for надо-быстро (superc-like, g++).
;; Require at phase 0 for runtime FFI box; for-syntax for compile helpers.
(require ffi/unsafe
         racket/file
         racket/system
         racket/string)

(provide start end
         fast-lib-box
         get-ffi-obj-from-fast
         compile-cpp-to-shared
         reset-fast-state!
         add-cpp-fragment!
         add-fast-export!
         take-cpp-source
         take-fast-exports
         fresh-generated-name
         shared-lib-suffix
         find-g++)

;; ---------------------------------------------------------------------------
;; Runtime library box
;; ---------------------------------------------------------------------------

(define fast-lib-box (box #f))

(define-syntax-rule (get-ffi-obj-from-fast sym type)
  (get-ffi-obj sym (unbox fast-lib-box) type))

;; ---------------------------------------------------------------------------
;; Expand-time state (phase 0 of this module = phase 1 when required for-syntax)
;; ---------------------------------------------------------------------------

(define cpp-fragments (box '()))
(define fast-exports (box '()))
(define name-counter (box 0))

(define (reset-fast-state!)
  (set-box! cpp-fragments '())
  (set-box! fast-exports '())
  (set-box! name-counter 0))

(define (add-cpp-fragment! s)
  (set-box! cpp-fragments (append (unbox cpp-fragments) (list s))))

(define (add-fast-export! export)
  ;; (list adina-id generated-name-string ret-key arg-keys)
  ;; ret-key / arg-keys: 'целое | 'вещественное | 'логическое
  (set-box! fast-exports (append (unbox fast-exports) (list export))))

(define (take-cpp-source)
  (string-append* (unbox cpp-fragments)))

(define (take-fast-exports)
  (unbox fast-exports))

(define (fresh-generated-name)
  (set-box! name-counter (add1 (unbox name-counter)))
  (format "generated_name~a" (unbox name-counter)))

(define (shared-lib-suffix)
  (case (system-type 'os)
    [(windows) #".dll"]
    [(macosx) #".dylib"]
    [else #".so"]))

(define (find-g++ #:who [who 'надо-быстро])
  (define p (find-executable-path "g++"))
  (unless p
    (raise-syntax-error
     who
     (string-append
      "не найден компилятор g++. "
      "Установите MinGW/GCC и добавьте g++ в PATH")))
  p)

(define (compile-cpp-to-shared cpp-source #:who [who 'надо-быстро])
  (define g++ (find-g++ #:who who))
  (define base (make-temporary-file "adina-fast~a"))
  (define cpp-path (path-replace-suffix base #".cpp"))
  (define o-path (path-replace-suffix base #".o"))
  (define so-path (path-replace-suffix base (shared-lib-suffix)))
  (with-output-to-file cpp-path
    (λ () (write-string cpp-source))
    #:exists 'replace)
  (define compile-ok?
    (zero?
     (system*/exit-code
      (path->string g++)
      "-std=c++17"
      "-fPIC"
      "-c"
      (path->string cpp-path)
      "-o"
      (path->string o-path))))
  (unless compile-ok?
    (raise-syntax-error
     who
     (format "ошибка компиляции C++: ~a" cpp-path)))
  (define link-ok?
    (zero?
     (system*/exit-code
      (path->string g++)
      "-shared"
      (path->string o-path)
      "-o"
      (path->string so-path))))
  (unless link-ok?
    (raise-syntax-error
     who
     (format "ошибка компоновки C++: ~a" o-path)))
  (path->string so-path))

(define (start _src)
  (reset-fast-state!))

(define (end)
  (void))
