#lang racket/base
;; C++ compile + per-module C++ registry for надо-быстро.
(require ffi/unsafe
         racket/file
         racket/system
         racket/string
         racket/path
         file/sha1)

(provide start end
         fast-lib-box
         get-ffi-obj-from-fast
         compile-cpp-to-shared
         fast-runtime-so-paths
         reset-fast-state!
         add-cpp-fragment!
         add-fast-export!
         take-cpp-source
         take-fast-exports
         fresh-generated-name
         shared-lib-suffix
         find-g++
         register-module-cpp!
         lookup-module-cpp
         current-fast-module-key
         set-current-fast-module-key!
         fast-runtime-path-emitted?)

(define fast-lib-box (box #f))

(define-syntax-rule (get-ffi-obj-from-fast sym type)
  (get-ffi-obj sym (unbox fast-lib-box) type))

(define cpp-fragments (box '()))
(define fast-exports (box '()))
(define name-counter (box 0))
;; Persistent across modules: abs-path-string → cpp body (no shared preamble)
(define module-cpp-registry (make-hash))
(define current-module-key (box #f))
;; One define-runtime-path per expanding module
(define fast-runtime-path-emitted? (box #f))

(define (reset-fast-state!)
  (set-box! cpp-fragments '())
  (set-box! fast-exports '())
  (set-box! name-counter 0)
  (set-box! current-module-key #f)
  (set-box! fast-runtime-path-emitted? #f))

(define (current-fast-module-key)
  (unbox current-module-key))

(define (set-current-fast-module-key! key)
  (set-box! current-module-key key))

(define (add-cpp-fragment! s)
  (set-box! cpp-fragments
            (append (unbox cpp-fragments) (list s))))

(define (add-fast-export! export)
  (set-box! fast-exports
            (append (unbox fast-exports) (list export))))

(define (take-cpp-source)
  (string-append* (unbox cpp-fragments)))

(define (take-fast-exports)
  (unbox fast-exports))

(define (fresh-generated-name)
  (set-box! name-counter (add1 (unbox name-counter)))
  (format "generated_name~a" (unbox name-counter)))

(define (register-module-cpp! key cpp)
  (when (and key (string? key) (positive? (string-length cpp)))
    (hash-set! module-cpp-registry key cpp)))

(define (lookup-module-cpp key)
  (and key (hash-ref module-cpp-registry key #f)))

(define (shared-lib-suffix)
  (case (system-type 'os)
    [(windows) #".dll"]
    [(macosx) #".dylib"]
    [else #".so"]))

;; For module-key = absolute path to .1 → abs .so + relative path string.
;; ASCII-only file names (hash): Cyrillic breaks g++/ffi-lib on Windows.
;; Otherwise both #f.
(define (fast-runtime-so-paths module-key)
  (cond
    [(and (path-string? module-key)
          (absolute-path? (string->path module-key)))
     (define src (simplify-path (string->path module-key) #f))
     (define dir (path-only src))
     (define digest
       (substring
        (sha1 (string->bytes/utf-8 (path->string src)))
        0 16))
     (define so-name
       (path-replace-suffix
        (string->path
         (string-append "adina-fast-" digest))
        (shared-lib-suffix)))
     (define rel
       (build-path "compiled" "native"
                   (system-library-subpath #f)
                   so-name))
     (values (build-path dir rel) (path->string rel))]
    [else (values #f #f)]))

(define (find-g++ #:who [who 'надо-быстро])
  (define p (find-executable-path "g++"))
  (unless p
    (raise-syntax-error
     who
     (string-append
      "не найден компилятор g++. "
      "Установите MinGW/GCC и добавьте g++ в PATH")))
  p)

(define (compile-cpp-to-shared cpp-source
                               #:output-so [output-so #f]
                               #:who [who 'надо-быстро])
  (define g++ (find-g++ #:who who))
  (define base (make-temporary-file "adina-fast~a"))
  (define cpp-path (path-replace-suffix base #".cpp"))
  (define o-path (path-replace-suffix base #".o"))
  (define so-path
    (cond
      [output-so
       (define p
         (if (path? output-so)
             output-so
             (string->path output-so)))
       (make-parent-directory* p)
       p]
      [else
       (path-replace-suffix base (shared-lib-suffix))]))
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
