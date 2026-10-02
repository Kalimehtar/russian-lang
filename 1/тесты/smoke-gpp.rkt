#lang racket/base
(require (for-syntax racket/base 1/run-fast))
(require ffi/unsafe 1/run-fast)
(define-syntax (load-smoke stx)
  (define cpp (string-append
    "#ifdef _WIN32\n"
    "#define ADINA_EXPORT extern \"C\" __declspec(dllexport)\n"
    "#else\n"
    "#define ADINA_EXPORT extern \"C\"\n"
    "#endif\n"
    "ADINA_EXPORT int f() { return 42; }\n"))
  (define so (compile-cpp-to-shared cpp #:who 'smoke))
  (with-syntax ([so so])
    #'(begin
        (define lib (ffi-lib so))
        (define f (get-ffi-obj 'f lib (_cprocedure '() _int)))
        (displayln (f)))))
(load-smoke)
