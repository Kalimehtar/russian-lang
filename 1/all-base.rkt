#lang racket/base
(require 1/lang)
(provide (all-from-out 1/lang) (all-defined-out))
;; имена для формирования документации
;; нельзя просто импортировать базовая.1, так как на сайти тогда не сформируется

(define-syntax-rule (defnames name ...) (begin (define name #f) ...))

(defnames список список? пара пара? длина-строки подстрока логический?)