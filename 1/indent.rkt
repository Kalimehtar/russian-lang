#lang racket/base

;; This module provides an indenter for the sweet-exp language.
;; Within a normal s-expression, it uses normal s-expression indentation rules.
;; Otherwise, it leaves code indented the way it was, and indents blank lines
;; and comments to match the indentation of the next expression.

(provide indent)

(require racket/class)

;; indent :
;; (is-a?/c racket:text<%>) exact-nonnegative-integer? -> (or/c #f exact-nonnegative-integer?)
;; This function is used to indent lines in DrRacket. It is called with the position containing
;; the line to be indented. It is expected to return the number of spaces that should appear
;; at the beginning of the line or #f. If #f is returned, DrRacket uses the standard s-expression
;; indentation rules.
(define (indent text pos)
  (if (within-sexp-not-first-line? text pos)
      #f ; use normal racket s-expression indentation
      (get-indentation text pos)))

(define (get-indentation text pos)
  (define line-start (send text get-start-of-line pos))
  (if (> line-start 0)
      (let ([pos (sub1 line-start)])
        (- (send text get-start-of-line pos)
           (send text line-start-position (send text position-line pos))))
      0))

(define (line=? text pos1 pos2)
  (= (send text get-start-of-line pos1)
     (send text get-start-of-line pos2)))

(define (find-up-sexp text pos)
  (and pos (send text find-up-sexp pos)))

(define (within-sexp-not-first-line? text pos)
  (define up-sexp
    (find-up-sexp text (send text get-start-of-line pos)))
  (cond [(not up-sexp) #f]
        [(line=? text up-sexp pos)
         (within-sexp-not-first-line? text up-sexp)]
        [else #t]))

