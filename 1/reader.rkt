#lang racket
(require syntax/readerr syntax/srcloc syntax/parse racket/list)
(provide my-read my-read-syntax)

(define (my-read [p (current-input-port)]) (syntax->datum (my-read-syntax #f p)))

(define (my-read-syntax [source-name (object-name (current-input-port))]
                        [port (current-input-port)])
  (with-handlers ([(λ (e) #t) (λ (e) (raise (translate e)))])
    (parameterize ([current-source-name source-name]
                   [current-input-port port])
      (indent-read))))

(define (translate e)
  (define dict
    '(("unexpected" . "неожиданно встретилась")
      ("expected" . "ожидалась")
      ("to close preceding" . "для закрытия предшествующей")
      ("found instead" . "вместо этого встретилась")
      ("read-syntax" . "чтение-синтаксиса")))
  (define (replace-dict str dict)
    (if (null? dict)
        str
        (replace-dict (string-replace str (caar dict) (cdar dict)) (cdr dict))))
  (cond
    [(exn:fail:syntax:unbound? e)
     (exn:fail:syntax:unbound (exn-message e) (exn-continuation-marks e) (exn:fail:syntax-exprs e))]
    [(exn:fail:read? e) (exn:fail:read
                         (replace-dict (exn-message e) dict)
                         (exn-continuation-marks e) (exn:fail:read-srclocs e))]
    [else e]))

(define (rt-char=? c default-c [c=? char=?])
  (define-values (c2 _1 _2)
    (if (char? c)
        (let ([r (current-readtable)])
          (if r (readtable-mapping r c) (values c #f #f)))
        (values #f #f #f)))
  (and (char? c2) (c=? c2 default-c)))

(define (accumulate-hspace)
  (define c (peek-char-or-special))
  (if (and (char? c)
           (or (char-whitespace? c) (char=? c #\!))
           (not (eqv? c #\newline)))
      (cons (read-char)
            (accumulate-hspace))
      null))

(define (comment? c)
  (and (rt-char=? c #\-) (rt-char=? (peek-char-or-special (current-input-port) 1) #\-)))

(define (block-comment? c)
  (and (rt-char=? c #\#) (rt-char=? (peek-char-or-special (current-input-port) 1) #\|)))

(define (indentationlevel)
  (define indent (accumulate-hspace))
  (define c (peek-char-or-special))
  (cond [(eof-object? c) ""]
        [(comment? c)
         (consume-to-eol!)
         (indentationlevel)]
        [(eqv? c #\newline)
         (read-char)
         (indentationlevel)]
        [else
         (when (rt-char=? c #\;) (read-char))
         (list->string indent)]))

(define (indentation>? indentation1 indentation2)
  (define len1 (string-length indentation1))
  (define len2 (string-length indentation2))
  (and (> len1 len2)
       (string=? indentation2 (substring indentation1 0 len2))))

(define (consume-to-eol!)
  (define c (read-char-or-special))
  (unless (or (eof-object? c)
              (rt-char=? c #\newline))
    (consume-to-eol!)))

(define (consume-block-comment!)
  (define (rec)
    (define c (peek-char-or-special))
    (unless (and (rt-char=? c #\|) (rt-char=? (peek-char-or-special (current-input-port) 1) #\#))
      (when (block-comment? c)
        (consume-block-comment!))
      (read-char-or-special)
      (rec)))
  (read-char) (read-char) (rec) (read-char) (read-char))
     

(define (consume-whitespaces!)
  (define c (peek-char-or-special))
  (cond
    [(and (char? c) (char-whitespace? c))
     (read-char)
     (consume-whitespaces!)]
    [(comment? c)
     (consume-to-eol!)
     (consume-whitespaces!)]
    [(block-comment? c)
     (consume-block-comment!)
     (consume-whitespaces!)]))

(define (indent-read)
  (define indentation (list->string (accumulate-hspace)))
  (define c (peek-char-or-special))
  (cond    
    [(eof-object? c) (read-char) c]
    [(comment? c) (consume-to-eol!) (indent-read)]
    [(rt-char=? c #\newline) (read-char) (indent-read)]
    [(> (string-length indentation) 0)
     (define-values (ln col pos) (port-next-location (current-input-port)))
     (raise-read-error "Выражения верхнего уровня должны начнинаться с начала строки"
                       (current-source-name) ln 0 (- pos col) col)]
    [else
     (define-values (ln col pos) (port-next-location (current-input-port)))
     (match-define (cons level stx) (read-block-clean ""))
     (if (dot? stx)
         (raise-read-error "неожиданная `.`" (current-source-name) ln col pos 1)
         stx)]))

(define (read-block-clean level)
  (define-values (ln col pos) (port-next-location (current-input-port)))
  (match-define (cons next-level stx) (read-block level))
  (cons next-level
        (if (or (eof-object? stx) (null? stx))
            stx
            (match stx
                 [(list x) x]
                 [_
                  (define-values (_1 _2 end-pos) (port-next-location (current-input-port)))
                  (clean (datum->syntax #f stx
                                        (vector (current-source-name)
                                                ln col pos (- end-pos pos))
                                        (read-syntax #f (open-input-string "orig"))))]))))



(define (split-sc x)
  (define r null)
  (define c null)
  (let loop ([r null] [c null] [l (syntax-e x)])
    (cond
      [(null? l)
       (datum->syntax x
                      (map (λ (elem)
                             (define elem* (match elem
                                             [(list x) x]
                                             [_ elem]))
                             (clean (datum->syntax x elem*)))
                           (filter (λ (x) (not (null? x)))
                                   (reverse (cons c r)))))]
      [(not (pair? l)) (loop r (append c l) null)]
      [(eq? (syntax-e (car l)) '|;|) (loop (cons c r) null (cdr l))]
      [else (loop r (append c (list (car l))) (cdr l))])))

(define sym-= (datum->syntax #f '=))

(define (clean x)
  (syntax-parse x
    [(a (~datum =) b) #`(#,sym-= #,(clean #'a) #,(clean #'b))]
    [(b ... (kw:keyword c) d ...) (clean #'(b ... kw c d ...))]
    [(b ... (kw:keyword c ...) d ...) (clean #'(b ... kw (c ...) d ...))]
    [((a ...) (~datum =) b ...) #`(#,sym-= #,(clean #'(a ...)) b ...)]
    [(a (~datum =) . b) #`(#,sym-= a b)]
    [(a c ... (~datum =) . b) #`(#,sym-= (a c ...) . b)]
    [(a (~datum :=) b) #'(:= a b)]
    [(a (~datum :=) . b) #`(:= a #,(clean #'b))]
    [(a ... (~datum :=) b) #'(:= (a ...) b)]
    [(a ... (~datum :=) . b) #`(:= (a ...) #,(clean #'b))]
    [(a ... b (~datum |.|) c (~datum |.|) d e ...) #'(c a ... b d e ...)]
    [(a ... b (~datum |.|) c . d) #'(c a ... b d)]
    [(a ... (~and dot (~datum |.|)) . b)
     (apply raise-read-error "неожиданная `.`" (build-source-location-list #'dot))]
    [((~and q
            (~or (~datum quote)
                 (~datum unquote)
                 (~datum quasiquote)
                 (~datum unquote-splicing)))
      b c d ...)
     #`(q #,(clean #'(b c d ...)))]
    [(a (~datum ||) b) #`(|| a b)]
    [(a (~datum ||) . b) #`(|| a #,(clean #'b))]
    [(a ... (~datum ||) b) #`(|| #,(clean (datum->syntax x (syntax->datum #'(a ...)))) b)]
    [(a ... (~datum ||) . b) #`(|| #,(clean (datum->syntax x (syntax->datum #'(a ...))))
                                   #,(clean #'b))]
    [(a (~datum &&) b) #'(&& a b)]
    [(a (~datum &&) . b) #'(&& a #,(clean #'b))]
    [(a ... (~datum &&) b) #`(&& #,(clean (fix #'(a ...))) b)]
    [(a ... (~datum &&) . b) #`(&& #,(clean (fix #'(a ...)))
                                   #,(clean #'b))]
    [_ x]))

(define (fix l)
  (define orig (car (syntax-e l)))
  (define last-orig (last (syntax-e l)))
  (define srcloc
    (list (syntax-source orig)
          (syntax-line orig)
          (syntax-column orig)
          (syntax-position orig)
          (and
           (syntax-position last-orig)
           (syntax-position orig)
           (syntax-span last-orig)
           (+ (- (syntax-position last-orig)
                 (syntax-position orig))
              (syntax-span last-orig)))))
  (datum->syntax orig (syntax->datum l) srcloc orig))

(define (clean-list x)
  (syntax-parse x
    [(a ... (~datum |;|) . b) (clean (split-sc x))]
    [_ (clean x)]))

(define current-source-name (make-parameter #f))

(define (parse-dot first rest ln col pos)
  (match rest
    [(list x) x]
    [(list) (raise-read-error "неожиданная `.`" (current-source-name) ln col pos 1)]
    [(list-rest a ... b) (cons first rest)]))

(define (read-block level)
  (define char (peek-char-or-special))
  (cond
    [(eof-object? char)
     (read-char)
     (cons -1 null)]
    [(comment? char)
     (consume-to-eol!)
     (read-block level)]
    [(block-comment? char)
     (consume-block-comment!)
     (read-block level)]
    [(and (rt-char=? char #\\)
          (rt-char=? (peek-char-or-special (current-input-port) 1) #\newline))
     (read-char) (read-char) (read-block level)]
    [(rt-char=? char #\newline)
     (read-char)     
     (define next-level (indentationlevel))
     (if (indentation>? next-level level)
         (read-blocks next-level)
         (cons next-level null))]
    [(rt-char=? char #\;)
     (read-char)
     (consume-whitespaces!)
     (cons level null)]
    [(and (char? char) (char-whitespace? char))
     (read-char)
     (read-block level)]
    [else
     (define-values (ln col pos) (port-next-location (current-input-port)))
     (define first (read-item))
     (match-define (cons new-level rest) (read-block level))
     (define-values (_1 _2 end-pos) (port-next-location (current-input-port)))
     (cons new-level
           (cond
             [(eof-object? first) first]
             [(eof-object? rest) first]
             [(dot? first) (parse-dot first rest ln col pos)]
             [($? first)
              (match rest                
                [(list a b c ...) (list rest)]
                [_ rest])]
             [else
              (cons first rest)]))]))

(define (readquote qt)
  (define char (peek-char-or-special))
  (define-values (ln col pos) (port-next-location (current-input-port)))  
  (define stx (if (char-whitespace? char) qt (list qt (read-item))))
  (define-values (_1 _2 end-pos) (port-next-location (current-input-port)))
  (datum->syntax #f stx
                 (vector (current-source-name)
                         ln col pos (- end-pos pos))
                 (read-syntax #f (open-input-string "orig"))))

(define (read-list end)
  (consume-whitespaces!)
  (define char (peek-char-or-special))
  (define-values (ln col pos) (port-next-location (current-input-port)))
  (cond
    [(eof-object? char)
     (raise-read-eof-error "файл закончился внутри списка" (current-source-name) ln col pos 1)]
    [(rt-char=? char end)
     (read-char)
     null]
    [(rt-char=? char #\.)
     (parse-dot (read-item) (read-list end) ln col pos)]
    [(rt-char=? char #\$)
     (define first (read-item))
     (define rest (read-list end))
     (match rest
       [(list a b c ...) (list rest)]
       [_ rest])]
    [else
     (cons (read-item) (read-list end))]))

(define (read-item)
  (define char (peek-char-or-special))
  (define-values (ln col pos) (port-next-location (current-input-port)))
  (cond [(eof-object? char) char]
        [(rt-char=? char #\.)
         (read-char)
         (datum->syntax #f '|.| (vector (current-source-name) ln col pos 1))]
        [(rt-char=? char #\;)
         (read-char)
         (datum->syntax #f '|;| (vector (current-source-name) ln col pos 1))]
        [(rt-char=? char #\`)
         (read-char)
         (readquote 'quasiquote)]
        [(rt-char=? char #\')
         (read-char)
         (readquote 'quote)]
        [(rt-char=? char #\,)
         (read-char)
         (cond
           [(eqv? (peek-char-or-special) #\@)
            (read-char)
            (readquote 'unquote-splicing)]
           [else (readquote 'unquote)])]
        [else
         (define res
           (cond
             [(rt-char=? char #\()
              (read-char)
              (clean-list (datum->syntax #f (read-list #\))))]
             [(rt-char=? char #\[)
              (read-char)
              (clean-list (datum->syntax #f (read-list #\])))]
             [(rt-char=? char #\{)
              (read-char)
              (clean-list (datum->syntax #f (read-list #\})))]
             [else (read-syntax)]))
         (let loop ([res res])
         (cond
           [(rt-char=? (peek-char-or-special) #\()
            (read-char)
            (loop (datum->syntax #f (cons res (clean-list (datum->syntax #f (read-list #\)))))))]
           [(rt-char=? (peek-char-or-special) #\[)
            (read-char)
            (loop (datum->syntax #f (cons 'bracket (cons res (read-list #\])))))]
           [(rt-char=? (peek-char-or-special) #\{)
            (read-char)
            (define l (clean-list (datum->syntax #f (read-list #\}))))
            (loop (datum->syntax #f
                                 (cons (if (cons? (car (syntax->datum l))) 'send+ 'send)
                                       (cons res l))))]
           [else res]))]))

(define (parse-block-dot stx [next-blocks null])
  (cond
    [(null? stx) null]
    [(dot? stx) next-blocks]
    [else (cons stx next-blocks)]))

(define (read-blocks level)
  ;; indent -> (listof syntax?)
  (match-define (cons next-level stx) (read-block-clean level))
  (cond [(equal? next-level level)
         (match-define (cons next-next-level next-blocks) (read-blocks level))
         (cons next-next-level (parse-block-dot stx next-blocks))]
        [else
         (cons next-level (parse-block-dot stx null))]))

(define (dot? x) (and (syntax? x) (eq? (syntax-e x) '|.|)))
(define ($? x) (and (syntax? x) (eq? (syntax-e x) '$)))

(module+ test
  (require rackunit)
  (define (test a b)
    (check-equal? (with-input-from-string a my-read) b))
  (test "(1 2 . 3)" '(1 2 . 3))
  (test "1 2 . 3" '(1 2 . 3))
  (check-equal? (with-input-from-string "1 2; 3" (λ () (list (my-read) (my-read)))) '((1 2) 3))
  (check-equal? (with-input-from-string "1 2; 3 4" (λ () (list (my-read) (my-read)))) '((1 2) (3 4)))
  (test "1 2\n 3 4;  \n \n    5 6\n  7 8"
                '(1 2 (3 4) (5 6 (7 8))))
  (test "(2 #|23 32|# . 3)"  '(2 . 3))
  (test "2 3 -- sadasd  sad as\n 4 5"  '(2 3 4 5))
  (test "f(a) f(g(d))" '((f a) (f (g d))))
  (test "f(a; b c; d)" '(f a (b c) d))
  (test "цикл/первый\n ;\n  p points\n  #:когда tau < p[0]\n bonus := bonus + p[1]"
        '(цикл/первый ((p points) #:когда (tau < (bracket p 0))) (:= bonus (bonus + (bracket p 1)))))
  (test "цикл/первый (p points; #:когда tau < p[0])\n bonus := bonus + p[2]"
        '(цикл/первый ((p points) #:когда (tau < (bracket p 0))) (:= bonus (bonus + (bracket p 2)))))
  (test "new(point%){move-x 5; move-y 7; move-x 12}"
        '(send+ (new point%) (move-x 5) (move-y 7) (move-x 12)))
  (test "new(point%){move-x 5}"
        '(send (new point%) move-x 5)))
