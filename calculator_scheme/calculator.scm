#lang racket
(require racket/string)

(define (tokenize input)
  (define operators '(#\+ #\- #\* #\/ #\( #\)))
  (define (is-operator? ch)
    (member ch operators))
  
  (define (split-helper chars current tokens)
    (cond
      ((null? chars) (reverse (if (string=? current "") tokens (cons current tokens))))
      ((is-operator? (car chars))
       (split-helper (cdr chars) "" (cons (string (car chars)) (if (string=? current "") tokens (cons current tokens)))))
      (else
       (split-helper (cdr chars) (string-append current (string (car chars))) tokens))))
  
  (split-helper (string->list input) "" '()))

(define (to-postfix tokens)
  (define precedence (hash '+ 1 '- 1 '* 2 '/ 2))
  (define (higher-precedence? op1 op2)
    (>= (hash-ref precedence op1 0) (hash-ref precedence op2 0)))
  
  (define (shunting-yard tokens output stack)
    (cond
      ((null? tokens) (append output (reverse stack)))
      ((number? (string->number (car tokens))) ;; If number
       (shunting-yard (cdr tokens) (append output (list (string->number (car tokens)))) stack))
      ((equal? (car tokens) "(") ;; If left paranthesis
       (shunting-yard (cdr tokens) output (cons "(" stack)))
      ((equal? (car tokens) ")") ;; If right paranthesis
       (let loop ((s stack) (o output))
         (if (or (null? s) (equal? (car s) "("))
             (shunting-yard (cdr tokens) o (cdr s))
             (loop (cdr s) (append o (list (car s))))))
       )
      (else ;; Operatör ise
       (let loop ((s stack) (o output))
         (if (or (null? s) (equal? (car s) "(") (not (higher-precedence? (string->symbol (car tokens)) (string->symbol (car s)))))
             (shunting-yard (cdr tokens) o (cons (string->symbol (car tokens)) s))
             (loop (cdr s) (append o (list (car s)))))))))
  
  (shunting-yard tokens '() '()))

(define (evaluate-postfix postfix)
  (define (eval-helper stack tokens)
    (cond
      ((null? tokens) (car stack))
      (else
       (let ((token (car tokens)))
         (if (number? token)
             (eval-helper (cons token stack) (cdr tokens))
             (let ((b (car stack))
                   (a (cadr stack)))
               (cond
                 ((eq? token '+) (eval-helper (cons (+ a b) (cddr stack)) (cdr tokens)))
                 ((eq? token '-) (eval-helper (cons (- a b) (cddr stack)) (cdr tokens)))
                 ((eq? token '*) (eval-helper (cons (* a b) (cddr stack)) (cdr tokens)))
                 ((eq? token '/) (if (= b 0)
                                     (error "Error: Division by zero!")
                                     (eval-helper (cons (/ a b) (cddr stack)) (cdr tokens))))
                 (else (error "Hata: Invalid operator!")))))))))
  
  (eval-helper '() postfix))

(define (eval-expression input)
  (let ((tokens (tokenize (string-trim input))))
    (evaluate-postfix (to-postfix tokens))))

(define (calculator)
  (display "Scheme Calculator\n")
  (let loop ()
    (display "> ")
    (let ((input (read-line)))
      (cond
        ((or (string=? input "quit") (string=? input "exit"))
         (display "Çıkış yapılıyor.\n"))
        ((string=? (string-trim input) "")
         (display "Sonuç: 0\n")
         (loop))
        (else
         (let ((result (eval-expression input)))
           (display "Sonuç: ")
           (display result)
           (newline)
           (loop)))))))

;; Başlat
(calculator)