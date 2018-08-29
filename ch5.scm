;; ========================= ;;
;; Chapter 5 - Simply Scheme ;;
;; ========================= ;;

;; Exercise 5.2
;; ------------

(define (f1 s1 s2)
  (se (bf s1) (bl s2)))

(define (f2 s1 s2)
  (se (bf s1) (bl s2) (word (first s1) (last s2))))

(define (f3 s1 s2)
  (se s1 s1))

(define (f4 s1 s2)
  (word (item 2 s1) (item 2 s2)))

(equal? '(b c d e) (f1 '(a b c) '(d e f)))
(equal? '(b c d e af) (f2 '(a b c) '(d e f)))
(equal? '(a b c a b c) (f3 '(a b c) '(d e f)))
(equal? 'be (f4 '(a b c) '(d e f)))


;; Exercise 5.14
;; -------------

(define (third s)
  (first (bf (bf s))))

(equal? 'c (third 'abcd))
(equal? 'c (third '(a b c d)))


;; Exercise 5.15
;; -------------

(define (first-two w)
  (word (first w) (first (bf w))))

(equal? 'am (first-two 'ambulatory))

;; Exercise 5.16
;; -------------

(define (two-first w1 w2)
  (word (first w1) (first w2)))

(define (two-first-sent s)
  (word (first (first s))
        (first (last s))))

(equal? 'be (two-first 'brian 'epstein))
(equal? 'be (two-first-sent '(brian epstein)))


;; Exercise 5.19
;; -------------

(define (insert-and s)
  (se (bl s) 'and (last s)))

(equal? '(maria and jose) (insert-and '(maria jose)))



;; Exercise 5.20
;; -------------

(define (middle-names s)
  (se (bl (bf s))))

(equal? '(maria) (middle-names '(jose maria garcia)))
(equal? '(maria anunciacion) (middle-names '(jose maria anunciacion garcia)))
(equal? '() (middle-names '(jose garcia)))
(equal? '(maria anunciacion de los santos) (middle-names '(jose maria anunciacion de los santos garcia)))


;; Exercise 5.21
;; -------------

(define (query s)
  (se (item 2 s) (item 1 s) (bl (bf (bf s))) (word (last (bf (bf s))) '?)))

(equal? '(are you stupid?) (query '(you are stupid)))
(equal? '(should I have known better?) (query '(I should have known better)))






















