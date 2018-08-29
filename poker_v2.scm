;; ============================================ ;;
;; Project: Scoring Poker Hands - Simply Scheme ;;
;; ============================================ ;;

;; Suit:
;; -----

(define (count-suit suit hand)
  (accumulate + (every (lambda (card) (if (equal? suit (first card)) 1 0)) hand)))

(define (suit-counts hand)
  (every (lambda (suit) (count-suit suit hand)) '(s h c d)))

(define (number2text n)
  (cond ((= n 0) 'none)
        ((= n 1) 'one)
        ((= n 2) 'two)
        ((= n 3) 'three)
        ((= n 4) 'four)
        ((= n 5) 'five)))

(define (combine sent1 sent2)
  (cond ((empty? sent1) sent2)
        ((empty? sent2) sent1)
        (else (se (first sent1) (first sent2)
                  (combine (bf sent1) (bf sent2))))))

(define (suit-counts-in-text hand)
  (every number2text (suit-counts hand)))

(define (compute-suits hand)
  (combine (suit-counts-in-text hand)
           '(s h c d)))

;; Rank:
;; -----

(define (count-rank rank hand)
  (accumulate + (every (lambda (card) (if (equal? rank (bf card)) 1 0)) hand)))

(define (rank-counts hand)
  (every (lambda (rank) (count-rank rank hand)) '(a k q j t 9 8 7 6 5 4 3 2 1)))

(define (rank-counts-in-text hand)
  (every number2text (rank-counts hand)))

(define (delete-none-ranks sent)
  (cond ((<= (count sent) 1) '())
        ((equal? (first sent) 'none) (delete-none-ranks (bf (bf sent))))
        (else (se (first sent) (first (bf sent)) (delete-none-ranks (bf (bf sent)))))))

(define (compute-ranks hand)
  (delete-none-ranks (combine (rank-counts-in-text hand)
                              '(a k q j t 9 8 7 6 5 4 3 2 1))))

(define (two? wd)
  (member? wd '(two)))

(define (rank hand)
  (let ((rank-list (compute-ranks hand)))
    (cond ((member? 'five rank-list) 'five-of-a-kind)
          ((member? 'four rank-list) 'four-of-a-kind)
          ((member? 'three rank-list) 'three-of-a-kind)
          ((= (count (keep two? rank-list)) 2) 'two-pairs)
          ((= (count (keep two? rank-list)) 1) 'pair)
          (else 'nothing))))

;; Tests:

(define h0 '(ha s3 c6 s1 ck)) ; all-ranks-different
(define h1 '(ha s3 c6 s1 ca)) ; pair
(define h2 '(ha s3 c1 s1 ca)) ; two-pairs
(define h3 '(ha s3 ca s1 ca)) ; three-of-a-kind
(define h4 '(ha s3 ca sa ca)) ; four-of-a-kind
(define h5 '(ha sa ca sa ca)) ; five-of-a-kind



(compute-ranks h0)
(compute-ranks h1)
(compute-ranks h2)
(compute-ranks h3)
(compute-ranks h4)
(compute-ranks h5)


(rank h0)
(rank h1)
(rank h2)
(rank h3)
(rank h4)
(rank h5)
