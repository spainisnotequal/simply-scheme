;; ============================================= ;;
;; Project: Scoring Bridge Hands - Simply Scheme ;;
;; ============================================= ;;


(define (card-val card)
  (cond ((number? (bf card)) 0)
        ((equal? (bf card) 'a) 4)
        ((equal? (bf card) 'k) 3)
        ((equal? (bf card) 'q) 2)
        ((equal? (bf card) 'j) 1)))

(define (high-card-points hand)
  (accumulate + (every card-val hand)))

(define (count-suit suit hand)
  (accumulate + (every (lambda (card) (if (equal? suit (first card)) 1 0)) hand)))

(define (suit-counts hand)
  (every (lambda (suit) (count-suit suit hand)) '(s h c d)))

(define (suit-dist-points x)
  (cond ((= x 0) 3)
        ((= x 1) 2)
        ((= x 2) 1)
        (else 0)))

(define (hand-dist-points hand)
  (accumulate + (every suit-dist-points (suit-counts hand))))

(define (bridge-val hand)
  (+ (high-card-points hand) (hand-dist-points hand)))




(= 2 (card-val 'cq))
(= 0 (card-val 's7))
(= 4 (card-val 'ha))

(= 9 (high-card-points '(sa s10 hq ck c4)))
(= 13 (high-card-points '(sa s10 s7 s6 hq hj h9 ck c4 dk d9 d3)))

(= 2 (count-suit 's '(sa s10 hq ck c4)))
(= 2 (count-suit 'c '(sa s10 s7 s6 s2 hq hj h9 ck c4 dk d9 d3)))
(= 5 (count-suit 'd '(h3 d7 sk s3 c10 dq d8 s9 s4 d10 c7 d4 s2)))

(equal? '(2 1 2 0) (suit-counts '(sa s10 hq ck c4)))
(equal? '(5 3 2 3) (suit-counts '(sa s10 s7 s6 s2 hq hj h9 ck c4 dk d9 d3)))
(equal? '(5 1 2 5) (suit-counts '(h3 d7 sk s3 c10 dq d8 s9 s4 d10 c7 d4 s2)))

(= 1 (suit-dist-points 2))
(= 0 (suit-dist-points 7))
(= 3 (suit-dist-points 0))

(= 1 (hand-dist-points '(sa s10 s7 s6 s2 hq hj h9 ck c4 dk d9 d3)))
(= 3 (hand-dist-points '(h3 d7 sk s3 c10 dq d8 s9 s4 d10 c7 d4 s2)))

(= 14 (bridge-val '(sa s10 s7 s6 s2 hq hj h9 ck c4 dk d9 d3)))
(= 8 (bridge-val '(h3 d7 sk s3 c10 dq d8 s9 s4 d10 c7 d4 s2)))
