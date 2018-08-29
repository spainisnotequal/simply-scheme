;; ========================== ;;
;; Chapter 10 - Simply Scheme ;;
;; ========================== ;;


(load "../programs/ttt.scm")

(ttt '____x____ 'o)
(ttt 'o__xx____ 'o)
(ttt 'o_xxxo___ 'o)
(ttt 'o_xxxoox_ 'o)


"----- Exercise 10.1 -----"

(define (triple-appearances letter position)
  (every (lambda (triple) (appearances letter triple)) position))

(define (already-won? position letter)
  (if (empty? (keep (lambda (wd)
                      (if (= wd 3)
                          #t
                          #f))
                    (triple-appearances letter position)))
      #f
      #t))


(define pos1 (find-triples 'o_oox_xxx)) ; 'x wins
(define pos2 (find-triples 'x_xxo_ooo)) ; 'o wins
(define pos3 (find-triples 'o_oox_xxo)) ; nobody wins (yet)

(already-won? pos1 'x)
(not (already-won? pos1 'o))

(not (already-won? pos2 'x))
(already-won? pos2 'o)

(not (already-won? pos3 'x))
(not (already-won? pos3 'o))


"----- Exercise 10.2 -----"

(define (already-filled? position)
  (if (empty? (keep number? (accumulate word position)))
      #t
      #f))

(define (tie-game? position)
  (if (and (not (already-won? position 'x))
           (not (already-won? position 'o))
           (already-filled? position))
      #t
      #f))


(define pos4 (find-triples 'oxooxxxox)) ;already filled and ended in a tie

(not (already-filled? pos1))
(already-filled? pos4)

(not (tie-game? pos1))
(not (tie-game? pos2))
(not (tie-game? pos3))
(tie-game? pos4)