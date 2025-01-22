#lang racket
(require racket/include)
(require racket/list)
(require graphics/graphics)
(open-graphics)


;CONSTANTS
(define red-ai? #t) ;Determins if red is ai or player controlled
(define blue-ai? #t) ;Determins if blue is ai or player controlled
(define ai-depth 3) ;Determins the depth of the minimax algorithm
(define alpha-beta #t) ;Determins if the algorithm used is minimax with or without pruning

(define gen-size 10) ;Amount of chromosomes in a generation
(define gene-range 5) ;Range of values per gene (5 - 0-4)
(define mut-rate 10) ;1 out of how many chromosomes mutates


;Side variable are equal to either red or blue.
;Determines which side a piece belongs to.
(define blue -1)
(define red 1)

;Receives a side, returns the equivilant ai boolean.
(define (ai? side)
  (cond
    ((= side red) red-ai?)
    (else blue-ai?)))

;Structure for storing all the information of a given game state.
(define-struct state (board cards side))

;Returns a starting state.
;This is a function and not a constant so that every time it would generate a new card set.
(define (starting) (make-state board (gen-cards) blue))

;Returns true if game is won by the previous play.
(define (win? state) 
  (or (not (contains-piece? (state-board state) (make-piece king (state-side state))))
      (equal-piece? (matrix-ref (state-board state) (make-posn 2 (+ 2 (* -2 (state-side state)))))  (make-piece king (- (state-side state)) ))))


(include "Board.rkt")
(include "Draw.rkt")
(include "Screens.rkt")
(include "Cards.rkt")
(include "Functions.rkt")
(include "Minimax.rkt")
(include "Genetic.rkt")
(include "Game.rkt")


;Starts program at menu
(menu)
