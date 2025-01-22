(define view '()) ;Screen
(define pix  (open-pixmap   "Onitamaaaaaaaaa" 950 800)) ;Virtual screen to prevent screen tearing

(define space 30) ; y space between the edge of the board and the cards
(define p2-y (- board-y square-size space)) ; y cord of the p2 cards
(define p1-y (+ board-y (* square-size 5) space)) ; y cords of the p1 cards
(define card2-x (+ board-x (* square-size 2) square-part)) ; x cord of both the right cards

;Returns the y cord of the equivilant card
(define (side-y side)
  (cond ((= side blue) p1-y)
        (else p2-y)))

;Draws a square with a thickness of n pixels
(define (draw-multi-square pos width height color n)
  (build-list n (lambda (n) ((draw-rectangle pix) (make-posn (+ n (posn-x pos)) (+ n (posn-y pos))) (- width (* 2 n)) (- height (* 2 n)) color))))

;Draws the board
(define (draw-board board)
  (for-each draw-square board (build-list 25 (lambda (n) (make-posn (+ board-x (* square-size (remainder n 5))) (+ board-y (* square-size (quotient n 5))))))))

;Draws a square and a piece at pos
(define (draw-square piece pos)
  ((draw-solid-rectangle pix) pos square-part square-part "white")
  (cond
    ((not (= empty-tile (piece-type piece))) ((draw-pixmap pix) (piece->pic (piece-type piece) (piece-side piece)) pos))))

;Returns corresponding pic
(define (piece->pic type side)
  (cond
    ((and (= type pawn) (= side red)) "Pieces/red_pawn.png")
    ((and (= type pawn) (= side blue)) "Pieces/blue_pawn.png")
    ((and (= type king) (= side red)) "Pieces/red_king.png")
    ((and (= type king) (= side blue)) "Pieces/blue_king.png")))

;Draws cards at cords
(define (draw-card card x y side)
  ((draw-solid-rectangle pix) (make-posn x y) (* square-size 2) square-size "white")
  ((draw-string pix) (make-posn (+ x 85) (+ y 15)) (card-name card) "black")
  (((draw-pixmap-posn (card-pic card) ) pix) (make-posn (+ x 15) (+ y 25)))
  (draw-moves (card-moves card) (+ x 120) (+ y 25) 14 side))

;Draws a set of cards
(define (draw-cards cards side)
  (draw-card (list-ref cards 0) board-x (side-y side) side)
  (draw-card (list-ref cards 1) card2-x (side-y side) side)
  (draw-card (list-ref cards 2) board-x (side-y (- side)) (- side))
  (draw-card (list-ref cards 3) card2-x (side-y (- side)) (- side))
  (draw-card (list-ref cards 4) 100 400 side))

;Draws all moves for a card
(define (draw-move-set moves x y size side)
  (for-each (lambda (move) ((draw-solid-rectangle pix) (make-posn (+ x (* (posn-x move) side -1 size))  (- y (* (posn-y move) side -1 size))) size size "black")) moves))

;Draws a move grid, a red square and a move set
(define (draw-moves moves x y size side)
  (define pos (build-list 25 (lambda (n) (make-posn (+ x (* size (quotient n 5))) (+ y (* size (remainder n 5)))))))
  (for-each (lambda (pos) ((draw-rectangle pix) pos size size "black")) pos)
  ((draw-solid-rectangle pix) (make-posn (+ x (* 2 size)) (+ y (* 2 size))) size size "red")
  (draw-move-set moves (+ x (* 2 size)) (+ y (* 2 size)) size side))

;Draws a set of destinations
(define (draw-dests dests)
  (for-each (lambda (dest) (draw-multi-square (super-pos (mult-pos dest (make-posn square-size square-size)) board-start) square-part square-part "red" 4)) dests))

;Draws everything
(define (draw state card-index piece-pos)
  ((draw-viewport pix) (make-rgb 0.243 0.153 0.137))
  (draw-board (state-board state))
  (draw-cards (state-cards state) (state-side state))
  
  (cond ((posn? piece-pos)
         (draw-multi-square (super-pos (mult-pos piece-pos (make-posn square-size square-size)) board-start) square-part square-part "green" 4)))
  (cond ((> card-index -1)
         (draw-multi-square (make-posn (+ board-x (* card-index (- card2-x board-x))) (side-y (state-side state))) (* 2 square-size) square-size "green" 4)))
  (cond ((and (posn? piece-pos) (> card-index -1))
         (draw-dests (get-dests state card-index piece-pos))))
  (copy-viewport pix view))