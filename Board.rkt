
(define board-x 350) ;Left edge of board on viewport
(define board-y 150) ;Top edge of board on viewport
(define board-start (make-posn board-x board-y))
(define square-size 100) ;Width and Height of a square on the board grid
(define square-part (* square-size 0.9)) ;The part of a square that isn't empty space

;Receives a click posn and returns a posn on the board grid
(define (click->board-posn click)
  (make-posn (quotient (- (posn-x click) board-x) square-size) (quotient (- (posn-y click) board-y) square-size)))

;Receives a board and a posn on the board grid
;returns the coresponding piece from the board
(define (matrix-ref board posn)
  (list-ref board (+ (* (posn-y posn) 5) (posn-x posn))))

;Receives a board, a posn on the board grid and a piece
;returns the board with the coresponding piece set to v
(define (matrix-set L pos v)
  (list-set L (+ (* 5 (posn-y pos)) (posn-x pos)) v))


(define-struct piece (type side))


(define pawn 1)
(define king 2)
(define empty-tile 0)


;Returns true if the side and type of both pieces is equal
(define (equal-piece? piece1 piece2)
  (and (= (piece-side piece1) (piece-side piece2)) (= (piece-type piece1) (piece-type piece2))))

;Returns true if the board contains piece1
(define (contains-piece? board piece1)
  (memf (lambda (piece2) (equal-piece? piece1 piece2)) board))

(define piece-types (list pawn pawn king pawn pawn)) 
(define red-side (make-list 5 red))
(define blue-side (make-list 5 blue))
(define empty-tiles (make-list 5 empty-tile))

(define board
  (append (map make-piece piece-types red-side)
          (map make-piece empty-tiles empty-tiles)
          (map make-piece empty-tiles empty-tiles)
          (map make-piece empty-tiles empty-tiles)
          (map make-piece piece-types blue-side)))



;Returns all possible destinations in the state using card-index and piece-pos
(define (get-dests state card-index piece-pos)
  (define move->dest (lambda (move) (super-pos piece-pos (mult-pos move (make-posn (- (state-side state)) (state-side state))))))
  (define legal? (lambda (dest) (and (posn-rect? dest 0 4 0 4) (not (= (piece-side (matrix-ref (state-board state) dest)) (state-side state))))))
  (filter legal? (map move->dest (card-moves (list-ref (state-cards state) card-index)))))
