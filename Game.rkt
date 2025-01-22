;Starts the game screen and calls the turn start function with a starting state
(define (game)
  (set! view (open-viewport "Onitamaaaaaaaaa" 950 800))
  (turn-start (starting)))

;Draws state
;If game won returns victory screen.
;If current player is ai calls turn-start recursively with ai solution of current turn.
;if currrent player isn't ai calls turn-start recursively with player choice of which turn to make.
(define (turn-start state)
  (draw state -1 -1)
  (cond
    ((win? state) (victory state))
    ((ai? (state-side state)) (turn-start (solve state)))
    (else (turn-start (choose state 1 -1 (mouse))))))

;Draws the choice made by the player and waits for new click.
(define (update-selected state card-index piece-pos)
  (draw state card-index piece-pos)
  (choose state card-index piece-pos (mouse)))

;Processes player click
;If player clicks on a card, draws the board with the card chosen and awaits new click
;If player clicks on the board calls board-click
;If player clicks on nothing waits for new click recursively
(define (choose state card-index piece-pos click)
  (cond
    ((posn-rect? click board-x (* 2 square-size) (side-y (state-side state)) square-size)
     (update-selected state 0 piece-pos))
    ((posn-rect? click card2-x (* 2 square-size) (side-y (state-side state)) square-size)
     (update-selected state 1 piece-pos))
    ((posn-rect? click board-x (* square-size 5) board-y (* square-size 5))
     (board-click state card-index piece-pos (click->board-posn click)))
    (else
     (choose state card-index piece-pos (mouse)))))

;Processes board clicks
;If player clicks on a friendly piece, redraws the board with it selected
(define (board-click state card-index piece-pos click-pos)
  (cond
    ((= (piece-side (matrix-ref (state-board state) click-pos)) (state-side state))
     (update-selected state card-index click-pos))
    ((and (posn? piece-pos) (> card-index -1) (contains-pos? (get-dests state card-index piece-pos) click-pos))
      (make-play state card-index piece-pos click-pos))
    (else (choose state card-index piece-pos (mouse)))))

;Receives a state a card a piece and a move destination and returns a state after that play is made
(define (make-play state card-index piece-pos dest-pos)
  (make-state (move-piece (state-board state) piece-pos dest-pos)
              (rearrange-cards (state-cards state) card-index)
              (- (state-side state))))

;Receives a list of cards and the index of the used card and returns the list of cards after a play is made using the card at the index.
(define (rearrange-cards cards card-index)
  (list (third cards) (fourth cards) (fifth cards) (list-ref cards (- 1 card-index)) (list-ref cards card-index)))

;Moves a piece from one location to another.
;If a piece is already at the destination replaces it
(define (move-piece board piece-pos dest-pos)
  (matrix-set (matrix-set board dest-pos (matrix-ref board piece-pos)) piece-pos (make-piece empty-tile empty-tile)))