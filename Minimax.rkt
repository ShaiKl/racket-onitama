;Chromosome used for card heuristic function
(define chrom '(4 4 1 3 3))

;Solves state using the minimax algorithm (with or withouting abp depending on settings)
(define (solve state)
  (cond
    (alpha-beta (abp-start state))
    (else (mm-start state))))
  
  
;Inital alpha beta minimax function
;Returns the child of state with the highest score
(define (abp-start state)
  (argmax (lambda (move) (abp move (sub1 ai-depth) -inf.0 +inf.0 -1)) (get-moves state)))

;Main minimax abp loop
(define (abp state depth a b max?)
  (cond
    ((win? state) (* (- -1000 depth) max?))
    ((= depth 0)  (* (heur state) max?))
    ((= max? +1) (abp-max (get-moves state) (sub1 depth) a b))
    ((= max? -1) (abp-min (get-moves state) (sub1 depth) a b))))

(define (abp-max moves depth a b)
  (cond
    ((or (empty? moves) (>= a b)) a)
    (else (abp-max (rest moves) depth (max a (abp (first moves) depth a b -1)) b))))

(define (abp-min moves depth a b)
  (cond
    ((or (empty? moves) (>= a b)) b)
    (else (abp-min (rest moves) depth a (min b (abp (first moves) depth a b 1))))))

;Inital minimax function
;Returns the child of state with the highest score
(define (mm-start state)
  (argmax (lambda (move) (mm move (sub1 ai-depth) -1)) (get-moves state)))

;Main minimax abp loop
(define (mm state depth max?)
  (cond
    ((win? state) (* (- -1000 (* 100 depth)) max?))
    ((= depth 0)  (* (heur state) max?))
    ((= max? +1) (mm-max (get-moves state) (sub1 depth)))
    ((= max? -1) (mm-min (get-moves state) (sub1 depth)))))

(define (mm-max moves depth)
  (cond
    ((empty? moves) -inf.0)
    (else (max (mm (first moves) depth -1) (mm-max (rest moves) depth)))))

(define (mm-min moves depth)
  (cond
    ((empty? moves) +inf.0)
    (else (min (mm (first moves) depth 1) (mm-min (rest moves) depth)))))


;Returns the posn in the grid of every piece of side side
(define (get-pieces state side)
  (define list (build-list 25 (lambda (n) (make-posn (remainder n 5) (quotient n 5)))))
  (define proc  (lambda (pos) (= (piece-side (matrix-ref (state-board state) pos)) side)))
  (filter proc list))

;Returns all child nodes of state
(define (get-moves state)
  (get-moves2 state 0 (get-pieces state (state-side state))))

(define (get-moves2 state card-index pieces)
  (cond
    ((= card-index 2) '())
    (else (append (get-moves3 state card-index pieces) (get-moves2 state (add1 card-index) pieces)))))

(define (get-moves3 state card-index pieces)
  (cond
    ((empty? pieces) '())
    (else (append (get-moves4 state card-index (first pieces) (get-dests state card-index (first pieces))) (get-moves3 state card-index (rest pieces))))))

(define (get-moves4 state card-index piece dests)
  (cond
    ((empty? dests) '())
    (else (cons (make-play state card-index piece (first dests)) (get-moves4 state card-index piece (rest dests))))))


;Returns the heuristics of state from the prespective of (state-side state)
(define (heur state)
  (define allies (get-pieces state (state-side state)))
  (define enemies (get-pieces state (- (state-side state))))
  (define difference (- (length allies) (length enemies)))
  (- (+ (heur-board allies (- 10 difference))
           (heur-cards (take (state-cards state) 1)))
        (+ (heur-board enemies (+ 10 difference))
           (heur-cards (list-tail (state-cards state) 2)))))
     

(define (heur-board pieces val)
  (cond
    ((empty? pieces) 0)
    (else (+ (- val (abs (- 2 (posn-y (first pieces)))))
             (heur-board (rest pieces) val)))))

(define (heur-cards cards)
  (cond
    ((empty? cards) 0)
    (else (+ (get-gene chrom (card-name (first cards))) (heur-cards (rest cards))))))

;Receives a chromosome and the name of a gene.
;Returns the gene within the chromosome with the index of the card with the name
(define (get-gene chrom name)
  (list-ref chrom (index-of (map card-name basic-set) name)))