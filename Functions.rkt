;Returns the posn of the next mouse-click
(define (mouse)
  (mouse-click-posn (get-mouse-click view)))

;Returns the highest number in a list
(define (maxL L)
  (cond
    ((empty? (rest L)) (first L))
    (else (maxL (cons (max (first L) (second L)) (rest (rest L)))))))

;Returns the index of the highest number in a list
(define (maxL-index L index best-index)
  (cond
    ((= index (length L)) best-index)
    ((> (list-ref L index) (list-ref L best-index)) (maxL-index L (add1 index) index))
    (else (maxL-index L (add1 index) best-index))))

;Returns the lowest number in a list
(define (minL L)
  (cond
    ((empty? (rest L)) (first L))
    (else (minL (cons (min (first L) (second L)) (rest (rest L)))))))

;Subtracts the minimum number in L from all values in L
(define (equalize L)
  (subL L (minL L)))

;Subtracts v from all values in L
(define (subL L v)
  (cond
    ((empty? L) '())
    (else (cons (- (first L) v) (subL (rest L) v)))))

;Returns true if posn is within the bounds of a rectangle
(define (posn-rect? posn x width y height)
  (and (>= (posn-x posn) x) (<= (posn-x posn) (+ x width)) (>= (posn-y posn) y) (<= (posn-y posn) (+ y height))))

;Adds up the coordinates of 2 posn
(define (super-pos pos1 pos2)
  (make-posn (+ (posn-x pos1) (posn-x pos2)) (+ (posn-y pos1) (posn-y pos2))))

;Multiplies the coordinates of 2 posn
(define (mult-pos pos1 pos2)
  (make-posn (* (posn-x pos1) (posn-x pos2)) (* (posn-y pos1) (posn-y pos2))))

;Returns true if both posn have equal coordinates
(define (equal-pos? pos1 pos2)
  (and (= (posn-x pos1) (posn-x pos2)) (= (posn-y pos1) (posn-y pos2))))

;Returns true if L contains pos1
(define (contains-pos? L pos1)
  (not (zero? (count (lambda (pos2) (equal-pos? pos1 pos2)) L))))
