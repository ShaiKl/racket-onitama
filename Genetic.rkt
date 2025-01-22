
;Returns a randomly generated population
(define (starting-gen)
  (generate-pop gen-size))

;Receives the amount of chromosomes in a population
;Returns a randomly generated population
(define (generate-pop gen-size)
  (cond
    ((zero? gen-size) '())
    (else (cons (generate-chrom (length basic-set)) (generate-pop (sub1 gen-size))))))

;Receives the amount of genes in a chromosome
;Returns a randomly generated chromosome
(define (generate-chrom chrom-size)
  (cond
    ((zero? chrom-size) '())
    (else (cons (random gene-range) (generate-chrom (sub1  chrom-size))))))



;Returns fitness score for a population
(define (get-fitness pop)
  (tournament pop (make-list (length pop) 0) 0 1))

;Returns fitness score for a population empty list of length pop and 0 1
(define (tournament pop fitness n1 n2)
  (cond
    ((= n1 (length pop)) (equalize fitness))
    ((= n2 (length pop)) (tournament pop fitness (add1 n1) 0))
    ((= n1 n2) (tournament pop fitness n1 (add1 n2)))
    (else (tournament pop (update-fit n1 n2 fitness (1v1 (starting) (list-ref pop n1) (list-ref pop n2) 0)) n1 (add1 n2)))))

;Recieves 2 chromosomes and the number 0
;Returns 1 if c1 wins in a simulation, -1 if c2 wins and 0 if it's a tie
(define (1v1 state c1 c2 turn)
  (set! chrom c1)
  (cond
    ((win? state) (state-side state))
    ((= turn 100) 0)
    (else (1v1 (solve state) c2 c1 (add1 turn)))))

;Recieves a fitness list, 2 indexes and the result of a 1v1
;Returns the fitness list with the result added to index n1 and subtracted from index n2
(define (update-fit n1 n2 fitness result)
  (list-set (list-set fitness n1 (+ (list-ref fitness n1) result)) n2 (- (list-ref fitness n2) result)))



;Receives a population, fitness score, a sum of fitness and an empty list
;Returns a new population based on the existing population using the roulette method
(define (make-pop pop fitness sum new-pop)
  (cond
    ((<= gen-size (length new-pop)) new-pop)
    (else (make-pop pop fitness sum (remove-duplicates (append (crossover-roulette pop fitness sum) new-pop))))))
    
;Receives a population, fitness and sum of fitness
;Returns 2 chromsomes made as a crossover between 2 chromosomes in the population using the roulette method
(define (crossover-roulette pop fitness sum)
  (define win1 (roulette fitness (add1 (random sum))))
  (define win2 (roulette (list-set fitness win1 0) (add1 (random (- sum (list-ref fitness win1))))))
  (double-cross (list-ref pop win1) (list-ref pop win2) (generate-cross (length (first pop)))))

;Receives a list and a random number between 0 and the sum of fitness
;Subtracts (first fitness) from num recursively untill it reaches 0 then returns the index of (first fitness)
;Example (roulette '(5 7 0 2 4 8) 13) -> 3
(define (roulette fitness num)
  (cond
    ((<= num (first fitness)) 0)
    (else (add1 (roulette (rest fitness) (- num (first fitness)))))))

;Receives 2 chromosomes and a list of (length c1) with 0 and 1 randomly
;Returns a new chromosome that includes genes from c1 with a coresponding 0 in cross and genes from c2 with coresponding 1 in cross
;Example (crossover '(1 2 3) (4 5 6) (0 0 1)) -> '(1 2 6)
(define (crossover c1 c2 cross)
  (cond
    ((empty? c1) '())
    ((zero? (first cross)) (cons (first c1) (crossover (rest c1) (rest c2) (rest cross))))
    (else (cons (first c2) (crossover (rest c1) (rest c2) (rest cross))))))

;Receives 2 chromosomes and a list of (length c1) with 0 and 1 randomly
;Returns both possible crossovers with the chromosomes and cross
(define (double-cross c1 c2 cross)
  (cons (mutate (crossover c1 c2 cross)) (cons (mutate (crossover c2 c1 cross)) '())))

;Returns a list of (random 2) of length n
(define (generate-cross n)
  (cond
    ((zero? n) '())
    (else (cons (random 2) (generate-cross (sub1 n))))))



(define  standard '(0 0 0 0 0))

;Receives a chromosome
;Returns a string containing the number and percentage of wins by c over standard chrom out of all 240 starting combinations for 5 cards
;The simulation is always done in depth 3 to save time
(define (evaluate c)
  (define cur-depth ai-depth)
  (set! ai-depth 3)
  (define wins (evaluate2 c (permutations basic-set)))
  (set! ai-depth cur-depth)
  (string-append (number->string wins) "/240, " (number->string (round (/ wins 2.4))) "%"))

;Recieves 2 chromosomes and a list of cards list combinations
;Returns number of wins of c1 over c2
(define (evaluate2 c combs)
  (cond
    ((empty? combs) 0)
    (else (+ (perserve-wins (1v1 (make-state board (first combs) blue) c standard 0))
             (perserve-wins (- (1v1 (make-state board (first combs) blue) standard c 0)))
             (evaluate2 c (rest combs))))))

;Returns 1 if v is 1 and 0 otherwise
(define (perserve-wins v)
  (cond
    ((= v 1) 1)
    (else 0)))



;1 out of mut-rate times returns chrom with a random gene swapped out with a random gene within gene-range
(define (mutate chrom)
  (cond
    ((zero? (random mut-rate)) (list-set chrom (random (length basic-set))  (random gene-range)))
    (else chrom)))



;Init functions for genetics algorithm
;Displays some stuff and starts the learning loop
(define (genetics)
  (display (string-append "gen-size - " (number->string gen-size) "\ngene-range - " (number->string gene-range) "\nmut-rate - " (number->string mut-rate) "\nai-depth - " (number->string ai-depth) "\n\n"))
  (learn (starting-gen) 0))

;Genetic algorithm learning loop
(define (learn pop gen)
  (display (string-append "Generation #" (number->string gen) ": "))
  (display pop)
  (learn (next-gen pop (get-fitness pop)) (add1 gen)))

;Receives a population and the population's fitness score
;Prints out the fitness score and prints out the best chrom's wins and percentage
;Returns next generation pop
(define (next-gen pop fitness)
  (define best-chrom (list-ref pop (maxL-index fitness 0 0)))
  (display "\nFitness: ")
  (display fitness)
  (display "\n")
  (display best-chrom)
  (display " - ")
  (display (evaluate best-chrom))
  (newline)
  (newline)
  (make-pop pop fitness (apply + fitness) '()))
