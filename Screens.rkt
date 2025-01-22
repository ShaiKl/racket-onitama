;Draws menu and returns menu-click
(define (menu)
  (set! view (open-viewport "Main Menu" 400 600))
  ((draw-pixmap view) "Screens/menu.png" (make-posn 0 0))
  (menu-click (make-posn 0 0)))

;Returns player's choice on the main menu screen
(define (menu-click click)
  (cond
    ((posn-rect? click 100 200 150 80) (close-viewport view) (game))
    ((posn-rect? click 100 200 250 80) (close-viewport view) (genetics))
    ((posn-rect? click 100 200 350 80) (close-viewport view) (set! view (open-viewport "rules" 700 500)) (rules 0))
    ((posn-rect? click 100 200 450 80) (exit))
    (else (menu-click (mouse)))))



;Shows last play, waits, shows victory screens, waits for click, goes to menu
(define (victory state)
  (draw state -1 -1)
  (sleep 2)
  ((draw-pixmap view) (victory-pic (- (state-side state))) (make-posn 0 0))
  (get-mouse-click view)
  (close-viewport view)
  (menu))

;Receives a side and returns corresponding victory picture
(define (victory-pic side)
  (cond
    ((= side blue) "Screens/blue.png")
    ((= side red) "Screens/red.png")))



(define (rules page)
  (cond
    ((= page 3)
     (close-viewport view)
     (menu))
    (else
     ((draw-pixmap view) (string-append "Screens/Rules" (number->string page) ".png") (make-posn 0 0))
     (get-mouse-click view)
     (rules (add1 page)))))