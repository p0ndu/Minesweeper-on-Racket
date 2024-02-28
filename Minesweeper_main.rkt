#lang racket/gui

;; Define the dimensions of the game board and the number of mines
(define width 10)
(define height 10)
(define num-mines 10)

;; Initialize the game board as a vector of vectors, representing each cell
(define board
  (make-vector height
               (make-vector width (list #f #f)))) ; Each cell is represented as a list: [#f #f] (not revealed, not flagged)

;; Create the main frame for the game
(define frame (new frame% [label "Minesweeper"] [width 200] [height 200]))

;; Create the canvas to display the game board
(define canvas (new canvas% [parent frame]
                             [min-width 200]
                             [min-height 200]
                             [paint-callback
                              ;; Callback function to draw the game board
                              (lambda (canvas dc)
                                (for ([y (in-range height)])
                                  (for ([x (in-range width)])
                                    (let* ([cell (vector-ref (vector-ref board y) x)]
                                           [revealed? (car cell)] ; Check if cell is revealed
                                           [flagged? (cadr cell)]) ; Check if cell is flagged
                                      (cond
                                        ;; If the cell is both revealed and flagged, draw as flagged
                                        [(and revealed? flagged?)
                                         (send dc set-brush "green" 'solid)
                                         (send dc draw-rectangle (* x 20) (* y 20) 20 20)
                                         (send dc set-brush "black" 'solid)
                                         (send dc draw-text (+ (* x 20) 7) (+ (* y 20) 14) "F")]
                                        ;; If the cell is revealed, draw as revealed
                                        [revealed?
                                         (send dc set-brush "white" 'solid)
                                         (send dc draw-rectangle (* x 20) (* y 20) 20 20)
                                         ; Implement how revealed cells should be drawn here
                                         ]
                                        ;; If the cell is hidden, draw as hidden
                                        [else
                                         (send dc set-brush "gray" 'solid)
                                         (send dc draw-rectangle (* x 20) (* y 20) 20 20)])))))]))

;; Show the main frame
(send frame show #t)

;; Function to handle left-click events on the canvas
(define (on-canvas-click event)
  ;; Get the x and y coordinates of the clicked cell
  (define x (quotient (send event get-x) 20))
  (define y (quotient (send event get-y) 20))
  ;; Get the cell at the clicked coordinates
  (let ([cell (vector-ref (vector-ref board y) x)])
    ;; If the cell is not flagged, set it to be revealed
    (unless (cadr cell)
      (vector-set! (vector-ref board y) x (list #t (cadr cell)))))
  ;; Refresh the canvas to reflect the changes
  (send canvas refresh))

;; Function to handle right-click events on the canvas
(define (on-canvas-right-click event)
  ;; Get the x and y coordinates of the clicked cell
  (define x (quotient (send event get-x) 20))
  (define y (quotient (send event get-y) 20))
  ;; Get the cell at the clicked coordinates
  (let ([cell (vector-ref (vector-ref board y) x)])
    ;; If the cell is not revealed, toggle its flagged status
    (unless (car cell)
      (vector-set! (vector-ref board y) x (list (car cell) (not (cadr cell)))))))
  ;; Refresh the canvas to reflect the changes
  (send canvas refresh)

;; Function to handle mouse events on the canvas
(define (on-canvas-mouse-event event)
  ;; Get the button pressed in the event
  (define button (send event get-button))
  ;; Depending on the button pressed, call the appropriate function
  (cond
    [(= button 'left)
     (on-canvas-click event)]
    [(= button 'right)
     (on-canvas-right-click event)]))

;; Bind the mouse event handler to the canvas
(send canvas on-event on-canvas-mouse-event)

;; but somehow when i click on the squares nothing is shown ffs. wtf am i doing wrong lmao :(((((((