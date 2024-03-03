#lang racket/gui


;; Define the dimensions of the game board and the number of mines
(define width 5)
(define height 5)
(define num-mines 5)



;following section commented out because of weirdness in the make-vector function not allowing you to cahnge one index of a 3d vector
;manually defining the board instead, figure this out and change it later in order to make the baord size dynamic
;also changed the innermost section to be a vector instead of a list because why was it a list?
;added third boolean value for if it has a mine or not
;added a 4th value which is going to be an int, telling you how many surrounding mines there are

#|
;; Initialize the game board as a vector of vectors, representing each cell
(define board
  (make-vector height
               (make-vector width (list #f #f)))) ; Each cell is represented as a list: [#f #f] (not revealed, not flagged)
|#
(define board (vector (vector (vector #f #f #f 0) (vector #f #f #f 0) (vector #f #f #f 0) (vector #f #f #f 0) (vector #f #f #f 0))
                      (vector (vector #f #f #f 0) (vector #f #f #f 0) (vector #f #f #f 0) (vector #f #f #f 0) (vector #f #f #f 0))
                      (vector (vector #f #f #f 0) (vector #f #f #f 0) (vector #f #f #f 0) (vector #f #f #f 0) (vector #f #f #f 0))
                      (vector (vector #f #f #f 0) (vector #f #f #f 0) (vector #f #f #f 0) (vector #f #f #f 0) (vector #f #f #f 0))
                      (vector (vector #f #f #f 0) (vector #f #f #f 0) (vector #f #f #f 0) (vector #f #f #f 0) (vector #f #f #f 0))
                      )
  )


;; Create the main frame for the game
(define frame (new frame% [label "Minesweeper"] [width 200] [height 200]))


;minor changes to the paint callback to work with a 3rd dimension vector rather than list
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
                                  [revealed? (vector-ref cell 0)] ; Check if cell is revealed
                                  [flagged? (vector-ref cell 1)]) ; Check if cell is flagged
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





;adding functionality for the game mechanics



#|not in use anymore, delete later

(define (populateMines)
  (for ([i (in-range num-mines )])
    (define x (random width))
    (define y (random height))
    (cond ;if theres already a mine on the randomly selected square then decrement i
      [(not (vector-ref (vector-ref (vector-ref board y) x) 2))
      (vector-set! (vector-ref (vector-ref board y) x) 2 #t)
      (printf "i is ~a \n" i)
      ]
      [#t
       (set! i (- i 1))
       ]
      )
    )
  )
|#


;randomly fills the board with however many mines are needed, call it passing a 0 as counter
(define (populateMines counter) 
  (cond
    [(< counter num-mines)
     (define x (random width))
     (define y (random height))
     
     (cond
       [(not (vector-ref (vector-ref (vector-ref board y) x) 2))
        (vector-set! (vector-ref (vector-ref board y) x) 2 #t)
        (populateMines (+ counter 1))
        ]
       
       [#t
        (populateMines counter)
        ]
       
       )
     ]
    )
  )

;getters for diff attributes of tiles, rememer y scales downwards as far as the computer is concerned
(define (mine? x y)
  (vector-ref (vector-ref (vector-ref board y) x) 2)
  )

(define (flagged? x y)
  (vector-ref (vector-ref (vector-ref board y) x ) 1)
  )

(define (cleared? x y)
  (vector-ref (vector-ref (vector-ref board y) x) 0)
  )



;internal function for getSurroundingMines
(define (GSMI x y)
  (cond
    [(and (x >= 0) (y >= 0) (y < height) (x < width))
     
     (cond
       [(mine? x y)
        1]
       )
     ]
    
    [#t 0]
    )
  )


;returns number of mines surrounding a tile, the max and min functions are to avoid going out of bounds of the array
;also checks the tile you called it on, but if that was a mine the game would already be over so it doesnt matter; just be aware when debugging
(define (getSurroundingMines x y)
  (define numMines 0)
  (for ([i (in-range (max 0 (- x 1)) (min height (+ x 2)))])
        (for ([ j (in-range (max 0 (- y 1)) (min height (+ y 2)))])
          (cond
            [(mine? i j)
             (set! numMines (+ numMines 1))
             ]
            )
          )
    )
  numMines
  )
          
             
       
     
    
  


;recursively clears tiles on the board following these rules - currently WIP
; - if tile is a mine, game ends
; - if tile is not a mine, show number of surrounding mines
; - if there are no surrounding mines, clear all surrounding tiles ignoring whether they are flagged or not
(define (clearTile x y)
  (cond
    [(mine? x y)
     (gameOver)
     ]
    )
  )
  

    
     
;placeholder for now, add functionality later
(define (gameOver)
  0)



