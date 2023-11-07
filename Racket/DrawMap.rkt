#lang racket/gui
(require racket/include)



(require "WorldMap.rkt")


; Define a list of noise functions
(define noise-functions
  '("square-bump" "euclidian-squared" "lol-function"))




(define (world-map) (initialize-world-map noise-composition euclidian-squared))

(define (world-map-distance-func distance-func) (initialize-world-map noise-composition distance-func))


(define distance-functions
  (hash "square-bump" square-bump
        "euclidian-squared" euclidian-squared
        "lol-function" lol-function
        ))

; Set frequency to specific note
(define (set-distance-function choice event canvas)
  (world-map-distance-func (hash-ref distance-functions (send choice get-string-selection)))
  (printf (send choice get-string-selection))
  (send canvas refresh))




(define frame (new frame%
                  [label "Tile Map Example"]
                  [width (* MAP-WIDTH TILE-SIZE)]
                  [height (* MAP-HEIGHT TILE-SIZE)]
                  [alignment '(center center)]))













; Make a static text message in the frame
(define msg (new message% [parent frame]
                          [label "No events so far..."]))
    
  ; Derive a new canvas (a drawing window) class to handle events
(define my-canvas%
  (class canvas% ; The base class is canvas%
    ; Define overriding method to handle mouse events
    (define/override (on-event event)
      (send msg set-label "Canvas mouse")
      (send msg set-label "Canvas mouse1"))
    
    ; Define overriding method to handle keyboard events
    (define/override (on-char event)

      (world-map)
       (send canvas refresh)    ; Refresh the canvas to display the new map
      


      )
    ; Call the superclass init, passing on all init args
    (super-new)))
      
(define canvas (new  my-canvas%
                   [parent frame]
                   [min-width (* MAP-WIDTH TILE-SIZE)]
                   [min-height (* MAP-HEIGHT TILE-SIZE)]
                   [stretchable-width #f]
                   [stretchable-height #f]
                   [paint-callback (lambda (canvas dc)
                                     (draw-tile-map canvas (world-map)))]))



; Create a list-box with the noise function options
(define noise-function-list-box
  (new list-box%
    [label "Select Noise Function"] ; Set the label here
    [choices noise-functions]
    [parent frame]
    [callback
     (lambda (choice event)
       (set-distance-function choice event canvas))]))





(send frame show #t)