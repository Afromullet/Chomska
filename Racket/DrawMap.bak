#lang racket/gui
(require racket/include)



(require "WorldMap.rkt")



(define (world-map) (initialize-world-map))

(define (generate-new-world-map)
  (set! world-map (initialize-world-map)))




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




(send frame show #t)