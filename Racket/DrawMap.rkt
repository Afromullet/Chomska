#lang racket/gui
(require racket/include)



(require "WorldMap.rkt")


;Parameters that modify world map generation
(struct map-parameters ([distance-func #:mutable]
                        [noise-func #:mutable]
                        [redist-fudge #:mutable]
                        [redist-exp #:mutable]))

(define (create-map-parameters current-dist-func current-noise-func redist-fudge redist-exp)
  (map-parameters current-dist-func current-noise-func redist-fudge redist-exp))

(define map-params (create-map-parameters no-distance-function noise-composition 1 1))



(define (world-map) (initialize-world-map (map-parameters-distance-func map-params)
                                          (map-parameters-noise-func map-params)
                                          (map-parameters-redist-fudge map-params)
                                          (map-parameters-redist-exp map-params)))


(map-parameters-distance-func map-params)

(define update-world-map
  (lambda ()
    (set! world-map (initialize-world-map (map-parameters-distance-func map-params)
                                          (map-parameters-noise-func map-params)
                                          (map-parameters-redist-fudge map-params)
                                          (map-parameters-redist-exp map-params)))world-map))


#|_______________________________Distance Function Selection  Helpers_________________________________

Functions that allow the user to select which distance function to apply to the world map
____________________________________________________________________________________________________|#

; Define a list of noise functions
(define distance-func-names
  '("square-bump" "euclidian-squared" "no-distance-function"))


(define (set-distance-function choice event canvas)
  
  (define distance-functions
    (hash "square-bump" square-bump
          "euclidian-squared" euclidian-squared
          "no-distance-function" no-distance-function))
  
  (set-map-parameters-distance-func! map-params (hash-ref distance-functions (send choice get-string-selection))))




#|_______________________________Modulation Function Selection  Helpers_________________________________

Functions that allow the user to select which modulation function to apply to the world map
____________________________________________________________________________________________________|#


; Define a list of noise functions
(define modulation-func-names
  '("composition" "sinusoidal" "linear"))

(define (set-modulation-function choice event canvas)
  (define modulation-functions
    (hash "composition" noise-composition
          "sinusoidal" sine-noise
          "linear" linear-stuff))
  (set-map-parameters-noise-func! map-params (hash-ref modulation-functions (send choice get-string-selection))))



#|______________________________Sliders_________________________________

Handles all slider action
____________________________________________________________________________________________________|#



(define (get-fudge-factor entry event)
  (define factor (send entry get-value))
  (set-map-parameters-redist-fudge! map-params (send entry get-value)))


(define (get-expt entry event)
  (define factor (send entry get-value))
  (set-map-parameters-redist-exp! map-params (send entry get-value)))




#|_______________________________GUI Compoents_____________________________________________________

Creates the GUI components
____________________________________________________________________________________________________|#

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
(define distance-function-list-box
  (new list-box%
       [label "Select Distance Function"] ; Set the label here
       [choices distance-func-names]
       [parent frame]
       [callback
        (lambda (choice event)
          (set-distance-function choice event canvas))]))


; Create a list-box with the noise function options
(define modulation-function-list-box
  (new list-box%
       [label "Select Modulation Function"] ; Set the label here
       [choices modulation-func-names]
       [parent frame]
       [callback
        (lambda (choice event)
          (set-modulation-function choice event canvas))]))

(define generate-new-map-button
  (new button%
       [label "Generate new map"]
       [parent frame]
       [callback
        (lambda (choice event )
          update-world-map(send canvas refresh))]))


(define redistribution-fudge-factor
  (new slider%
       [label "Redistribution Fudge Factor"]
       [min-value 0]
       [max-value 10]
       [parent frame]
       [callback get-fudge-factor] ))


(define redistribution-exponent
  (new slider%
       [label "Redistribution Exponent"]
       [min-value 0]
       [max-value 10]
       [parent frame]
       [callback get-expt] ))


(send frame show #t)