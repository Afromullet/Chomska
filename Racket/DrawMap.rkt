#lang racket/gui
(require racket/include)



(require "WorldMap.rkt")

(define elevation-transformer (create-noise-transformer no-distance-function noise-composition 1 1))
(define moisture-transformer (create-noise-transformer no-distance-function noise-composition 1 1))

(define (world-map) (initialize-world-map elevation-transformer moisture-transformer))

(define update-world-map
  (lambda ()
    (set! world-map (initialize-world-map elevation-transformer moisture-transformer))))


#|_______________________________User Input Event Handlers_________________________________

Handles user inptu events
____________________________________________________________________________________________________|#

; Define a list of distance functions
(define distance-func-names
  '("square-bump" "euclidian-squared" "no-distance-function"))

;Lets us get the function with user selected choice
(define (set-distance-function choice event canvas transformer)
  (define distance-functions
    (hash "square-bump" square-bump
          "euclidian-squared" euclidian-squared
          "no-distance-function" no-distance-function))
  (set-distance-applier transformer  (hash-ref distance-functions (send choice get-string-selection))))



; Define a list of modulator functions
(define modulation-func-names
  '("composition" "sinusoidal" "linear"))

;Lets us get the function with user selected choice
(define (set-modulation-function choice event canvas transformer)
  (define modulation-functions
    (hash "composition" noise-composition
          "sinusoidal" sine-noise
          "linear" linear-stuff))
  (set-modulator transformer (hash-ref modulation-functions (send choice get-string-selection))))


;Handles the sliders 
(define (get-fudge-factor entry event transformer)
  (define factor (send entry get-value))
  (set-redist-fudge transformer (send entry get-value)))


(define (get-expt entry event transformer)
  (define factor (send entry get-value))
  (set-redist-exp transformer (send entry get-value)))




#|_______________________________Main Window GUI Compoents_____________________________________________________

Creates the GUI components for the main window
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

(define generate-new-map-button
  (new button%
       [label "Generate new map"]
       [parent frame]
       [callback
        (lambda (choice event )
          update-world-map(send canvas refresh))]))


(send frame show #t)


#|_______________________________Elevation Parameter GUI Compoents_____________________________________________________

Creates the GUI components for the main window
____________________________________________________________________________________________________|#

;GUI Portion for elevation noise parameters

; Make a frame by instantiating the frame% class
(define elevation-settings-frame (new frame% [label "Example"]))

; Create a list-box with the noise function options
(define elevation-distance-function-list-box
  (new list-box%
       [label "Select Elevation Distance Function"] ; Set the label here
       [choices distance-func-names]
       [parent elevation-settings-frame]
       [callback
        (lambda (choice event)
          (set-distance-function choice event canvas elevation-transformer))]))


; Create a list-box with the noise function options
(define elevation-modulation-function-list-box
  (new list-box%
       [label "Select Elevation Modulation Function"] ; Set the label here
       [choices modulation-func-names]
       [parent elevation-settings-frame]
       [callback
        (lambda (choice event)
          (set-modulation-function choice event canvas elevation-transformer))]))

(define elevation-redistribution-fudge-factor
  (new slider%
       [label "Elevation Redistribution Fudge Factor"]
       [min-value 0]
       [max-value 10]
       [parent elevation-settings-frame]
       [callback
        (lambda (entry event)  (get-fudge-factor entry event elevation-transformer))]))


(define elevation-redistribution-exponent
  (new slider%
       [label "Redistribution Exponent"]
       [min-value 0]
       [max-value 10]
       [parent elevation-settings-frame]
       [callback
        (lambda (entry event) (get-expt entry event elevation-transformer))]))


; Show the frame by calling its show method
(send elevation-settings-frame show #t)

#|_______________________________moisture Parameter GUI Compoents_____________________________________________________

Creates the GUI components for the main window
____________________________________________________________________________________________________|#

;GUI Portion for moisture noise parameters

; Make a frame by instantiating the frame% class
(define moisture-settings-frame (new frame% [label "Example"]))

; Create a list-box with the noise function options
(define moisture-distance-function-list-box
  (new list-box%
       [label "Select moisture Distance Function"] ; Set the label here
       [choices distance-func-names]
       [parent moisture-settings-frame]
       [callback
        (lambda (choice event)
          (set-distance-function choice event canvas moisture-transformer))]))


; Create a list-box with the noise function options
(define moisture-modulation-function-list-box
  (new list-box%
       [label "Select moisture Modulation Function"] ; Set the label here
       [choices modulation-func-names]
       [parent moisture-settings-frame]
       [callback
        (lambda (choice event)
          (set-modulation-function choice event canvas moisture-transformer))]))

(define moisture-redistribution-fudge-factor
  (new slider%
       [label "moisture Redistribution Fudge Factor"]
       [min-value 0]
       [max-value 10]
       [parent moisture-settings-frame]
       [callback
        (lambda (entry event)  (get-fudge-factor entry event moisture-transformer))]))


(define moisture-redistribution-exponent
  (new slider%
       [label "Redistribution Exponent"]
       [min-value 0]
       [max-value 10]
       [parent moisture-settings-frame]
       [callback
        (lambda (entry event) (get-expt entry event moisture-transformer))]))


; Show the frame by calling its show method
(send moisture-settings-frame show #t)



