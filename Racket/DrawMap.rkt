#lang racket/gui
(require racket/include)



(require "WorldMap.rkt")


(define elevation-noise-composition (compose-noise 3))
(define moisture-noise-composition (compose-noise 3))
(define sine-noise (sine-modulated-noise 0 360 1 100))



(define elevation-sine-modulation (sine-modulated-noise 0 360 1 100))

(define elevation-transformer (create-noise-transformer no-distance-function elevation-noise-composition 1 1))
(define moisture-transformer (create-noise-transformer no-distance-function moisture-noise-composition 1 1))

(define (world-map) (initialize-world-map elevation-transformer moisture-transformer))

(define update-world-map
  (lambda ()
    (set! world-map (initialize-world-map elevation-transformer moisture-transformer))))



#|_______________________________Function lookup tables for elevation and moisture________________________________


____________________________________________________________________________________________________|#


;Selects the appropriate modulation function table based on the input
;Elevation and moisture keep track of their own functions, so we have two hash tables
(define (get-modulation-func-table type)

  (define elevation-modulation-functions
    (hash "composition" elevation-noise-composition
          "sinusoidal" sine-noise
          "linear" linear-stuff))

  (define moisture-modulation-functions
    (hash "composition" noise-composition
          "sinusoidal" sine-noise
          "linear" linear-stuff))
  (cond
    [ (eq? type "elevation") elevation-modulation-functions]
    [ (eq? type "moisture") moisture-modulation-functions]
    [else (void)]))
    
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
(define (set-modulation-function choice event canvas transformer modulation-function-table)
  (set-modulator transformer (hash-ref modulation-function-table (send choice get-string-selection))))


;I want to keep things grouped together as much as possible
;This is to select which kind of distance function parameter to modify
(define (set-distance-function-parameter type)

  (define (set-fudge-factor entry event transformer)
    (define factor (send entry get-value))
    (set-redist-fudge transformer (send entry get-value)))

  (define (set-expt entry event transformer)
    (define factor (send entry get-value))
    (set-redist-exp transformer (send entry get-value)))

  (cond
    [ (eq? type "fudge") set-fudge-factor]
    [ (eq? type "expt") set-expt]
    [else (void)]))

  


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
      
(define canvas
  (new  my-canvas%
                     [parent frame]
                     [min-width (* MAP-WIDTH TILE-SIZE)]
                     [min-height (* MAP-HEIGHT TILE-SIZE)]
                     [stretchable-width #f]
                     [stretchable-height #f]
                     [paint-callback (lambda (canvas dc)
                                       (draw-tile-map canvas (world-map) dc))]))



(define generate-new-map-button
  (new button%
       [label "Generate new map"]
       [parent frame]
       [callback
        (lambda (choice event )
          update-world-map(send canvas refresh))]))


(send frame show #t)


#|_______________________________Elevation Parameter GUI Compoents_____________________________________________________

GUI Portion for elevation noise parameters
____________________________________________________________________________________________________|#


; Make a frame by instantiating the frame% class
(define elevation-settings-frame (new frame% [label "Example"]))


#|
; Create a list-box with the noise function options
(define elevation-distance-function-list-box
  (new list-box%
       [label "Select Elevation Distance Function"] ; Set the label here
       [choices distance-func-names]
       [parent elevation-settings-frame]
       [callback
        (lambda (choice event)
          (set-distance-function choice event canvas elevation-transformer))]))
|#


; Create a list-box with the noise function options
(define elevation-modulation-function-list-box
  (new list-box%
       [label "Select Elevation Modulation Function"] ; Set the label here
       [choices modulation-func-names]
       [parent elevation-settings-frame]
       [callback
        (lambda (choice event)
          (set-modulation-function choice event canvas elevation-transformer (get-modulation-func-table "elevation")))]))

(define elevation-redistribution-fudge-factor
  (new slider%
       [label "Elevation Redistribution Fudge Factor"]
       [min-value 0]
       [max-value 10]
       [parent elevation-settings-frame]
       [callback
        (lambda (entry event)
          ((set-distance-function-parameter "fudge")
           entry event elevation-transformer))]))


(define elevation-redistribution-exponent
  (new slider%
       [label "Redistribution Exponent"]
       [min-value 0]
       [max-value 10]
       [parent elevation-settings-frame]
       [callback
        (lambda (entry event)
          ((set-distance-function-parameter "expt")
           entry event elevation-transformer))]))



; Show the frame by calling its show method
(send elevation-settings-frame show #f)



#|_______________________________Sine Modulation Input GUI compoents_____________________________________________________

Creates the GUI components for Sone Modulation Input
____________________________________________________________________________________________________|#

;The else clause is void because nothing happens but the event handler still requires a procedure
(define (update-sine-settings choice event text-field transformer modulator)
  (define octave-input (send text-field get-value))
 
  (if (string->number octave-input)
      (begin
        ;(set! modulator (compose-noise (string->number octave-input)))
        (set-modulator transformer modulator)) 
      (void)))


(define elevation-menu-bar
  (new menu-bar%
       [parent elevation-settings-frame]
       ))

(define elevation-sine-menu
  (new menu%
       [label "Sine Modulation Settings"]
       [parent elevation-menu-bar]
       [demand-callback
        (lambda (m) (send elevation-setting-frames show #t))]
       ))

          



; Make a frame by instantiating the frame% class
(define elevation-setting-frames (new frame% [label "Example"]))


(define sine-min-freq-textbox
  (new text-field%
       [label "Min Freq"]
       [parent elevation-setting-frames]))

(define sine-max-freq-textbox
  (new text-field%
       [label "Max Freq"]
       [parent elevation-setting-frames]))

(define sine-start-deg-freq-textbox
  (new text-field%
       [label "Start Deg"]
       [parent elevation-setting-frames]))

(define sine-stop-deg-freq-textbox
  (new text-field%
       [label "Stop Deg"]
       [parent elevation-setting-frames]))

(define store-sine-settings-button
  (new button%
       [label "Store Sine Settings"]
       [parent elevation-setting-frames]))


;(define sine-setting-textbox

#|_______________________________moisture Parameter GUI Compoents_____________________________________________________


;GUI Portion for moisture noise parameters
____________________________________________________________________________________________________|#


; Make a frame by instantiating the frame% class
(define moisture-settings-frame (new frame% [label "Example"]))

#|
; Create a list-box with the noise function options
(define moisture-distance-function-list-box
  (new list-box%
       [label "Select moisture Distance Function"] ; Set the label here
       [choices distance-func-names]
       [parent moisture-settings-frame]
       [callback
        (lambda (choice event)
          (set-distance-function choice event canvas moisture-transformer))]))
|#

; Create a list-box with the noise function options
(define moisture-modulation-function-list-box
  (new list-box%
       [label "Select moisture Modulation Function"] ; Set the label here
       [choices modulation-func-names]
       [parent moisture-settings-frame]
       [callback
        (lambda (choice event)
          (set-modulation-function choice event canvas moisture-transformer (get-modulation-func-table "moisture")))]))

(define moisture-redistribution-fudge-factor
  (new slider%
       [label "moisture Redistribution Fudge Factor"]
       [min-value 0]
       [max-value 10]
       [parent moisture-settings-frame]
       [callback
        (lambda (entry event)
          ((set-distance-function-parameter "fudge")
           entry event moisture-transformer))]))

(define moisture-redistribution-exponent
  (new slider%
       [label "Redistribution Exponent"]
       [min-value 0]
       [max-value 10]
       [parent moisture-settings-frame]
       [callback
        (lambda (entry event)
          ((set-distance-function-parameter "expt")
           entry event moisture-transformer))]))


; Show the frame by calling its show method
(send moisture-settings-frame show #f)


#|_______________________________GUI Components to Enable/Disable Windows_____________________________________________________


____________________________________________________________________________________________________|#

;Opens the elevation settings frame
(define open-elevation-settings
  (new check-box%
       [label "Elevation Noise Function Selection Active"]
       [parent frame]
       [callback
        (lambda (choice event) (if (send choice get-value)
                                   (send elevation-settings-frame show #t)
                                   (send elevation-settings-frame show #f)))]))
;Opens the moisture settings frame
(define open-moisture-settings
  (new check-box%
       [label "Moisture Noise Function Selection Active"]
       [parent frame]
       [callback
        (lambda (choice event) (if (send choice get-value)
                                   (send moisture-settings-frame show #t)
                                   (send moisture-settings-frame show #f)))]))


#|_______________________________Noise Composition Input GUI compoents_____________________________________________________

Creates the GUI components for Noise Composition Input
____________________________________________________________________________________________________|#


;todo
;The else clause is void because nothing happens but the event handler still requires a procedure
(define (update-composition-settings choice event text-field transformer modulator)
  (define octave-input (send text-field get-value))
 
  (if (string->number octave-input)
      (begin
        (set! modulator (compose-noise (string->number octave-input)))
        (set-modulator transformer modulator)) 
      (void)))

(define elevation-num-octaves-text-field
  (new text-field%
       [label "Number of Octaves"]
       [parent elevation-settings-frame]
       [enabled #t]))

(define update-elevation-composition-settings-button
  (new button%
       [label "Update Composition Settings"]
       [parent elevation-settings-frame]
       [callback
        (lambda (choice event) 
          (update-composition-settings choice event elevation-num-octaves-text-field elevation-transformer elevation-noise-composition))]))

(define moisture-num-octaves-text-field
  (new text-field%
       [label "Number of Octaves"]
       [parent moisture-settings-frame]
       [enabled #t] ))


(define update-moisture-composition-settings-button
  (new button%
       [label "Update Composition Settings"]
       [parent moisture-settings-frame]
       [callback
        (lambda (choice event) 
          (update-composition-settings choice event moisture-num-octaves-text-field moisture-transformer moisture-noise-composition))]))




(define (create-distance-function-list-box label parent transformer)
  (new list-box%
       [label label]
       [choices distance-func-names]
       [parent parent]
       [callback
        (lambda (choice event)
          (set-distance-function choice event canvas transformer))]))

(define moisture-distance-function-list-box
  (create-distance-function-list-box "Select moisture Distance Function" moisture-settings-frame moisture-transformer))

(define elevation-distance-function-list-box
  (create-distance-function-list-box "Select Elevation Distance Function" elevation-settings-frame elevation-transformer))




      
        
