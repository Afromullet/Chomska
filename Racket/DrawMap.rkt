#lang racket/gui
(require racket/include)



(require "WorldMap.rkt")


(define elevation-noise-composition (compose-noise 3))
(define moisture-noise-composition (compose-noise 3))
(define sine-noise (sine-modulated-noise 0 360 1 100))

(define elevation-sine-modulation (sine-modulated-noise 0 360 1 100))

(define elevation-transformer (create-noise-transformer "elevation" no-distance-function elevation-noise-composition 1 1))
(define moisture-transformer (create-noise-transformer "moisture" no-distance-function moisture-noise-composition 1 1))

(define (world-map) (initialize-world-map elevation-transformer moisture-transformer))
              
                 

(define update-world-map
  (lambda () (set! world-map (initialize-world-map elevation-transformer moisture-transformer))))
    

;;;;;;;;;;Handles Modulation Function Selection;;;;;;;;;;

; Define a list of modulator functions
(define modulation-func-names
  '("composition" "sinusoidal" "linear"))

;Selects the appropriate modulation function table based on the input
;Elevation and moisture keep track of their own functions, so we have two hash tables
(define (get-modulation-func-table type)
  (match type
    ["elevation"
     (hash "composition" elevation-noise-composition
           "sinusoidal" sine-noise
           "linear" linear-stuff)]

    ["moisture"
     (hash "composition" moisture-noise-composition
           "sinusoidal" sine-noise
           "linear" linear-stuff)]))
 


   

;;;;;;;;;;;Handles Distance Function Selection;;;;;;;;;;

(define distance-func-names
  '("square-bump" "euclidian-squared" "no-distance-function"))

;Lets us get the function with user selected choice
(define (set-distance-function choice event canvas transformer)
  (set-distance-applier transformer  (dist-func-selector(send choice get-string-selection))))
 
 
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

 
;;;;;;;;;; Main GUI window components ;;;;;;;;;;

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



;;;;;;;;;; Creaters the menu Frames ;;;;;;;;;;

(define elevation-setting-frames (new frame% [label "Elevation"]))

(define elevation-modulation-function-frame (new frame% [label "Elevation Mod"]))

(define elevation-distance-frame (new frame% [label "Elevation"]))

(define elevation-sine-settings-frames (new frame% [label "Elevation"]))

(define elevation-composition-settings-frames (new frame% [label "Elevation"]))

(define moisture-settings-frame (new frame% [label "Moisture"]))

(define moisture-composition-settings-frames (new frame% [label "Moisture"]))

(send frame show #t)

(send moisture-settings-frame show #f)

(send elevation-setting-frames show #f)

(send elevation-distance-frame show #f)




;;;;;;;;;; Checkbox to choose which Windows to open ;;;;;;;;;;

;Opens the elevation settings frame
(define open-elevation-settings
  (new check-box%
       [label "Elevation Noise Function Selection Active"]
       [parent frame]
       [callback
        (lambda (choice event) (if (send choice get-value)
                                   (send elevation-setting-frames show #t)
                                   (send elevation-setting-frames show #f)))]))
;Opens the moisture settings frame
(define open-moisture-settings
  (new check-box%
       [label "Moisture Noise Function Selection Active"]
       [parent frame]
       [callback
        (lambda (choice event) (if (send choice get-value)
                                   (send moisture-settings-frame show #t)
                                   (send moisture-settings-frame show #f)))]))


;;;;;;;;;; Data Update Handling ;;;;;;;;;;


;todo
;The else clause is void because nothing happens but the event handler still requires a procedure
(define (update-composition-settings choice event text-field transformer modulator)
  (define octave-input (send text-field get-value))
 
  (if (string->number octave-input)
      (begin
        (set! modulator (compose-noise (string->number octave-input)))
        (set-modulator transformer modulator)) 
      (void)))


;The else clause is void because nothing happens but the event handler still requires a procedure
(define (update-sine-settings choice event text-field transformer modulator)
  (define octave-input (send text-field get-value))
 
  (if (string->number octave-input)
      (begin
        ;(set! modulator (compose-noise (string->number octave-input)))
        (set-modulator transformer modulator)) 
      (void)))



;;;;;;;;;; Will be removed once menu stuff is finished

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


;;;;;;;;;; Functions to create GUI Components ;;;;;;;;;;

; Create a list-box with the distance function options that allows the user to click on an element to select which kind of distance function to apply
(define (create-distance-function-list-box parent transformer)
  (new list-box%
       [label "Select Distance Function"]
       [choices distance-func-names]
       [parent parent]
       [callback
        (lambda (choice event)
          (set-distance-function choice event canvas transformer))]))

; Create a list-box with the noise function options that allows the user to click on an element to select which kind of noise function to apply
(define (create-modulation-function-list-box parent transformer type)
  (new list-box%
       [label "Select Modulation Function"] ; Set the label here
       [choices modulation-func-names]
       [parent parent]
       [callback
        (lambda (choice event)
          (set-modulation-function choice event canvas transformer (get-modulation-func-table type)))]))

;Creaters a slider for the distance function fudge setting
(define (create-fudge-slider parent transformer)
  (new slider%
       [label "Redistribution Fudge Factor"]
       [min-value 0]
       [max-value 10]
       [parent parent]
       [callback
        (lambda (entry event)
          ((set-distance-function-parameter "fudge")
           entry event transformer))]))

;Creaters a slider for the distance function exponent setting
(define (create-exponent-slider parent transformer)
  (new slider%
       [label "Redistribution Exponent"]
       [min-value 0]
       [max-value 10]
       [parent parent]
       [callback
        (lambda (entry event)
          ((set-distance-function-parameter "expt")
           entry event transformer))]))

;Creates a text field that doesn't have an event handler
(define (create-data-textbox parent label)
  (new text-field%
       [label label ]
       [parent parent]))



(define (create-num-octaves-text-field parent)
  (new text-field%
       [label "Number of Octaves"]
       [parent parent]
       [enabled #t]))

(define (create-composition-settings-button parent text-field transformer composer)
  (new button%
       [label "Update Composition Settings"]
       [parent parent]
       [callback
        (lambda (choice event) 
          (update-composition-settings choice event text-field transformer composer))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Moisture GUI Components
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define moisture-distance-function-list-box
  (create-distance-function-list-box moisture-settings-frame moisture-transformer))

(define moisture-modulation-function-list-box (create-modulation-function-list-box moisture-settings-frame moisture-transformer "moisture"))


(define moisture-redistribution-fudge-factor (create-fudge-slider moisture-settings-frame moisture-transformer))

(define moisture-redistribution-exponent (create-exponent-slider moisture-settings-frame moisture-transformer))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Elevation GUI Components
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(define elevation-distance-function-list-box
  (create-distance-function-list-box elevation-distance-frame elevation-transformer))

(define elevation-modulation-function-list-box (create-modulation-function-list-box elevation-modulation-function-frame elevation-transformer "elevation"))

(define elevation-redistribution-fudge-factor (create-fudge-slider elevation-distance-frame elevation-transformer))

(define elevation-redistribution-exponent (create-exponent-slider elevation-distance-frame elevation-transformer))


(define elevation-num-octaves-text-box (create-num-octaves-text-field elevation-composition-settings-frames))

(define elevation-composition-button (create-composition-settings-button
                                      elevation-composition-settings-frames
                                      elevation-num-octaves-text-box
                                      elevation-transformer
                                      elevation-noise-composition))


;Elevation sine-settings GUI components
(define sine-min-freq-textbox (create-data-textbox elevation-sine-settings-frames "Min Freq"))
(define sine-max-freq-textbox (create-data-textbox elevation-sine-settings-frames "Max Freq"))
(define sine-start-deg-freq-textbox (create-data-textbox elevation-sine-settings-frames "Start Deg"))
(define sine-stop-deg-freq-textbox (create-data-textbox elevation-sine-settings-frames "End Deg"))


(define elevation-menu-bar
  (new menu-bar%
       [parent elevation-setting-frames]
       ))

(define elevation-sine-menu
  (new menu%
       [label "Sine Modulation Settings"]
       [parent elevation-menu-bar]
       [demand-callback
        (lambda (m)
          (send elevation-sine-settings-frames show #t)
          (send elevation-distance-frame show #t)
          (send elevation-modulation-function-frame show #t)


          )]
       ))



(define elevation-composition-menu
  (new menu%
       [label "Composition Settings"]
       [parent elevation-menu-bar]
       [demand-callback
        (lambda (m)
          (send elevation-composition-settings-frames show #t)
          (send elevation-distance-frame show #t)
          (send elevation-modulation-function-frame show #t)
                

          )]
       ))


(define elevation-distance-menu
  (new menu%
       [label "Distance Function Settings"]
       [parent elevation-menu-bar]
       [demand-callback
        (lambda (m)
          (send elevation-sine-settings-frames show #t)
          (send elevation-distance-frame show #t)
          (send elevation-modulation-function-frame show #t)
                

          )]
       ))


(define elevation-modulation-menu
  (new menu%
       [label "Modulation Function Settings"]
       [parent elevation-menu-bar]
       [demand-callback
        (lambda (m) 
          (send elevation-sine-settings-frames show #t)
          (send elevation-distance-frame show #t)
          (send elevation-modulation-function-frame show #t)


          )]
       ))








      
        
