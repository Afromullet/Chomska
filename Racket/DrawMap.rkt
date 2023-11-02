#lang racket/gui
(require racket/include)



(require "WorldMap.rkt")



(define world-map (initialize-world-map))




(define (on-key-press event)
  (define key (send event get-key-code))
  (if (= key #\x)
      (begin
        (display "X key pressed!\n")
        (newline))
      (begin
        (display (format "Unrecognized key pressed: ~a\n" key))
        (newline))))


    
        

(define frame (new frame%
                  [label "Tile Map Example"]
                  [width (* MAP-WIDTH TILE-SIZE)]
                  [height (* MAP-HEIGHT TILE-SIZE)]
                  [alignment '(center center)]))

(define canvas (new canvas%
                   [parent frame]
                   [min-width (* MAP-WIDTH TILE-SIZE)]
                   [min-height (* MAP-HEIGHT TILE-SIZE)]
                   [stretchable-width #f]
                   [stretchable-height #f]
                   [paint-callback (lambda (canvas dc)
                                     (draw-tile-map canvas world-map))]
                   ;[on-key-press on-key-press]

                   ))


; Attach the on-key-press handler to the canvas using on-event
(send canvas on-event
      (lambda (e)
        (cond
          [(is-a? e key-event%)
           (define key-event (send e get-key-event))
           (when (eq? (send key-event get-key-code) 'x)
             (display "X key pressed!\n")
             (newline))]
          [else
           (display "Unrecognized event\n")
           (newline)])))

(send frame show #t)