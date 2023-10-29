#lang racket/gui
(require racket/include)

(require "WorldMap.rkt")


(define tile-size 32) ; Size of each tile (in pixels)
(define map-width 10) ; Number of tiles in the X direction
(define map-height 6) ; Number of tiles in the Y direction

(define (draw-tile dc x y)
  (send dc set-brush (make-object brush% "blue"))
  (send dc draw-rectangle x y tile-size tile-size))



(define (draw-tile-map canvas)
  (define dc (send canvas get-dc))
  (for ([x (in-range MAP-WIDTH)])
    (for ([y (in-range MAP-HEIGHT)])
      (let ([tile-x (* x tile-size)]
             [tile-y (* y tile-size)])
        (draw-tile dc tile-x tile-y)))))

(define (draw-tile-map2 canvas)
  (define dc (send canvas get-dc))
  (for ([x (in-range map-width)])
    (for ([y (in-range map-height)])
      (let ([tile-x (* x tile-size)]
             [tile-y (* y tile-size)])
        (draw-tile dc tile-x tile-y)))))

(define frame (new frame%
                  [label "Tile Map Example"]
                  [width (* map-width tile-size)]
                  [height (* map-height tile-size)]
                  [alignment '(center center)]))

(define canvas (new canvas%
                   [parent frame]
                   [min-width (* map-width tile-size)]
                   [min-height (* map-height tile-size)]
                   [stretchable-width #f]
                   [stretchable-height #f]
                   [paint-callback (lambda (canvas dc)
                                     (draw-tile-map canvas))]))

(send frame show #t)