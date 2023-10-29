#lang racket/gui
(require racket/include)



(require "WorldMap.rkt")



(define (draw-tile dc x y)
  (send dc set-brush (make-object brush% "blue"))
  (send dc draw-rectangle x y TILE-SIZE TILE-SIZE))

(define (draw-tile-png dc x y image-path)
  (define image (read-bitmap image-path))
  (send dc draw-bitmap image x y))




;Need to change this so we don't need to search teh map twice with get-drawing data and get-tile-noise-data
(define (draw-tile-map canvas)
  (define dc (send canvas get-dc))
  (for ([x (in-range MAP-WIDTH)])
    (for ([y (in-range MAP-HEIGHT)])
      (let ([tile (get-tile world-map x y)])
         (draw-tile-png dc (hash-ref tile 'tile-width)
                      (hash-ref tile 'tile-height ) (get-noise-png(hash-ref tile 'noise)))))))       
        

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
                                     (draw-tile-map canvas))]))

(send frame show #t)