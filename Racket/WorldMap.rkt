#lang racket
(require racket/include)
(require images/flomap racket/flonum noise)
(require racket/match)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                Definitions                                                       ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define MAP-WIDTH 5)
(define MAP-HEIGHT 5)
(define TILE-SIZE 32)
(require racket/match)

(define grid-size 5) ; Define the size of the grid
(define world-map '()) ; Initialize an empty grid
(define world-map2 '()) ; Initialize an empty grid


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                Simplex Noise Related                                              ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; Creates a tile with a simplex noise value
(define (create-noise-tile x-cord y-cord)
  (hash 'x x-cord
        'y y-cord
        'tile-width (* x-cord TILE-SIZE)
        'tile-height (* y-cord TILE-SIZE)
        'noise (abs(simplex x-cord y-cord))))


(define (get-noise-png noise-val)
(cond
[(between? noise-val 0 0.5) "deserttile1.png"]
 [(between? noise-val 0.51 1) "foresttile1.png"]
 ))
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                Basic Tile Related                                               ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; Creates a tile 
(define (create-tile x-cord y-cord)
  (hash 'x x-cord
        'y y-cord
        'tile-width (* x-cord TILE-SIZE)
        'tile-height (* y-cord TILE-SIZE)))

;Returns the hash table at the coordinates
(define (get-tile grid x y)
  (list-ref (list-ref grid x) y))

;Gets what we need to draw a tile
(define (get-drawing-data grid x y)
  (let ([map-tile (get-tile grid x y)])
    (cons (hash-ref map-tile 'tile-width) (hash-ref map-tile 'tile-height))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                World Map Related                                                ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Creates a map where each tile is a hash table containing the coordinates.
(for ([x-cord (in-range MAP-WIDTH)])
  (define row '())
  (for ([y-cord (in-range MAP-HEIGHT)])
    (set! row
          (append row(list (create-noise-tile x-cord y-cord)))))
     (set! world-map(append world-map (list row))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                Utility Todo move                                                 ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;Tells us whether a value is in a range
(define (between? value min-value max-value)
 (and (>= value min-value) (<= value max-value)))

(provide (all-defined-out))