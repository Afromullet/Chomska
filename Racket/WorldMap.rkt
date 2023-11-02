#lang racket/gui
(require racket/include)
(require  noise)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                Definitions                                                       ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define MAP-WIDTH 50)
(define MAP-HEIGHT 50)
(define TILE-SIZE 8)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                Perlin Noise Related                                              ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (get-seed)
  (current-inexact-milliseconds))


;We use Noise composition. The final noise source equals the sum of:
;amplitude_1 * noise(octave_1 * nx, octave_1 * ny) +
;amplitude_2 * noise(octave_2 * nx,octave_2 * ny)+
;...+
;amplitude_N * noise(octave_N * nx, octave_N * ny);
;Then we normalize the noise at the end using the sum of the amplitudes
;Represents the data we need for noise composition.
;Stores a list of amplitudes, octaves, and the total amplitude
(struct noise-params (amplitudes octaves total-amplitude ))

;Used to create the noise-params type struct
(define (create-noise-params amplitudes octaves)
  (noise-params amplitudes octaves (apply + amplitudes))) 

;Creates the noise-params, given a number of octaves
(define (build-noise-params num-octaves)
  (let ([amps '()]
        [octs '()])
  (for/list ([i (in-range 1 (+ num-octaves 1))])
    (let* ([octave (expt 2 (- i 1))]
           [amplitude (/ 1.0 octave)])
      (set! amps (append amps (list amplitude)))
      (set! octs (append octs (list octave)))))
         (create-noise-params amps octs)))

;Gets a single noise sample
;Divides by the map width and height so we get values between 0-1
(define (generate-noise amplitude octave x-cord y-cord)
  (* amplitude (abs(perlin (* octave (/ x-cord MAP-WIDTH)) (* octave (/ y-cord MAP-HEIGHT))))))

; Creates a tile with a simplex noise value
(define (create-noise-tile x-cord y-cord)
  (hash 'x x-cord
        'y y-cord
        'tile-width (* x-cord TILE-SIZE)
        'tile-height (* y-cord TILE-SIZE)
        ;'noise (abs(simplex x-cord y-cord))))
        'noise (compose-noise noise-source x-cord y-cord)))

;Composes the noise by adding several noise sources together using octaves and amplitudes.
;Adds an offset through a seed so we don't get the same valeu every time. 
(define (compose-noise source x-cord y-cord)
  (define noise-values
    (for/list ([amp (noise-params-amplitudes source)]
               [oct (noise-params-octaves source)])
      (generate-noise amp oct (+ x-cord (get-seed)) (+ y-cord (get-seed)))))
       (normalize-noise (foldl + 0 noise-values) (noise-params-total-amplitude source)))

;normalizes the noise so that we have values between 0 and 1
(define (normalize-noise noise-value total-amplitude)
  (/ noise-value total-amplitude))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                Drawing Related                                                   ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;Need to change this so we don't need to search teh map twice with get-drawing data and get-tile-noise-data
(define (draw-tile-map canvas world-map)
  (define dc (send canvas get-dc))
  (for ([x (in-range MAP-WIDTH)])
    (for ([y (in-range MAP-HEIGHT)])
      (let ([tile (get-tile world-map x y)])
         (draw-tile-png dc (hash-ref tile 'tile-width)
                      (hash-ref tile 'tile-height ) (get-noise-png(hash-ref tile 'noise)))))))   


(define (draw-tile-png dc x y image-path)
  (define image (read-bitmap image-path))
  (send dc draw-bitmap image x y))

;Returns the png for a noise value
(define (get-noise-png noise-val)
(cond
[(< noise-val 0.1) "water1.png"]
[(< noise-val 0.2) "beachsand1.png"]
[(< noise-val 0.3) "tree1.png"]
[(< noise-val 0.5) "jungle1.png"]
[(< noise-val 0.7) "shrubland1.png"]
[(< noise-val 0.9) "deserttile1.png"]
[else  "snowground1.png"]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                Basic Tile Related                                               ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;Returns the hash table at the coordinates
(define (get-tile grid x y)
  (vector-ref (vector-ref grid x) y))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                World Map Related                                                ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (initialize-world-map)
  (define world-map (make-vector MAP-WIDTH))
  (for ([x-cord (in-range MAP-WIDTH)])
    (define row (make-vector MAP-HEIGHT))
    (for ([y-cord (in-range MAP-HEIGHT)])
      (vector-set! row y-cord (create-noise-tile x-cord y-cord)))
    (vector-set! world-map x-cord row))
  world-map)


(define noise-source (build-noise-params 3))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                Utility Todo move                                                 ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(provide (all-defined-out))