#lang racket/gui
(require racket/include)
(require  noise)
(require plot)






;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                Definitions                                                       ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define MAP-WIDTH 50)
(define MAP-HEIGHT 50)
(define TILE-SIZE 8)

(define (get-seed)
  (current-inexact-milliseconds))

(define (normalize-sample value min max)
  (/ (- value min) (- max min)))

#|
________________________________"Biomes_______________________________________________

____________________________________________________________________________________________________
|#



;Returns the png for a noise value
(define (get-noise-png noise-val)
  (cond
    [(< noise-val 0.1) (cons "ocean" "water1.png")]
    [(< noise-val 0.2) (cons "beach" "beachsand1.png")]
    [(< noise-val 0.3) (cons "forest" "tree1.png")]
    [(< noise-val 0.5) (cons "jungle" "jungle1.png")]
    [(< noise-val 0.7) (cons "taiga" "shrubland1.png")]
    [(< noise-val 0.9) (cons "desert" "deserttile1.png")]
    [else  (cons "snowlands" "snowground1.png")]))


;Creates a biome based on heeight
(define (create-biome  height-value)
  (let ([biome-data (get-noise-png height-value)])  
  (hash
   'name (car biome-data)
   'pngPath (cdr biome-data))))

(define (get-biome-png tile)(hash-ref (hash-ref tile 'biome) 'pngPath))

 

#|
________________________________"Interface" for noise_______________________________________________

This is to allow different implemetnations of noise. All of the implementations require an x and a y.
Any other parameters are up to the person implementing it

Implementations look like this

(define (function-name param-1 param-2...param-n)
  (noise (lambda (x y) ...))
____________________________________________________________________________________________________
|#


(struct noise (function))

(define (noise-at noise x y)
  ((noise-function noise) x y))




#|
________________________Noise Composition _________________________
The section below contains the functions that use noise composition
To generate a final noise output value


We use Noise composition. The final noise source equals the sum of:
amplitude_1 * noise(octave_1 * nx, octave_1 * ny) +
amplitude_2 * noise(octave_2 * nx,octave_2 * ny)+
...+
amplitude_N * noise(octave_N * nx, octave_N * ny)
Then we normalize the noise at the end using the sum of the amplitudes
Represents the data we need for noise composition.
Stores a list of amplitudes, octaves, and the total amplitud
_______________________________________________________________
|#


;Contains the parameters we use to compose noise
;Has a list of amplitudes and octaves which use in compose-noise
;Where we iterate over the list of amplitude and octaves
(struct noise-composition-params (amplitudes octaves total-amplitude ))

;Used to create the noise-composition-params type struct
(define (create-noise-composition-params amplitudes octaves)
  (noise-composition-params amplitudes octaves (apply + amplitudes))) 

;Creates the noise-composition-params, which is based on
;the number of octaves. 
(define (build-noise-composition-params num-octaves)
  (let ([amps '()]
        [octs '()])
    (for/list ([i (in-range 1 (+ num-octaves 1))])
      (let* ([octave (expt 2 (- i 1))]
             [amplitude (/ 1.0 octave)])
        (set! amps (append amps (list amplitude)))
        (set! octs (append octs (list octave)))))
    (create-noise-composition-params amps octs)))


#|
Composes the noise by adding several noise sources together using octaves and amplitudes.
Adds an offset through a seed so we don't get the same valeu every time.

Generate-octave-noise gets ONE sample of noiss, given an amplitdue, octave, and coordinates

noise-values calls generate-octave noise for all of the octaves and amplitudes

Then we normalize it at the end 
|#
(define (compose-noise source)

  (noise (lambda (x y)

  (define (generate-octave-noise amplitude octave x y) ;Divides by the map width and height so we get values between 0-1
    (* amplitude (abs(perlin (* octave (/ x MAP-WIDTH))
                             (* octave (/ y MAP-HEIGHT))))))
  (define noise-values
    (for/list ([amp (noise-composition-params-amplitudes source)]
               [oct (noise-composition-params-octaves source)])
      (generate-octave-noise amp oct (+ x (get-seed)) (+ y (get-seed)))))
           
  (define normalized-noise (normalize-composition-noise (foldl + 0 noise-values) (noise-composition-params-total-amplitude source)))
normalized-noise)))

;normalizes the noise so that we have values between 0 and 1 using the sum of the amplitudes
(define (normalize-composition-noise noise-value total-amplitude)
  (/ noise-value total-amplitude))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                Trigonometric Functions for Noise                                ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;Basic implementation of numpy linspace
(define (linspace start stop num)
  (for/list ([i (in-range num)])
    (+ start (* (/ i (- num 1.0)) (- stop start)))))

;We need values to sample from. 
(define width-sample-space (linspace 0  100 MAP-WIDTH))
(define height-sample-space (linspace 0 100  MAP-HEIGHT))

(define (linear-modulated-noise min max slope)
  (noise
    (lambda (x y)
      (define (linear-noise n slope) (* slope n)) ; Our linear function

      (let ([linear-x (linear-noise x slope)]
            [linear-y (linear-noise y slope)])
        (abs (simplex (+ (+ linear-x) (get-seed)) (+ (+ y linear-y) (get-seed))))))))

;Gets an x and y sample from a sinusoid to buiild nose
(define (sine-modulated-noise start-deg end-deg min max)
  (noise
    (lambda (x y)
      (define (sine-modulation start-deg end-deg min max sample-num)
        (let ([radians (* (- end-deg start-deg) (/ pi 180))]
              [dev (/ (- max min) 2)]
              [base (/ (+ max min) 2)])
          (+ (* dev (sin (* radians sample-num))) base min max)))
      
      (let ([sine-x (sine-modulation start-deg end-deg min max (list-ref width-sample-space x))]
            [sine-y (sine-modulation start-deg end-deg min max y)])
        (abs (perlin (+ (+ x sine-x) (get-seed) (+ (+ y sine-y) (get-seed)))))))))


;(plot (points (map vector samples data) #:color 'red))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                Drawing and Tile Related                                                   ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;Travereses the map and chooses which tile to draw
;Selects a tile based off a noise value.
;Doesn't send the tile we draw to the canvas, that's handled by draw-tile-png
(define (draw-tile-map canvas world-map)
  (define dc (send canvas get-dc))
  (for ([x (in-range MAP-WIDTH)])
    (for ([y (in-range MAP-HEIGHT)])
      (let ([tile (get-tile world-map x y)])
                (draw-tile-png dc (hash-ref tile 'tile-width)
                       (hash-ref tile 'tile-height ) (get-biome-png tile))))))




;Sends the drawing command to the canvas
(define (draw-tile-png dc x y image-path)
  (define image (read-bitmap image-path))
  (send dc draw-bitmap image x y))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                Basic Tile Related                                               ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;Returns the hash table at the coordinates
(define (get-tile grid x y)
  (vector-ref (vector-ref grid x) y))

;Square-bump distance function
(define (square-bump x y)
  (- 1.0(* (- 1 (expt x 2)) (- 1 (expt y 2)))))



;Applies the distance function
(define (apply-reshape func x y noise-val)
  (let ([distance (func x y)])
    (/ (+ noise-val (- 1.0 distance)) 2)))


; Creates a tile with a noise value. The noise-func is any kind of noise generation function
; That has at least an x-cord and a y-cord. All of the arguments before x-cord and y-cord are curried
;nx and ny are used as inputs to the distance function we're using
;At the moment using square bump distance
(define (create-noise-tile x-cord y-cord noise)
  (define nx (- (/ (* 2 x-cord) MAP-WIDTH) 1))
  (define ny (- (/ (* 2 y-cord) MAP-HEIGHT) 1))
  (define temp-noise (noise-at noise x-cord y-cord))
  (define final-noise (apply-reshape square-bump nx ny temp-noise))
  (hash 'x x-cord
        'y y-cord
        'tile-width (* x-cord TILE-SIZE)
        'tile-height (* y-cord TILE-SIZE)
        'biome (create-biome temp-noise)))



  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                World Map Related                                                ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define noise-composition (compose-noise (build-noise-composition-params 3)))
(define linear-stuff (linear-modulated-noise 1 1 2))
(define sine-noise (sine-modulated-noise 0 360 1 100))


;Creates the world map using a noise function
(define (initialize-world-map)
  (define world-map (make-vector MAP-WIDTH))
  (for ([x-cord (in-range MAP-WIDTH)])
    (define row (make-vector MAP-HEIGHT))
    (for ([y-cord (in-range MAP-HEIGHT)])

         (define temp-tile (create-noise-tile x-cord y-cord noise-composition))
        (vector-set! row y-cord temp-tile))

    (vector-set! world-map x-cord row))
  world-map)


;; Example usage:
(define-world-map-generator
  create-my-world
  [(with-noise-function noise-composition)
   (with-map-size 100 100)
   (with-seed 42)
   (with-distance-func square-bump)]
  [(add-biome 0.1 'ocean "water1.png")
   (add-biome 0.2 'beach "beachsand1.png")
   (add-biome 0.3 'forest "tree1.png")
   (add-biome 0.5 'jungle "jungle1.png")
   (add-biome 0.7 'taiga "shrubland1.png")
   (add-biome 0.9 'desert "deserttile1.png")
   (add-biome 1.0 'snowlands "snowground1.png")])


(provide (all-defined-out))