#lang racket/gui
(require racket/include)
(require racket/file)
(require  noise)
(require plot)



#|________________________________Definitions_______________________________________________

Global defs that will be used throughout the file
____________________________________________________________________________________________________|#


(define MAP-WIDTH 50)
(define MAP-HEIGHT 50)
(define TILE-SIZE 8)

(define (get-seed)
  (current-inexact-milliseconds))



#|________________________________"Biomes_______________________________________________

Handles biome selection
____________________________________________________________________________________________________|#




;Returns the png for a noise value
(define (get-biome-data noise-val)
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
  (let ([biome-data (get-biome-data height-value)])  
    (hash
     'name (car biome-data)
     'pngPath (cdr biome-data))))

(define (get-biome-png tile)(hash-ref (hash-ref tile 'biome) 'pngPath))

 
#|________________________Noise Composition _________________________
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
_______________________________________________________________|#



;Contains the parameters we use to compose noise
;Has a list of amplitudes and octaves which use in compose-noise
;Where we iterate over the list of amplitude and octaves
(struct noise-composition-params (amplitudes octaves total-amplitude ))


;Creates the noise-composition-params, which is based on
;the number of octaves. 
(define (build-noise-composition-params num-octaves)

  ;Used to create the noise-composition-params type struct
  (define (create-noise-composition-params amplitudes octaves)
    (noise-composition-params amplitudes octaves (apply + amplitudes))) 
  
  (let ([amps '()]
        [octs '()])
    (for/list ([i (in-range 1 (+ num-octaves 1))])
      (let* ([octave (expt 2 (- i 1))]
             [amplitude (/ 1.0 octave)])
        (set! amps (append amps (list amplitude)))
        (set! octs (append octs (list octave)))))
    (create-noise-composition-params amps octs)))


;Composes the noise by adding several noise sources together using octaves and amplitudes.
;Adds an offset through a seed so we don't get the same valeu every time.
;Generate-octave-noise gets ONE sample of noiss, given an amplitdue, octave, and coordinates
;noise-values calls generate-octave noise for all of the octaves and amplitudes
;Then we normalize it at the end 

(define (compose-noise source)
  (modulator (lambda (x y)
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


#|______________Trigonometric Functions for Noise_________________________


Applies modulation to a noise value
_________________________________________________________________________|#                             


;Basic implementation of numpy linspace
(define (linspace start stop num)
  (for/list ([i (in-range num)])
    (+ start (* (/ i (- num 1.0)) (- stop start)))))

;We need values to sample from. 
(define width-sample-space (linspace 0  100 MAP-WIDTH))
(define height-sample-space (linspace 0 100  MAP-HEIGHT))

(define (linear-modulated-noise min max slope)
  (modulator
   (lambda (x y)
     (define (linear-noise n slope) (* slope n)) ; Our linear function

     (let ([linear-x (linear-noise x slope)]
           [linear-y (linear-noise y slope)])
       (abs (simplex (+ (+ linear-x) (get-seed)) (+ (+ y linear-y) (get-seed))))))))

;Gets an x and y sample from a sinusoid to buiild nose
(define (sine-modulated-noise start-deg end-deg min max)
  (modulator
   (lambda (x y)
     (define (sine-modulation start-deg end-deg min max sample-num)
       (let ([radians (* (- end-deg start-deg) (/ pi 180))]
             [dev (/ (- max min) 2)]
             [base (/ (+ max min) 2)])
         (+ (* dev (sin (* radians sample-num))) base min max)))
      
     (let ([sine-x (sine-modulation start-deg end-deg min max (list-ref width-sample-space x))]
           [sine-y (sine-modulation start-deg end-deg min max y)])
       (abs (perlin (+ (+ x sine-x) (get-seed) (+ (+ y sine-y) (get-seed)))))))))


#|______________Other________________________


Applies modulation to a noise value
_________________________________________________________________________|#     




#|________________________________"Interface" for noise_______________________________________________

I want to experiment with a variety of noise generation and modification techniques.

A noise transformer requires the following:

1) Distance Applier: Any distance function that takes x,y as input
2) Modulator: A function that takes x,y as input and returns a noise value. Should work as long as
the function has the following form:
(define (function-name var-args)
  (modulator
   (lambda (x y)
3) todo comment on redist furge and redist exp later



____________________________________________________________________________________________________|#

;A noise transformer is a thing that takes a noise value
;And produces a new noise value
(struct noise-transformer
  ( [distance-applier #:mutable]
    [modulator #:mutable] ;todo rename the "Noise" struc tto modulator or something that makes sense giving this nbaming schem
    [redist-fudge #:mutable]
    [redist-expt #:mutable]))

(define (create-noise-transformer distance-applier modulator redist-furge redist-exp)
  (noise-transformer  distance-applier modulator redist-furge redist-exp))


(define (get-distance-applier transformer) (noise-transformer-distance-applier transformer))
(define (get-modulator transformer) (noise-transformer-modulator transformer))
(define (get-redist-fudge transformer) (noise-transformer-redist-fudge transformer))
(define (get-redist-expt transformer) (noise-transformer-redist-expt transformer))

(define (set-distance-applier transformer distance-applier) (set-noise-transformer-distance-applier! transformer distance-applier))
(define (set-modulator transformer modulator) (set-noise-transformer-modulator! transformer modulator))
(define (set-redist-fudge transformer redist-fudge) (set-noise-transformer-redist-fudge! transformer redist-fudge))
(define (set-redist-exp transformer redist-expt) (set-noise-transformer-redist-expt! transformer redist-expt))

;Applies the transformation to the noise at x,y
(define (transform-noise transformer x y)
  (define nx (- (/ (* 2 x) MAP-WIDTH) 1))
  (define ny (- (/ (* 2 y) MAP-HEIGHT) 1))
  (define temp-noise (expt (* (noise-at (get-modulator transformer) x y) (get-redist-fudge transformer) ) (get-redist-expt transformer)))
  (define final-noise (apply-reshape (get-distance-applier transformer) nx ny temp-noise))
  final-noise)



;A struct to store the modulator function so we can use a closure for variable args
(struct modulator (function))
(define (noise-at modulator x y)
  ((modulator-function modulator) x y))

;Current modulator functions
(define noise-composition (compose-noise (build-noise-composition-params 16)))
(define linear-stuff (linear-modulated-noise 1 1 2))
(define sine-noise (sine-modulated-noise 0 360 1 100))

;Distance functions
(define (square-bump x y)
  (- 1.0(* (- 1 (expt x 2)) (- 1 (expt y 2)))))

(define (euclidian-squared x y)
  (min 1 ( / (+ (expt x 2) (expt y 2)) (sqrt 2))))

(define (distance-squared x y)
  (- 1 (+ (expt x 2) (expt y 2))))

(define (no-distance-function x y) 1)


;Applies the distance function
;Checks to see if the function passed as a parameter is no-distance-function. Then it returns the original noise value
;I don't really like doing a conditional that checks for a specific procedure name, but that works for now. 
(define (apply-reshape func x y noise-val)
  (if (eq? func no-distance-function) noise-val
      (let ([distance (func x y)])
        (/ (+ noise-val (- 1.0 distance)) 2))))

#|_______________________________Drawing and Tile Related___________________________________________

Handles all the drawing operations
____________________________________________________________________________________________________|#


;Travereses the map and chooses which tile to draw
;Selects a tile based off a noise value.
;Doesn't send the tile we draw to the canvas, that's handled by draw-tile-png
(define (draw-tile-map canvas world-map)
  (define (draw-tile-png dc x y image-path) ;Sends the drawing command to the canvas
    (define image (read-bitmap image-path))
    (send dc draw-bitmap image x y))

  (define dc (send canvas get-dc))
  (for* ([x (in-range MAP-WIDTH)]
         [y (in-range MAP-HEIGHT)])
    (define tile (get-tile world-map x y))
    (draw-tile-png dc
                   (hash-ref tile 'tile-width)
                   (hash-ref tile 'tile-height)
                   (get-biome-png tile))))

;Returns the hash table at the coordinates
(define (get-tile grid x y)
  (vector-ref (vector-ref grid x) y))


#|_______________________________World Map Related___________________________________________


World map creation
____________________________________________________________________________________________________|#



;The noise transformer handles the noise generation
;The expt calculation for the temp noise. We use Math.pow(e * fudge_factor, exponent),
(define (create-noise-tile x-cord y-cord elevation-transformer moisture-transformer)

  (define elevation-noise (transform-noise elevation-transformer x-cord y-cord))
  (hash 'x x-cord
        'y y-cord
        'tile-width (* x-cord TILE-SIZE)
        'tile-height (* y-cord TILE-SIZE)
        'biome (create-biome elevation-noise)))


;Creates the world map using a noise function
(define (initialize-world-map elevation-transformer moisture-transformer)
  (define world-map (make-vector MAP-WIDTH))
  (for ([x-cord (in-range MAP-WIDTH)])
    (define row (make-vector MAP-HEIGHT))
    (for ([y-cord (in-range MAP-HEIGHT)])

      (define temp-tile (create-noise-tile x-cord y-cord elevation-transformer moisture-transformer))
      (vector-set! row y-cord temp-tile))

    (vector-set! world-map x-cord row))
  world-map)


(provide (all-defined-out))