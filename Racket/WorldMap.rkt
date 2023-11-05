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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                Noise Composition                                              ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;We use Noise composition. The final noise source equals the sum of:
;amplitude_1 * noise(octave_1 * nx, octave_1 * ny) +
;amplitude_2 * noise(octave_2 * nx,octave_2 * ny)+
;...+
;amplitude_N * noise(octave_N * nx, octave_N * ny);
;Then we normalize the noise at the end using the sum of the amplitudes
;Represents the data we need for noise composition.
;Stores a list of amplitudes, octaves, and the total amplitude
(struct noise-composition-params (amplitudes octaves total-amplitude ))

;Used to create the noise-composition-params type struct
(define (create-noise-composition-params amplitudes octaves)
  (noise-composition-params amplitudes octaves (apply + amplitudes))) 

;Creates the noise-composition-params, given a number of octaves
(define (build-noise-composition-params num-octaves)
  (let ([amps '()]
        [octs '()])
    (for/list ([i (in-range 1 (+ num-octaves 1))])
      (let* ([octave (expt 2 (- i 1))]
             [amplitude (/ 1.0 octave)])
        (set! amps (append amps (list amplitude)))
        (set! octs (append octs (list octave)))))
    (create-noise-composition-params amps octs)))


(struct noise (function))


(define (noise-at noise x y)
  (noise-function noise) x y)

;Composes the noise by adding several noise sources together using octaves and amplitudes.
;Adds an offset through a seed so we don't get the same valeu every time. 
(define (compose-noise source x-cord y-cord)
  ;Gets a single noise sample
  ;Divides by the map width and height so we get values between 0-1
  (define (generate-octave-noise amplitude octave x-cord y-cord)
    (* amplitude (abs(perlin (* octave (/ x-cord MAP-WIDTH))
                             (* octave (/ y-cord MAP-HEIGHT))))))

  (define noise-values
    (for/list ([amp (noise-composition-params-amplitudes source)]
               [oct (noise-composition-params-octaves source)])
      (generate-octave-noise amp oct (+ x-cord (get-seed)) (+ y-cord (get-seed)))))

  
  (define normalized-noise (normalize-composition-noise (foldl + 0 noise-values) (noise-composition-params-total-amplitude source)))


  
normalized-noise)

;normalizes the noise so that we have values between 0 and 1 using the sum of the amplitudes
(define (normalize-composition-noise noise-value total-amplitude)
  (/ noise-value total-amplitude))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                Trigonometric Functions for Noise                                ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define (linspace start stop num)
  (for/list ([i (in-range num)])
    (+ start (* (/ i (- num 1.0)) (- stop start)))))




  (define width-sample-space (linspace 0  100 MAP-WIDTH))
  (define height-sample-space (linspace 0 100  MAP-HEIGHT))


(define (linear-modulated-noise min max slope x-cord y-cord)
  (define (linear-noise n slope) (* slope n))
  (define x-val (linear-noise x-cord slope))
  (define y-val (linear-noise y-cord slope))

  (abs (simplex (+ x-val (get-seed)) (+ y-val (get-seed))))

  
  )
;Gets an x and y sample from a sinusoid and then composes them
(define (sine-modulated-noise start-deg end-deg min max x-cord y-cord)

  ;Creates a sine modulated signal
(define (sine-modulation start-deg end-deg min max sample-num)
  (let ([radians (* (- end-deg start-deg) (/ pi 180))]
        [dev (/ (- max min) 2)]
        [base (/ (+ max min)2)])
    ;(normalize-sample (+ (* dev (sin (* radians sample-num))) base) min max)
     (+ (* dev (sin (* radians sample-num))) base) min max)
    )

  ( let ([x-point (sine-modulation start-deg end-deg min max (list-ref width-sample-space x-cord))]
        [y-point (sine-modulation start-deg end-deg min max y-cord)])
     (abs (perlin (+ x-point (get-seed) (+ y-point (get-seed)))))

     ))


;(plot (points (map vector samples data) #:color 'red))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                Drawing and Tile Related                                                   ;
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


(define (square-bump x y)
  (- 1.0(* (- 1 (expt x 2)) (- 1 (expt y 2)))))

(define (apply-reshape func x y noise-val)
  (let ([distance (func x y)])
    (/ (+ noise-val (- 1.0 distance)) 2)
    )
  )

; Creates a tile with a noise value. The noise-func is any kind of noise generation function
; That has at least an x-cord and a y-cord. All of the arguments before x-cord and y-cord are curried
;nx and ny are used as inputs to the distance function we're using
;At the moment using square bump distance
(define (create-noise-tile x-cord y-cord noise-func)
  (define nx (- (/ (* 2 x-cord) MAP-WIDTH) 1))
  (define ny (- (/ (* 2 y-cord) MAP-HEIGHT) 1))
  (define temp-noise (noise-func x-cord y-cord))
  (define final-noise (apply-reshape square-bump nx ny temp-noise))
  (hash 'x x-cord
        'y y-cord
        'tile-width (* x-cord TILE-SIZE)
        'tile-height (* y-cord TILE-SIZE)
        ;'noise (abs(simplex x-cord y-cord))))
        ;'noise (/ (+ temp-noise (- 1 distance)) 2)
        'noise temp-noise
  


        ))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                World Map Related                                                ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(define (split-list lst)
  (map (lambda (item) (list item))lst))

(define (prepare-noise-func noise-func)
  (lambda (x-cord y-cord)
    (noise-func noise-source x-cord y-cord)))

(define (prepare-noise-func2 noise-func)
  (lambda (x-cord y-cord)
    (noise-func 0 360 1 10000 x-cord y-cord)))

(define (prepare-noise-func3 noise-func)
  (lambda (x-cord y-cord)
    (noise-func 0 100 1  x-cord y-cord)))



(define (initialize-world-map)
  (define world-map (make-vector MAP-WIDTH))
  (for ([x-cord (in-range MAP-WIDTH)])
    (define row (make-vector MAP-HEIGHT))
    (for ([y-cord (in-range MAP-HEIGHT)])
      ;(vector-set! row y-cord (create-noise-tile x-cord y-cord (prepare-noise-func-test sine-noise-args sine-modulated-noise))))
         (define temp-tile (create-noise-tile x-cord y-cord (prepare-noise-func compose-noise)))

          
        (vector-set! row y-cord temp-tile))
       ;(vector-set! row y-cord (create-noise-tile x-cord y-cord (prepare-noise-func3 linear-modulated-noise))))
    (vector-set! world-map x-cord row))
  world-map)


(define noise-source (build-noise-composition-params 3))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                Utility Todo move                                                 ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(provide (all-defined-out))