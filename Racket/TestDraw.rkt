#lang racket/gui

(define (draw-image dc x y image-path)
  (define image (read-bitmap image-path))
  (send dc draw-bitmap image x y))

(define frame (new frame% [label "Image Drawing Example"]
                         [width 400]
                         [height 300]))

(define canvas (new canvas% [parent frame]
                           [paint-callback
                            (lambda (canvas dc)
                              ; Draw the image at coordinates (x, y)
                              (draw-image dc 100 100 "forestgrass1.png"))]))

(send frame show #t)