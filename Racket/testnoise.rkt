#lang racket

(define script-path "myscript.py")

(define-values (input output error)
  (process "python" script-path))
  
(define output-string (port->string output))
(define error-string (port->string error))

(process-wait)

(displayln (format "Output: ~a" output-string))
(displayln (format "Error: ~a" error-string))