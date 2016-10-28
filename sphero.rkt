#lang racket

(require racket/system)
(require "sphero-api.rkt")
(require "colors.rkt")

;(define PORT "/dev/tty.Sphero-GYB-AMP-SPP")
(define PORT "sphero.txt")
(define sphero (connect-sphero PORT))

;; Variables for running.
(define speed 20)
(define count 2)

;; Square makes Sphero run in a square for n times.
(define (square n)
  (when (> n 0)
      (begin  (color sphero '#:red)
              (roll-for  sphero speed 0 1)
              
              (color sphero '#:white)
              (roll-for  sphero speed 90 1)
              
              (color sphero '#:blue)
              (roll-for  sphero speed 180 1)
              
              (color sphero '#:orange)
              (roll-for  sphero speed 270 1)

              (square (- n 1)))))

(set-collision-detection sphero)
;; We need to stop sphero before we disconnect otherwise he will run forever.
(stop sphero)
(disconnect-sphero sphero)
