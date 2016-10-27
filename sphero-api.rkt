#lang racket

(require "connection.rkt")
(require "commands.rkt")
(require "logging.rkt")

(provide connect-sphero)
(provide disconnect-sphero)
(provide roll)
(provide color)
(provide stop)


;; connect-sphero requires you to pass a port and then a value is
;; returned representing the connection.
(define (connect-sphero port)
  (log-info (format "Connecting Sphero on port ~a" port))
  (sphero-connect port))


;; Disconnects from the Sphero and cleans up any open connections.
(define (disconnect-sphero sphero)
  (log-info "Disconnecting Sphero")
  (sphero-disconnect sphero))


;; Roll expectes three parameters.
;; conn   : A Sphero connection created using above functions.
;; speed  : A value between zero and 255.
;; heading: Degrees relative to the Sphero's calibration. I.e., between 0 and 389.
(define (roll conn speed heading)
  (let ((packet (cmd-roll speed heading 0))) ;; seq is hardcoded atm
    (send-packet conn packet)))


;; Stops the Sphero from rolling any further. Provided for
;; convenience.
(define (stop conn)
  (roll conn 0 0))


;; Changes the color of the Sphero.
;; conn : A Sphero connection created using above functions.
;; red  : red value
;; green: green value
;; blue : blue value
(define (color conn red green blue)
  (let ((packet (cmd-color red green blue 0)))
    (send-packet conn packet)))
