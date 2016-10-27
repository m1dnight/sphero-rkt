#lang racket

;;   ____                            _   _             
;;  / ___|___  _ __  _ __   ___  ___| |_(_) ___  _ __  
;; | |   / _ \| '_ \| '_ \ / _ \/ __| __| |/ _ \| '_ \ 
;; | |__| (_) | | | | | | |  __/ (__| |_| | (_) | | | |
;;  \____\___/|_| |_|_| |_|\___|\___|\__|_|\___/|_| |_|

(provide sphero-connect)
(provide sphero-disconnect)
(provide send-packet)

(require "packets.rkt")
(require "logging.rkt")
(require file/sha1)

;; We need a data type that will represent the connections to the
;; Sphero. We have an incoming and an outgoing connection.
(struct connection (in out))

;; sphero-connect takes in a port and then creates an input and an
;; output port and returns in the form of a struct. Normally this
;; struct should never be exposed via the API.
(define (sphero-connect port)
  (let ((in-port  (open-input-file port #:mode 'binary))
        (out-port (open-output-file port #:mode 'binary #:exists 'append)))
    (log-info (format "Created input and output port on ~a" port))
    (connection in-port out-port)))

;; sphero-disconnect takes in the struct created above and closes both
;; the input and output port.
(define (sphero-disconnect connection)
  (let ((in-port  (connection-in connection))
        (out-port (connection-out connection)))
    (log-info "Closing input port for Sphero")
    (close-input-port  in-port)
    (log-info "Closing output port for Sphero")    
    (close-output-port out-port)))

;; Send takes in *bytes* and transmits them over the connection to the
;; sphero. Afterwards the output is flushed to be sure, and the number
;; of transmitted bytes is returned.
(define (send-packet conn bytes)
  (let* ((out  (connection-out conn))
         (sent (write-bytes bytes out)))
    (begin (log-info (format "Sent packet: ~a" (bytes->hex-string bytes)))
           (flush-output out))))
