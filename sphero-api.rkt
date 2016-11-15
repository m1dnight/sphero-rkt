#lang racket

(require "connection.rkt")
(require "commands.rkt")
(require "logging.rkt")
(require "colors.rkt")

(provide connect-sphero)
(provide disconnect-sphero)
(provide roll)
(provide color)
(provide stop)
(provide roll-for)
(provide set-collision-detection)
(provide color-rgb?)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; BASIC OPERATIONS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; connect-sphero requires you to pass a port and then a valu is
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
  (let* ((seq    (next-seq conn))
         (packet (cmd-roll speed heading seq))) ;; seq is hardcoded atm
    (log-info (format "CMD ~a roll ~a ~a" seq speed heading))    
    (send-packet conn packet)))

;; Changes the color of the Sphero.
;; conn : A Sphero connection created using above functions.
;; red  : red value
;; green: green value
;; blue : blue value
(define (color-rgb conn red green blue)
  (let* ((seq    (next-seq conn))
         (packet (cmd-color red green blue seq)))
    (log-info (format "CMD ~a set RGB ~a ~a ~a" seq red green blue))        
    (send-packet conn packet)))


;; Retrieves the color of the Sphero.
;; conn : A Sphero connection created using above functions.
(define (color-rgb? conn)
  (let* ((seq    (next-seq conn))
         (packet (cmd-color-get seq)))
    (log-info (format "CMD ~a get RGB" seq))            
    (send-packet conn packet)))

;; Enables the call of collisions on the Sphero.
(define (set-collision-detection conn)
  (define x-threshold #x40)
  (define y-threshold #x00)
  (define x-speed     #xFF)
  (define y-speed     #xFF)
  (define dead-time   #xFF)
  (let ((packet (cmd-set-cd x-threshold y-threshold x-speed y-speed dead-time 0)))
    (send-packet conn packet)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; DERIVED OPERATIONS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Derived operations use the basic API operations to create some
;; obvious abstractions.

;; Stops the Sphero from rolling any further. Provided for
;; convenience.
(define (stop conn)
  (roll conn 0 0))

;; Roll for n seconds.
(define (roll-for conn speed heading interval)
  (roll conn speed heading)
  (sleep interval))

(define (color conn c)
  (let ((valid? (is-color? c)))
    (if valid?
        (apply color-rgb (cons conn valid?))
        (log-error "Invalid color provided!"))))
