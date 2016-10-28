#lang racket

;;   ____                                          _     
;;  / ___|___  _ __ ___  _ __ ___   __ _ _ __   __| |___ 
;; | |   / _ \| '_ ` _ \| '_ ` _ \ / _` | '_ \ / _` / __|
;; | |__| (_) | | | | | | | | | | | (_| | | | | (_| \__ \
;;  \____\___/|_| |_| |_|_| |_| |_|\__,_|_| |_|\__,_|___/

(require "packets.rkt")
(require "sphero-commands.rkt")

(provide cmd-roll cmd-color)

;; Each function in this module generates bytes to send over the wire
;; to the Sphero.

;; Packet format
;; +-----+-----+-----+------+-------+---------+---------+-------+
;; | DID | CID | SEQ | DLEN | Speed | Heading | Heading | STATE |
;; +-----+-----+-----+------+-------+---------+---------+-------+
;; | 02h | 30h |     | 05h  | val   |         |         | val   |
;; +-----+-----+-----+------+-------+---------+---------+-------+

(define (cmd-roll speed heading seq)
  (let ((SOP2     #xFE)
        (DID      #x02)
        (CID      CMD_ROLL)
        (SEQ      #x00)
        ;; data
        (SPEED    speed)
        (HEADING1 (quotient heading   256))
        (HEADING2 (modulo heading 256))
        (STATE    #x01)) ;; Unsure what the "state" flag does. Works when we put it on 1.
    (make-packet SOP2 DID CID SEQ (list SPEED HEADING1 HEADING2 STATE))))

;; Packet format
;; +-----+-----+-----+------+-------+-------+-------+------+
;; | DID | CID | SEQ | DLEN |  RED  | GREEN | BLUE  | FLAG |
;; +-----+-----+-----+------+-------+-------+-------+------+
;; | 02h | 20h |     | 05h  | value | value | value |      |
;; +-----+-----+-----+------+-------+-------+-------+------+
;; "If FLAG is true, the value is also saved as the "user LED color"
;; which persists across power cycles and is rendered in the gap
;; between an application connecting and sending this command."

(define (cmd-color red green blue seq)
  (let ((SOP2     #xFE)
        (DID      #x02)
        (CID      CMD_SET_RGB_LED)
        (SEQ      #x00)
        ;; data
        (COLOR    (list red green blue)) ;; Reverse 
        (FLAG     #x01)) 
    (make-packet SOP2 DID CID SEQ (append COLOR (list FLAG)))))
    
