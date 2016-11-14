#lang racket

;;   ____                                          _     
;;  / ___|___  _ __ ___  _ __ ___   __ _ _ __   __| |___ 
;; | |   / _ \| '_ ` _ \| '_ ` _ \ / _` | '_ \ / _` / __|
;; | |__| (_) | | | | | | | | | | | (_| | | | | (_| \__ \
;;  \____\___/|_| |_| |_|_| |_| |_|\__,_|_| |_|\__,_|___/

(require "packets.rkt")
(require "sphero-commands.rkt")

(provide cmd-roll cmd-color cmd-set-cd)

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
        (SEQ      seq)
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
        (SEQ      seq)
        ;; data
        (COLOR    (list red green blue)) ;; Reverse 
        (FLAG     #x01)) 
    (make-packet SOP2 DID CID SEQ (append COLOR (list FLAG)))))


;; Packet format
;; +-----+-----+-----+------+
;; | DID | CID | SEQ | DLEN |
;; +-----+-----+-----+------+
;; | 02h | 22h |     | 01h  |
;; +-----+-----+-----+------+

(define (cmd-color-get seq)
  (let ((SOP2     #xFE)
        (DID      #x02)
        (CID      CMD_GET_RGB_LED)
        (SEQ      seq)) 
    (make-packet SOP2 DID CID SEQ '())))




;; Packet format
;; +-----+-----+-----+------+------+-----+------+-----+------+------+
;; | DID | CID | SEQ | DLEN | Meth | Xt  | Xspd | Yt  | Yspd | Dead |
;; +-----+-----+-----+------+------+-----+------+-----+------+------+
;; | 02h | 12h |     | 07h  | val  | val | val  | val | val  | val  |
;; +-----+-----+-----+------+------+-----+------+-----+------+------+

;; +------------+------------------------------------------------------------------------------------------------------------------------------------------------------+
;; | Meth       | Detection method type to use. Currently the only method supported is 01h. Use 00h to completely disable this service.                                |
;; +------------+------------------------------------------------------------------------------------------------------------------------------------------------------+
;; | Xt, Yt     | An 8-bit settable threshold for the X (left/right) and Y (front/back) axes of Sphero. A value of 00h disables the contribution of that axis.         |
;; +------------+------------------------------------------------------------------------------------------------------------------------------------------------------+
;; | Xspd, Yspd | An 8-bit settable speed value for the X and Y axes. This setting is ranged by the speed, then added to Xt, Yt to generate the final threshold value. |
;; +------------+------------------------------------------------------------------------------------------------------------------------------------------------------+
;; | Dead       | An 8-bit post-collision dead time to prevent retriggering; specified in 10ms increments.                                                             |
;; +------------+------------------------------------------------------------------------------------------------------------------------------------------------------+

(define (cmd-set-cd x-threshold y-threshold x-speed y-speed dead-time seq)
  (let* ((SOP2     #xFE)
         (DID      #x02)
         (CID      CMD_SET_COLLISION_DET)
         (SEQ      seq)
         ;; data
         (METH     #x01)
         (DATA    (list METH x-threshold x-speed y-threshold y-speed dead-time))) 
    (make-packet SOP2 DID CID SEQ (cons METH DATA))))
