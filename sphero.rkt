#lang racket

(require racket/system)
(require "sphero-api.rkt")

;; Port for the Sphero bluetooth connection.
(define PORT "/dev/tty.Sphero-GYB-AMP-SPP")
;(define PORT "sphero.txt")

;; Create a connection to the Sphero.
(define sphero (connect-sphero PORT))

(color sphero 255 0 0)
;(roll sphero 150 0)

(sleep 20)

(disconnect-sphero sphero)



















































;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; Packet construction: Set Color ;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ;; Header
;; (define SOP1 #xFF)
;; (define SOP2 #xFE)
;; (define DID  #x02)
;; (define CID  #x20)
;; (define SEQ  #x00) ;; A counter for the number of messages we have sent already.

;; (define HEAD (list SOP1 SOP2 DID CID SEQ))

;; ;; Payload

;; (define DLEN       #x05)
;; (define COLOR      (list #x00 #x00 #xFF)) ; Actually Blue Green Red
;; (define PERSISTENT (list #x01))
;; (define DATA       (append COLOR PERSISTENT))
;; (define BODY       (cons DLEN DATA))

;; ;; Checksum

;; (define CHK (checksum (append HEAD BODY)))

;; ;; Packet

;; (define COlOR (append HEAD BODY (list CHK)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Packet construction: Roll ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ;; Header
;; (define SOP1 #xFF) ;; Start of the packet
;; (define SOP2 #xFE) ;; Message options (see: "SOP2 bitfield encoding" at https://sdk.sphero.com/api-reference/api-packet-format/)
;; (define DID  #x02) ;; Can be #x02 or #x00. Differs per command. (see https://sdk.sphero.com/api-reference/api-quick-reference/)
;; (define CID  #x30) ;; Command id (#x30 = rolling)
;; (define SEQ  #x00) ;; A counter for the number of messages we have sent already.

;; (define HEAD (list SOP1 SOP2 DID CID SEQ))

;; ;; Payload

;; (define DLEN       #x05)                  ;; Length of the arguments + checksum 
;; (define SPEED      (list #x32 #x00 #x64)) ;; Speed (1 byte) | Heading  (2 bytes)
;; (define STATE      (list #x01))           ;; Whatever the fuck this means.
;; (define DATA       (append SPEED STATE))  
;; (define BODY       (cons DLEN DATA))

;; ;; Checksum

;; (define CHK (checksum (append HEAD BODY)))

;; ;; Packet

;; (define ROLL (append HEAD BODY (list CHK)))


