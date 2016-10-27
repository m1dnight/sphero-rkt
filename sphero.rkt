#lang racket

(require racket/system)

;;;;;;;;;;;;;
;; Helpers ;;
;;;;;;;;;;;;;

;; Pretty print a number in hexadecimal.
(define (hex-format a-number)
  (define digits "0123456789ABCDEF")
  (string (string-ref digits (quotient a-number 16))
          (string-ref digits (modulo a-number 16))))

;; Print a packet
(define (pprint-packet packet)
  (map (lambda (byte)
         (let ((v (hex-format byte)))
           (display (format "~s " v))))
       packet))

;;;;;;;;;;;;;;;;;;;;;;;;
;; Checksum generator ;;
;;;;;;;;;;;;;;;;;;;;;;;;

(define (checksum bytes)
  (bitwise-xor
   (modulo
    (apply + (drop bytes 2))
    256)
   #xFF))

;;;;;;;;;;;;;;;;;;;;;;;
;; Connect to Sphero ;;
;;;;;;;;;;;;;;;;;;;;;;;

(define port "/dev/tty.Sphero-GYB-AMP-SPP")

(displayln "Opening output file..")
(define out (open-output-file port #:mode 'binary #:exists 'append))
(displayln "Opened output file")

(displayln "Opening input file..")
(define in  (open-input-file port #:mode 'binary))
(displayln "Opened input file")

;;;;;;;;;;;;;;;;;;;;;;;;;
;; Packet construction ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

;; Header
(define SOP1 #xFF)
(define SOP2 #xFE)
(define DID  #x02)
(define CID  #x20)
(define SEQ  #x00) ;; A counter for the number of messages we have sent already.

(define HEAD (list SOP1 SOP2 DID CID SEQ))

;; Payload

(define DLEN       #x05)
(define COLOR      (list #x00 #x80 #x00))
(define PERSISTENT (list #x01))
(define DATA       (append COLOR PERSISTENT))
(define BODY       (cons DLEN DATA))

;; Checksum

(define CHK (checksum (append HEAD BODY)))

;; Packet

(define PACKET (append HEAD BODY (list CHK)))

;;;;;;;;;;;;;;;;;;;;
;; Sending packet ;;
;;;;;;;;;;;;;;;;;;;;

(displayln HEAD)
(displayln BODY)
(displayln CHK)

(displayln PACKET)
;;(pprint-packet PACKET)

(define bytes (list->bytes PACKET))
(displayln bytes)

(displayln (write-bytes (list->bytes PACKET) out))



;;;;;;;;;;;;;;;;;;;;;;
;; Close connection ;;
;;;;;;;;;;;;;;;;;;;;;;

(displayln "Closing..")
(sleep 10)
(close-output-port out)
(close-input-port in)
