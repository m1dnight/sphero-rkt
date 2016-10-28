#lang racket

;;  ____            _        _       
;; |  _ \ __ _  ___| | _____| |_ ___ 
;; | |_) / _` |/ __| |/ / _ \ __/ __|
;; |  __/ (_| | (__|   <  __/ |_\__ \
;; |_|   \__,_|\___|_|\_\___|\__|___/

(provide make-packet)
(provide pprint-packet)
(provide hex-format)
(provide print-bytestring-hex)

;; +===========+==========================================+===================================================================+
;; | NAME      |  Description                             |  Description                                                      |
;; +===========+==========================================+===================================================================+
;; | SOP1      |  Start of Packet #1                      |  Always FFh                                                       |
;; +-----------+------------------------------------------+-------------------------------------------------------------------+
;; | SOP2      |  Start of Packet #2                      |  F8 to FFh encoding 4 bits of per-message options (see below)     |
;; +-----------+------------------------------------------+-------------------------------------------------------------------+
;; | DID       |  Device ID                               |  The virtual device this packet is intended for                   |
;; +-----------+------------------------------------------+-------------------------------------------------------------------+
;; | CID       |  Command ID                              |  The command code                                                 |
;; +-----------+------------------------------------------+-------------------------------------------------------------------+
;; | SEQ       |  Sequence Number                         |  This client field is echoed in the response for all synchronous  |
;; |           |                                          |  commands(and ignored by Sphero when SOP2 has bit 0 clear)        |
;; +-----------+------------------------------------------+-------------------------------------------------------------------+
;; | DLEN      |  Data Length                             |  The number of bytes following through the end of the packet      |
;; +-----------+------------------------------------------+-------------------------------------------------------------------+
;; | Data      |  Optional data to accompany the Command  |                                                                   |
;; +-----------+------------------------------------------+-------------------------------------------------------------------+
;; | CHK       |  Checksum                                |  The modulo 256 sum of all the bytes from the DID through the end |
;; |           |                                          |  of the data payload, bit inverted (1's complement)               |
;; +-----------+------------------------------------------+-------------------------------------------------------------------+

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Helper Functions

;; Pretty print a number in hexadecimal.
(define (hex-format a-number)
  (define digits "0123456789ABCDEF")
  (string (string-ref digits (quotient a-number 16))
          (string-ref digits (modulo a-number 16))))

;; Print a packet to a string.
(define (pprint-packet packet)
  (map (lambda (byte)
         (let ((v (hex-format byte)))
           v))
       packet))

;; Takes in a byte string and then prettyprints it as hexadecimal.
(define (print-bytestring-hex byte-string)
  (let* ((bs (bytes->list byte-string))
         (str (map hex-format bs)))
    str))

;; checksum generates a checksum for a package by summing all the
;; bytes starting (and including) from the DID, taking the sum of them
;; nodulo 256 and then inverting all the bits (1's complement).
(define (checksum bytes)
  (bitwise-xor
   (modulo
    (apply + (drop bytes 2))
    256)
   #xFF))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Packet Construction

;; make-packet takes in the different parts of a packet and generates
;; the bytes to send over the wite.
(define (make-packet SOP2 DID CID SEQ DATA)
  (let* ((SOP1  #xFF)
         (DLEN  (+ (length DATA) 1)) ;; Length of data is always plus 1 for the checksum.
         (HEAD  (list SOP1 SOP2 DID CID SEQ))
         (BODY  (cons DLEN DATA))
         (CHK   (checksum (append HEAD BODY)))
         (PACK  (append HEAD BODY (list CHK)))
         (BYTES (list->bytes PACK)))
    (log-info (format "Packet: ~a" PACK))
    BYTES))

