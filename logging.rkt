#lang racket

(provide log-error log-info log-warning)

(define (log-error msg)
  (displayln
   (format "~a :: ERROR: ~a" msg (~a (current-seconds) #:max-width 10))))

(define (log-info msg)
  (displayln
   (format "INFO : ~a" msg)))

(define (log-warning msg)
  (displayln
   (format "WARN : ~a" msg)))

