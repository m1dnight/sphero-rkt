#lang racket

;;   ____                  ____                                          _     
;;  / ___|___  _ __ ___   / ___|___  _ __ ___  _ __ ___   __ _ _ __   __| |___ 
;; | |   / _ \| '__/ _ \ | |   / _ \| '_ ` _ \| '_ ` _ \ / _` | '_ \ / _` / __|
;; | |__| (_) | | |  __/ | |__| (_) | | | | | | | | | | | (_| | | | | (_| \__ \
;;  \____\___/|_|  \___|  \____\___/|_| |_| |_|_| |_| |_|\__,_|_| |_|\__,_|___/
                                                                            

;; See https://sdk.sphero.com/api-reference/api-quick-reference/
;; DID = #x00

(define CMD_PING                #x01)
(define CMD_VERSION             #x02)
(define CMD_SET_BT_NAME         #x10)
(define CMD_GET_BT_NAME         #x11)
(define CMD_SET_AUTO_RECONNECT  #x12)
(define CMD_GET_AUTO_RECONNECT  #x13)
(define CMD_GET_PWR_STATE       #x20)
(define CMD_SET_PWR_NOTIFY      #x21)
(define CMD_SLEEP               #x22)
(define SET_INACTIVE_TIMER      #x25)
(define CMD_GOTO_BL             #x30)
(define CMD_RUN_L1_DIAGS        #x40)
(define CMD_RUN_L2_DIAGS        #x41)
