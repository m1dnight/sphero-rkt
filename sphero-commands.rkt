#lang racket

;;  ____        _                    
;; / ___| _ __ | |__   ___ _ __ ___  
;; \___ \| '_ \| '_ \ / _ \ '__/ _ \ 
;;  ___) | |_) | | | |  __/ | | (_) |
;; |____/| .__/|_| |_|\___|_|  \___/ 
;;       |_|                         
;;   ____                                          _     
;;  / ___|___  _ __ ___  _ __ ___   __ _ _ __   __| |___ 
;; | |   / _ \| '_ ` _ \| '_ ` _ \ / _` | '_ \ / _` / __|
;; | |__| (_) | | | | | | | | | | | (_| | | | | (_| \__ \
;;  \____\___/|_| |_| |_|_| |_| |_|\__,_|_| |_|\__,_|___/

(provide syntax (all-defined-out))

;; See https://sdk.sphero.com/api-reference/api-quick-reference/
;; DID = #x02

(define CMD_SET_CAL                  #x01)
(define CMD_SET_STABILIZ             #x02)
(define CMD_SET_ROTATION_RATE        #x03)
(define CMD_REENABLE_DEMO            #x06)
(define CMD_SELF_LEVEL               #x09)
(define CMD_SET_DATA_STREAMING       #x11)
(define CMD_SET_COLLISION_DET        #x12)
(define CMD_LOCATOR                  #x13)
(define CMD_SET_ACCELERO             #x14)
(define CMD_READ_LOCATOR             #x15)
(define CMD_SET_RGB_LED              #x20)
(define CMD_SET_BACK_LED             #x21)
(define CMD_GET_RGB_LED              #x22)
(define CMD_ROLL                     #x30)
(define CMD_BOOST                    #x31)
(define CMD_MOVE                     #x32)
(define CMD_SET_RAW_MOTORS           #x33)
(define CMD_SET_MOTION_TO            #x34)
(define CMD_SET_OPTIONS_FLAG         #x35)
(define CMD_GET_OPTIONS_FLAG         #x36)
(define CMD_SET_TEMP_OPTIONS_FLAG    #x37)
(define CMD_GET_TEMP_OPTIONS_FLAG    #x38)
(define CMD_RUN_MACRO                #x50)
(define CMD_SAVE_TEMP_MACRO          #x51)
(define CMD_SAVE_MACRO               #x52)
(define CMD_INIT_MACRO_EXECUTIVE     #x54)
(define CMD_ABORT_MACRO              #x55)
(define CMD_MACRO_STATUS             #x56)
(define CMD_SET_MACRO_PARAM          #x57)
(define CMD_APPEND_TEMP_MACRO_CHUNK  #x58)
(define CMD_ERASE_ORBBAS             #x60)
(define CMD_APPEND_FRAG              #x61)
(define CMD_EXEC_ORBBAS              #x62)
(define CMD_ABORT_ORBBAS             #x63)
(define CMD_ANSWER_INPUT             #x64)
