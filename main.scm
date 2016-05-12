(require-extension forcible)
(require-extension shell)
(use srfi-1)
(use srfi-98)
(use posix)
(use regex)
(define-syntax defrule
  (syntax-rules ()
    [(defrule (name opener))
     (define (name str) (string-append "%{" opener "}" str))]
    [(defrule (name opener closer))
     (define (name str) (string-append "%{" opener "}" str "%{" closer "}"))]
    [(defrule (name arg opener closer))
     (define (name arg str) (string-append "%{" opener arg "}" str "%{" closer "}"))]))
(define-syntax defrules
  (syntax-rules ()
    [(_ rule) (defrule rule)]
    [(_ rule rules* ...)
     (begin (defrule rule)
            (defrules rules* ...))]))
(define-syntax defbarsegments
  (syntax-rules ()
    [(_ form) (lambda () form)]
    [(_ form forms* ...)
     (list (defbarsegments form) (defbarsegments forms* ...))]))
(define-syntax defbar
  (syntax-rules ()
    [(_ forms* ... timeout)
     (run-loop (flatten (defbarsegments forms* ...)) timeout "")]))
(defrules
  (:l "l")
  (:c "c")
  (:r "r")
  (:R "R" "R")
  (:O width "O" "O-")
  (:B colour "B" "B-")
  (:T index "T" "T-")
  (:U colour "U" "U-")
  (:F colour "F" "F-")
  (:S dir "S" "Sf"))
;; The %{Abutton:command} needs to be defined separately
(define :% "%%")
(define (:A str button-name command)
  (string-append "%{A" button-name ":" command "}" str "%{A}"))
(define (update-sections futures past-string)
  (let ((output (foldr string-append "" (map force futures))))
    (when (not (string=? past-string output))
      (print output)
      (flush-output))
    output))
(define (run-loop functions timeout string)
  (let ((updated-value (update-sections (map (lambda (f) (future (f))) functions) string)))
    (sleep timeout)
    (run-loop functions timeout updated-value)))




;;;;;;;;;;;;;;
#|
 _                                   __ _
| |__   __ _ _ __    ___ ___  _ __  / _(_) __ _
| '_ \ / _` | '__|  / __/ _ \| '_ \| |_| |/ _` |
| |_) | (_| | |    | (_| (_) | | | |  _| | (_| |
|_.__/ \__,_|_|     \___\___/|_| |_|_| |_|\__, |
                                          |___/
|#
;;;;;;;;;;;;;;



;; Get remaining system memory from the free -m command
(define (get-free-memory)
  (cadr (grep "[0-9]+" (string-split (capture (free --mega))))))
(define (get-free-cpu)
  (number->string (inexact->exact (round  (- 100 (string->number (last (string-split (capture (mpstat)))))) ))))
(define (get-current-song)
  (string-trim-both (capture (mpc current))))
(define (get-host-home)
  (get-environment-variable "USER"))
(define (get-kernel-version)
  (car (string-search "\\d+\\.\\d+" (capture (uname -r)))))
(define (get-time)
  (string-trim-both (capture (date +%I:%M))))
(defbar
  (:l
   (string-append
    "  "
    (get-kernel-version)
    (:F "#96b5b4" "   ")
    (get-free-cpu)
    "% "))
  (:c (get-current-song))
  (:r
   (string-append
    (:F "#bf616a" "  ")
    (get-free-memory)
    "MB  "
    (:F "#a3be8c" " ")
    (get-time)
    " ")) 1)
