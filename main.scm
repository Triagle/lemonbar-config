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
     (run-loop (flatten (defbarsegments forms* ...)) timeout)]))
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
(define (:A str button-name command)
  (string-append "%{A" button-name ":" command "}" str "%{A}"))
(define (update-sections futures)
  (for-each (lambda (f) (printf "~a" (force f))) futures)
  (newline)
  (flush-output))
(define (run-loop functions timeout)
  (update-sections (map (lambda (f) (future (f))) functions))
  (sleep timeout)
  (run-loop functions timeout))






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
  (format "~A" (inexact->exact (round  (- 100 (string->number (last (string-split (capture (mpstat)))))) ))))




(defbar
  (:l
   (string-append
    (:F "#bf616a" " ")
    (:F "#c0c5c3" (get-free-memory))))
  (:c "nah")
  (:r
    (:F "#c0c5c3" (get-free-cpu)))
  5)
