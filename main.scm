(require-extension forcible)
(require-extension shell)
(use srfi-1)
(use srfi-98)
(use posix)
(use regex)
(use utils)
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
(define (:A button-name command str)
  (string-append "%{A" button-name ":" command ":}" str "%{A}"))
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

;; Some convenience functions
(define (ellipize string length)
  (if (> (length string) length)
      (let ((string (substring string 0 (- length 3))))
        (string-append string "..."))
      string))
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
(define (get-workspace-number)
  (substring (car (filter (lambda (x) (member (substring x 0 1) '("F" "O")))
                          (string-split (cadr (string-split (string-trim-both (capture (bspc wm -g))) "M")) ":"))) 1))
(define (get-current-song)
  (string-trim-both (capture (mpc current))))
(define (get-kernel-version)
  (car (string-search "\\d+\\.\\d+" (capture (uname -r)))))
(define (get-wifi-ssid)
  (let ((ssid (string-trim-both (capture (iwgetid -r)))))
    (if (equal? ssid "Orcon-Wireless")
        "Home"
        (ellipize ssid 20))))
(define (get-wifi-strength)
  (let ((wifi-strength (string->number (cadr (string-split (car (string-search "\\d{4}\\s+\\d{2}" (read-all "/proc/net/wireless"))))))))
    (cond
     ((= wifi-strength 0) " ")
     ((<= wifi-strength 34) " ")
     ((<= wifi-strength 67) " ")
     ((<= wifi-strength 100) " "))))
(define (get-time)
  (string-trim-both (capture (date +%I:%M))))
(defbar
  (:l
   (string-append
    "  "
    (get-kernel-version)
    (:F "#96b5b4" "   ")
    (get-workspace-number)))
  (:c (get-current-song))
  (:r
   (string-append
    (:F "#bf616a" (get-wifi-strength))
    (get-wifi-ssid)
    " "
    (:F "#a3be8c" " ")
    (get-time)
    " ")) 1)
