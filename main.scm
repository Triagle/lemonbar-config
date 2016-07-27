(require-extension gochan miscmacros shell toml defstruct pty)
(use srfi-1 srfi-13 srfi-98 posix regex utils)
(define jobs (gochan))
(define res (gochan))
(defstruct bar-segment
  dynamic command reload static)
(define configuration
  (read-toml (read-all "config.toml")))
(print configuration)
(define bar (cdr (assoc 'bar (cdr (assoc 'config configuration)))))
(define items (let loop ((configuration-no-header (remove (lambda (item)
                                                            (equal? item (assoc 'config configuration))) configuration))
                         (id 0)
                         (acc '()))
                (if (null? configuration-no-header)
                    acc
                    (loop (cdr configuration-no-header) (+ id 1) (cons (cons id (alist->bar-segment (cdar configuration-no-header))) acc)))))
(define (watch-and-send cmd id chan)
  (call-with-pty-process-io
      cmd
    (lambda (in out pid)
      (letrec ((get-next (lambda ()
                           (let ((v (read-line in)))
                             (if (not (eof-object? v))
                                 (gochan-send chan (cons id v)))
                             (get-next)
                             ))))
        (get-next)))))

(define (bar-segment->chan id chan segment)
  (string-trim-both (watch-and-send (bar-segment-command segment) id chan)))
(define (bar-segment->string segment)
  (string-trim-both (capture (,(bar-segment-command segment)))))
(define (worker jobs results)
  (gochan-for-each jobs
                   (lambda (segment)
                     (let ((cont #t))
                       (let* ((seg-struct (cdr segment))
                              (poll (bar-segment-reload seg-struct))
                              (dynamic (equal? (bar-segment-dynamic seg-struct) "true"))
                              (static (or (equal? (bar-segment-reload seg-struct) "true") (bar-segment-static seg-struct))))
                         (if dynamic
                             (watch-and-send (bar-segment-command seg-struct) (car segment) results)
                             (let ((cont #t))
                               (while cont
                                 (gochan-send results (cons (car segment) (bar-segment->string seg-struct)))
                                 (if static
                                     (set! cont #f)
                                     (thread-sleep! poll))))))))))
(define job-list
  (map
   (lambda (x) (thread-start! (make-thread (cut worker jobs res) x)))
   (iota (length items))))

(define (start-bar)
  (let loop ((items items))
    (if (null? items)
        #t
        (begin
          (gochan-send jobs (car items))
          (loop (cdr items)))))
  (let loop ((bar-status '()) (prev-status ""))
    (let* ((update (gochan-receive res))
           (bar-status (sort (cons update (remove (lambda (x) (= (car x) (car update))) bar-status)) (lambda (x y) (< (car x) (car y)))))
           (bar-status-string (string-join (map cdr bar-status))))
      (if (not (string=? prev-status bar-status-string))
          (print bar-status-string))
      (loop bar-status bar-status-string))))
(start-bar)
