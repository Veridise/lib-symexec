#lang rosette

(provide (rename-out [println-and-exit exit]
                     [println-and-error error]
                     [println-and-log log]
                     [assert-type typed]
                     [concrete-integer? cinteger?]
                     [concrete-natural? cnatural?])
         set-logmode)

;; Modes: 'none < 'some < 'verbose
(define LOG_MODE 'some)

(define (set-logmode m)
  (set! LOG_MODE m))

(define (can-log? mode)
  (or (equal? LOG_MODE 'verbose) (and (equal? LOG_MODE 'some) (equal? mode 'some))))

(define (concrete-integer? x)
  (and (concrete? x) (integer? x)))

(define (concrete-natural? x)
  (and (concrete-integer? x) (>= x 0)))

; used for forced break out of the execution
(define (println-and-exit #:mode [mode 'some] msg . fmts)
  (when (can-log? mode)
    (printf (format "[tokamak:exit] ~a\n" (apply format (cons msg fmts)))))
  (exit 0))

; used for throwing tokamak error with message
(define (println-and-error msg . fmts)
  (error (format "[tokamak:error] ~a\n" (apply format (cons msg fmts))))
  (error error))

(define (println-and-log #:mode [mode 'verbose] msg . fmts)
  (when (can-log? mode)
    (printf (format "[tokamak:log] ~a\n" (apply format (cons msg fmts))))
    (flush-output)))

; usually for debugging, asserting obj is one of types, otherwise print and exit
; typs is a list of type predicates
; (note) as long as there's a path that returns false **for all typs**, the exit function will be triggered
(define (assert-type-helper obj typs)
  (if (null? typs) #f (if ((car typs) obj) #t (assert-type-helper obj (cdr typs)))))

(define (assert-type obj . typs)
  (when (and (empty? (symbolics obj)) (not (assert-type-helper obj typs)))
    (println-and-error "type checking failed, required types are: ~a, obj is: ~v" typs obj)))
