#lang rosette

(require (prefix-in tokamak: "tokamak.rkt")
         "sym.rkt")

(provide (all-defined-out))

;; In Cairo, memory is stored on separate segments
;; For other languages (e.g. Yul), might not need this struct
(struct rv (seg off) #:transparent #:reflection-name 'rv)

;; Convert integer or rv to rv
(define (torv i)
  (if (integer? i) (rv 0 i) i))

;; Convert integer or rv to integer
(define (fromrv r)
  (if (integer? r) r (rv-off r)))

(define (rvadd self other)
  (let ([self (torv self)] [other (fromrv other)]) (rv (rv-seg self) (+ (rv-off self) other))))

(define (rvsub self other)
  (cond
    [(union? other) (for/all ([v other #:exhaustive]) (rvsub self v))]
    [else
     (let ([self (torv self)])
       (cond
         [(integer? other) (rv (rv-seg self) (- (rv-off self) other))]
         [else
          ; other is also rv
          (assert (equal? (rv-seg self) (rv-seg other)) "rv-sub segs mismatch")
          (- (rv-off self) (rv-off other))]))]))

(define (rvmod self other)
  (let ([self (torv self)] [other (fromrv other)]) (rv (rv-seg self) (modulo (rv-off self) other))))

(define (rveq self other)
  (&& (equal? (rv-seg self) (rv-seg other)) (equal? (rv-off self) (rv-off other))))

(define (rvlt self other)
  (cond
    [(integer? other) #f]
    [else ; rv
     (let ([seg0 (rv-seg self)] [seg1 (rv-seg other)] [off0 (rv-off self)] [off1 (rv-off other)])
       (if (< seg0 seg1) #t (if (equal? seg0 seg1) (if (< off0 off1) #t #f) #f)))]))

(define (rvle self other)
  (|| (rvlt self other) (rveq self other)))

(define (rvge self other) (compose not rvlt))

(define (rvgt self other) (compose not rvle))

(struct memory
        (data ; vector of vectors representing data segments
         nsegs ; number of data segments
         storage-var-ids ; list of storage variable names
         storage-vars ; vector of storage variable functions, same order as storage-var-ids
         frozen ; True if memory should not be written to
         ; Used for replays
         constraint-checks ; list of functions that perform checks on memory sets
         prime) ; int or #f if integers should be unbounded
  #:mutable
  #:transparent
  #:reflection-name 'memory)

(define (new-memory #:data data
                    #:nsegs nsegs
                    #:storage-var-ids storage-var-ids
                    #:storage-vars storage-vars
                    #:frozen frozen
                    #:constraint-checks constraint-checks
                    #:prime prime)
  (memory data nsegs storage-var-ids storage-vars frozen constraint-checks prime))

;; Define a maximum memory length
(define MAX_MEM_LEN 100000)

; constructor
(define (make-memory #:storage-var-ids storage-var-ids
                     #:constraint-checks constraint-checks
                     #:prime prime)
  (define data0
    (list->vector (for/list ([_ (range MAX_MEM_LEN)])
                    null)))

  (new-memory #:data data0
              #:nsegs 0
              #:storage-var-ids storage-var-ids
              #:storage-vars (for/vector ([svid storage-var-ids])
                               (thunk* #f))
              #:frozen #f
              #:constraint-checks constraint-checks
              #:prime prime))

(define revert-var #f)

(define (set-revert-var!)
  (set! revert-var #t))

(define (reset-revert-var!)
  (set! revert-var #f))

;; Used for replays
(define (make-concrete-memory old-memory mdl)
  (define old-data (memory-data old-memory))
  (define new-data
    (for/vector ([seg old-data])
      (if (empty? seg)
          seg
          ;; Access function
          (segment (lambda (off)
                     (let ([old-val ((segment-access-function seg) off)]) (evaluate old-val mdl)))
                   ;; keys
                   (remove-duplicates (map (curryr evaluate mdl) (segment-keys seg)))))))

  (define svids (memory-storage-var-ids old-memory))

  (define storage-vars
    (for/vector ([svid svids])
      (thunk* #f)))

  (new-memory #:data new-data
              #:nsegs 0
              #:storage-var-ids svids
              #:storage-vars storage-vars
              #:frozen #t
              #:constraint-checks (memory-constraint-checks old-memory)
              #:prime (memory-prime old-memory)))

(define (apply-write readfunc my-keys val)
  (lambda (keys) (if (equal? my-keys keys) val (readfunc keys))))

(define (write-storage-var p varname k v)
  (let* ([varname (string->symbol varname)]
         [svs (memory-storage-vars p)]
         [svids (memory-storage-var-ids p)]
         [my-sv-index (index-of svids varname)]
         [my-sv (vector-ref svs my-sv-index)])
    (vector-set! svs my-sv-index (apply-write my-sv k v))))

(define (read-storage-var p varname k num-vals)
  (let* ([varname (string->symbol varname)]
         [svs (memory-storage-vars p)]
         [svids (memory-storage-var-ids p)]
         [my-sv-index (index-of svids varname)]
         [my-sv (vector-ref svs my-sv-index)]
         [val (my-sv k)])
    (if val
        val
        (let ([new-val (build-list num-vals (thunk* (symint (memory-prime p))))])
          (write-storage-var p (~a varname) k new-val)
          new-val))))

(define (segment-print seg port mode)
  (let* ([print-func (case mode
                       [(#t) write]
                       [(#f) display]
                       [else (lambda (p port) (print p port mode))])]
         [keys (segment-keys seg)]
         [vals (map (curry segment-ref seg) keys)]
         [kvpairs (map (lambda (k v) (~a k "->" v)) keys vals)])
    (print-func kvpairs port)))

(struct segment (access-function keys)
  #:mutable
  #:transparent
  #:methods gen:custom-write
  [(define write-proc segment-print)])

(define (segment-ref seg i)
  ((segment-access-function seg) i))

(define (segment-set! seg i v)
  (let ([af (segment-access-function seg)] [old-keys (segment-keys seg)])
    ;; NOTE: Duplicates are fine here, and checking slows down symexec
    (set-segment-keys! seg (cons i old-keys))
    (set-segment-access-function! seg (lambda (j) (if (equal? i j) v (af j))))))

; helper function
(define (make-default-segment #:vec [vec? #f])
  (if vec? (make-vector 10000 #f) (segment (lambda (off) #f) empty)))

(define (data-ref p addr)
  (define seg0 (if (integer? addr) 0 (rv-seg addr)))
  (define off0 (if (integer? addr) addr (rv-off addr)))

  (define l0 (vector-ref p seg0))
  (when (null? l0)
    (tokamak:error "l0 is null, given addr: ~a." addr))
  (define l1 ((if (segment? l0) segment-ref vector-ref) l0 off0))
  l1)

(define (data-set! p key val)
  (define seg0 (if (integer? key) 0 (rv-seg key)))
  (define off0 (if (integer? key) key (rv-off key)))
  (define l0
    (let ([t0 (vector-ref p seg0)])
      (cond
        [(null? t0)
         (vector-set! p seg0 (make-default-segment)) ; create a segment
         (vector-ref p seg0)] ; point to that newly created segment
        [else t0])))
  (when (equal? seg0 1)
    (tokamak:log "SETTING AP VAL: ~a ~a" off0 val))
  ((if (segment? l0) segment-set! vector-set!) l0 off0 val))

(define (seg-add! p ind)
  (when (! (null? (vector-ref p ind)))
    (tokamak:error "segment already exists at ind: ~a." ind))
  (vector-set! p ind (make-default-segment #:vec (equal? ind 1))))

(define (verify-same-value addr current value)
  (when (not (equal? current value))
    (tokamak:error "inconsistency memory error, addr: ~a, current: ~a, value: ~a."
                   addr
                   current
                   value)))

(define (memory-ref p addr)
  (relocate-value (data-ref (memory-data p) addr)))

(define (memory-set! p addr value)
  (tokamak:log "memory set addr=~a, value=~a" addr value)
  (map (curryr apply (list addr value)) (memory-constraint-checks p))
  (when (&& (rv? addr) (< (rv-off addr) 0))
    (tokamak:error "offset of rv must be nonnegative, got: ~a." (rv-off addr)))
  (when (or (not (memory-frozen p)) (not (data-ref (memory-data p) addr)))
    (data-set! (memory-data p) addr value))
  (define current (data-ref (memory-data p) addr))
  (verify-same-value addr current value))

(define (relocate-value value)
  (cond
    [(not (rv? value)) value]
    [(>= (rv-seg value) 0) value]
    [else value]))

(define (add-segment p #:size [size null])
  (let ([segment-index (memory-nsegs p)] [data (memory-data p)])
    (set-memory-nsegs! p (+ 1 segment-index))
    (when (not (memory-frozen p))
      (seg-add! data segment-index))
    (rv segment-index 0)))

(define (load-data p ptr data)
  (let ([n (length data)])
    (for ([i (range n)])
      (memory-set! p (rvadd ptr i) (list-ref data i))))
  (rvadd ptr (length data)))
