#lang scheme
(define (test-and-set! cell)
  (if (car cell)
      true
      (begin (set-car! cell true)
             false)))

(define (clear! cell)
  (set-car! cell false))

(define (make-mutex)
  (let ((cell (list false)))
    (define (the-mutex m)
      (cond ((eq? m 'acquire)
             (if (test-and-set! cell)
                 (the-mutex 'acquire))) ; retry
            ((eq? m 'release) (clear! cell))))
    the-mutex))

(define (make-serializer)
  (let ((mutex (make-mutex)))
    (lambda (p)
      (define (serialized-p . args)
        (mutex 'acquire)
        (let ((val (apply p args)))
          (mutex 'release)
          val))
      serialized-p)))

; a using mutex
; we give it n mutexes at max
; the acquiring and releasing of mutexes need to be serialized
; this will go into a deadlock upon hitting max mutexes
; the release and acquire cannot be on the same serializer
; bad solution
(define (make-semaphore n)
  (let ((total 0)
        (serializer (make-serializer)))
    (define (acquire)
      (if (< total n)
        (set! total (+ total 1))
        (acquire)))
    (define (release)
      (set! total (- total 1)))
    (define (the-semaphore m)
      (cond ((eq? m 'acquire) (serializer acquire))
            ((eq? m 'release) (serializer release))))))

; a correct solution would acquire lock and release before retrying
; so that release also gets a chance
(define (make-semaphore n)
  (let ((total 0)
        (access-lock (make-mutex)))
    (define (acquire)
      (access-lock 'acquire)
      (if (< total n)
        (begin (set! total (+ total 1))
               (access-lock 'release))
        (begin (access-lock 'release)
                (acquire))))
    (define (release)
      (access-lock 'acquire)
      (set! total (- total 1))
      (access-lock 'release))
    (define (the-semaphore m)
      (cond ((eq? m 'acquire) (acquire))
            ((eq? m 'release) (release))))))

; now, an even better way would be create separate locks for access and max
; this way if a process got an access lock but max value is reached, they release access lock and go into max lock
; as soon as max opens up when release gets called they continue execution by calling acquire again
(define (make-semaphore n)
  (let ((total 0)
        (access-lock (make-mutex))
        (release-indicator (make-mutex)))
    (define (acquire)
      (access-lock 'acquire)
      (if (< total n)
        (begin (set! total (+ total 1))
               (access-lock 'release))
        ; all semaphores occupied
        ; don't proceed further until one is released
        (begin (access-lock 'release)
               (release-indicator 'acquire)
               (acquire))))
    (define (release)
      (access-lock 'acquire)
      (set! total (- total 1))
      ; indicate that there is atleast one free semaphore now
      (release-indicator 'release)
      (access-lock 'release))
    (define (the-semaphore m)
      (cond ((eq? m 'acquire) (acquire))
            ((eq? m 'release) (release))))))

; b in terms of atomic test-and-set!
(define (make-semaphore n)
  (let ((total 0)
        (sem-lock (list false)))

    ; acquire lock, and then try to acquire value
    (define (acquire)
      (if (test-and-set! sem-lock)
          ; if someone else if getting a place in semphore
          ; wait for them to finish by keep retrying
          (acquire)
          ; try to get a place if possible
          (if (< total n)
              ; if there is place, occupy it
              (begin (set! total (+ total 1))
                     (clear! sem-lock))
              ; if none, then release and try again
              (begin (clear! sem-lock) (acquire)))))

    (define (release)
      (if (test-and-set! sem-lock)
          (release)
          (begin (set! total (- total 1))
                 (clear! sem-lock))))
    (define (the-semaphore m)
      (cond ((eq? m 'acquire) (acquire))
            ((eq? m 'release) (release))))))
        