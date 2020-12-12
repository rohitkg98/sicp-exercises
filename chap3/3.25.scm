#lang scheme
(require compatibility/mlist)
(require rnrs/mutable-pairs-6)

(define (assoc key records)
  (cond ((not (mpair? records)) false)
        ((null? records) false)
        ((equal? key (mcar (mcar records))) (mcar records))
        (else (assoc key (mcdr records)))))

(define (make-table)
  (let ((local-table (mlist '*table*)))
    (define (lookup keys)
      (define (helper keys records)
        ; keys could be null, handle that case
        (if (null? keys) #f
            (let ((value (assoc (mcar keys) records)))
              ; if value is false, that means it doesn't exists
              (cond ((not value) #f)
                    ; if no more keys left, return the value
                    ((null? (mcdr keys)) value)
                    ; if value is not a subtable, return false
                    ((not (mpair? value)) #f)
                    ; else search the next key, in the subtable
                    (else (helper (mcdr keys) (mcdr value)))))))
      (helper keys (mcdr local-table)))

    (define (insert! keys to-be-inserted)
      (define (helper keys table)
        (let ((table-name (mcar table))
              (records (mcdr table)))
          ; overwrite existing key path, or add a new one
          (if (null? keys)
              (begin (set-cdr! table to-be-inserted) 'ok)
              (let ((value (assoc (mcar keys) records)))
                ; if value is false, add a new value to records
                ; and keep going till keys are null
                ; if value exists, and is not a table, then too, do the same
                (cond ((or (not value)
                           (not (mpair? value)))
                       (set! value (mcons (mcar keys) to-be-inserted))
                       (set-cdr! table
                                 (mcons value
                                        records))
                       (helper (mcdr keys) value))
                      ; now value is a subtable, just move in
                      (else 
                       (helper (mcdr keys) value)))))))
      (helper keys local-table))
                     
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation -- TABLE" m))))
    dispatch))

(define operation-table (make-table))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))

