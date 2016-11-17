(define (make-table1)
  (list 'table))


(define (lookup key table)
  (let ((record (assoc key (cdr table))))
  (if record
      (caar record)
      'false))

(define (assoc key records)
  (null? records
         'false
          (equal? key (caar (car records)))
                (car records)
                (assoc key (cdr records))))
      
(define (insert! key value table)
  (let ((record (assoc key (cdr table))))
       (if record
           (set-cdr record value)
           (set-cdr table
                    (cons (cons key value)
                          (cdr table))))))
