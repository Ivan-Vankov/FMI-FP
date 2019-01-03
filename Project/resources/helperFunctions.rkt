#lang racket

(provide vector-push-back hash-insert read-file display-line read-bool search
         serialize-vector set->string)

; search -------------------------------------------------------

(define (search val l)
  (and (not (null? l))
       (or (equal? val (car l)) (search val (cdr l)))))


; push-back

(define (vector-push-back vec val)
  (list->vector (reverse
                   (cons val
                         (reverse (vector->list vec))))))

; hash-insert

(define (hash-insert hash key value)
  (make-hash (cons (cons key value) (hash->list hash))))

; Set to string ----------------------------------------
(define (set->string st)
  (define (helper curr-set str)
    (cond [(set-empty? curr-set) str]
          [(= (set-count curr-set) 1) (string-append str
                                                     (number->string
                                                      (set-first curr-set)))]
          [else (helper (set-rest curr-set)
                        (string-append
                         (string-append str 
                                        (number->string (set-first curr-set)))
                         ","))]))
  (helper st ""))

; Standard IO ------------------------------------------

(define (read-bool)
  (define answer (read-line))
  (cond [(string=? "yes" answer) #t]
        [(string=? "no" answer) #f]
        [else (display "Please enter \"yes\" or \"no\"\n")
              (read-bool)]))


; File IO ----------------------------------------------

; My readFile
(define (read-file path)
  (define opened-file (open-input-file path))
  ;(define (read-BOM file) (read-char file))
  (define (read-lines file)
    (define line (read-line file 'any))
    (if (eof-object? line)
        (begin
          (close-input-port file)
          '())
        (cons line (read-lines file))))
  ;(read-BOM opened-file)
  (read-lines opened-file))


; Writing to a file -----------

; displayln doesn't work
(define (display-line line out) 
  (display line out)
  (display "\r\n" out))

(define (serialize-vector vec path)
  (define file (open-output-file path #:exists 'truncate))
  (for ([i (in-range 0 (vector-length vec))])
    (display-line (vector-ref vec i) file))
  (close-output-port file))
