#lang racket

(require "resources/helperFunctions.rkt")

; Load from files -------------------------------------------------------
; Load the questions
(define questions-path "resources/questions.txt")

(define questions (list->vector (read-file questions-path)))

; Load the animals

(define animals-path "resources/animals.txt")

; Returns a list of pairs where the first element of a given pair
;   is the animal name and the second is a set of the indexes
;   of the questions that have a "yes" answer for the animal
(define (load-animals path)
  (define animals-buffer (read-file path))
  (define (format-animals buffer animals)
    (cond [(null? buffer) animals]
          [else (define animal (car buffer))
                (define animal-buffer (regexp-split ":" animal))
                (define animal-name (car animal-buffer))
                (define animal-properties (cadr animal-buffer))
                (define formated-animals (list->set (map string->number
                                                   (regexp-split "," animal-properties))))
                (format-animals (cdr buffer)
                                (cons (cons animal-name formated-animals)
                                      animals))]))
  (format-animals animals-buffer '()))

(define animals (make-hash (load-animals animals-path)))

; Serializing data -----------------------------------------------------------
(define (save-questions questions) (serialize-vector questions questions-path))

(define (save-animals animals)
  (define file (open-output-file animals-path #:exists 'truncate))
  (for ([(i j) animals]) 
    (fprintf file
             "~a:~a\r\n"
             i
             (set->string j)))
  (close-output-port file))

; Helper fields --------------------------------------------------------------
(define questions-remaining (stream->list
                       (in-range 0 (vector-length questions))))

(define (next-question questions-remaining) (car questions-remaining))

(define (next-questions questions-remaining) (cdr questions-remaining))

(define likely-animals (hash-keys animals))

; Helper functions --------------------------------------------------------

(define (get-animal-properties animal)
  (hash-ref animals animal))

; property: index of a question in <questions>
(define (animal-has-property animal property)
  (set-member? (get-animal-properties animal) property))

(define (animal-doesnt-have-property animal property)
  (not (set-member? (get-animal-properties animal) property)))

; likely-animals: list of animal names
; prop-index: index of the question in <questions>
;   that the animals are going to be filtered by
; possession: bool, if the animals should have that property or not
(define (filter-animals likely-animals prop-index possession)
  (define has-property
    (if possession
        animal-has-property
        animal-doesnt-have-property))
  (define predicate (lambda (animal)
                         (has-property animal prop-index)))
  (filter predicate likely-animals))

; possess: lambda that takes animal and property and returns bool
(define (all-possess-property animals property possess)
  (not (search #f (map (lambda (animal)
                       (possess animal property))
                     animals))))
;(all-possess-property '("Котка" "Куче" "Папагал") 0 animal-has-property)

(define (all-have-property animals property)
  (all-possess-property animals property animal-has-property))
;(all-have-property '("Котка" "Куче" "Папагал") 0)

(define (all-dont-have-property animals property)
  (all-possess-property animals property animal-doesnt-have-property))

(define (list-member v lst)
  (not (equal? (member v lst) #f)))

(define (get-new-animal)
  (define new-animal (read-line))
  (cond [(not (list-member new-animal (hash-keys animals)))
         new-animal]
        [else (printf "I already know of that animal.\nPlease enter a new one!\n")
              (get-new-animal)]))

(define (get-new-question)
  (define new-question (read-line))
    (cond [(not (not (equal? (vector-member new-question questions) #f)))
         new-question]
        [else (printf "I already know that question.\nPlease enter a new one!\n")
              (get-new-question)]))

(define (choose-an-animal animal1 animal2)
  (define chosen-animal (read-line))
  (cond [(string=? chosen-animal animal1) animal1]
        [(string=? chosen-animal animal2) animal2]
        [else (printf "That is not one of the two animals!\nPlease answer ~a or ~a!\n"
                      animal1 animal2)
              (choose-an-animal animal1 animal2)]))

(define (ask-for-unknown-animal likely-animal answers)
  (printf "What is it then?\n")
  (define new-animal (get-new-animal))
  (printf "How can I distinguish between your animal and mine?\n")
  (define new-question (get-new-question))
  (printf "Which one of our animals will answer <yes> to that question?\n")
  (define chosen-animal (choose-an-animal new-animal likely-animal))
  (printf "Thank you!\nI am writing it down!\n")
  (save-questions (vector-push-back questions new-question))
  (define new-question-index (vector-length questions))
  (cond [(string=? chosen-animal new-animal)
         (save-animals (hash-insert animals
                                    new-animal
                                    (set-add answers new-question-index)))]
        [else (hash-update! animals
                            chosen-animal
                            (lambda (x)
                              (set-add x new-question-index)))
              (save-animals (hash-insert
                             animals
                             new-animal
                             answers))]))

(define (ask-remaining-questions questions-remaining answers likely-animals)
  (define current-question (next-question questions-remaining))
  (define all-yes (all-have-property
                   likely-animals current-question))
  (define all-no (all-dont-have-property
                  likely-animals current-question))
  (define can-skip-question (or all-yes
                                all-no))
  (cond [can-skip-question (filter-by-questions
                            (next-questions questions-remaining)
                            likely-animals
                            answers)]
        [else 
         (printf "~a\n" (vector-ref questions current-question))
         (define answer (read-bool))
         (filter-by-questions
          (next-questions questions-remaining)
          (filter-animals likely-animals current-question answer)
          (if answer (set-add answers current-question)
              answers))]))

; Answers in <filter-by-questions> is a list of the indexes
;   of the questions that have been answered <yes> to
(define (filter-by-questions questions-remaining likely-animals answers)
  (cond [(= (length likely-animals) 1)
         (define likely-animal (car likely-animals))
         (printf "Is it a ~a?\n" likely-animal)
         (cond [(read-bool) "Success!!! :)"]
               [else (ask-for-unknown-animal likely-animal answers)])]
        [else (ask-remaining-questions questions-remaining
                                  answers likely-animals)]))

; Game of animals ------------------------------------------------------------
(define (game-of-animals)
  (filter-by-questions questions-remaining likely-animals (set)))

(game-of-animals)



