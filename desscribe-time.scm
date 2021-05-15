(define (seconds? sec)
  (if(and(<= 1 sec)(> 60 sec))
     #t
     #f))
(define (minutes? sec)
  (if (and(<= 60 sec)(> 3600 sec))
      #t
      #f))
(define (hours? sec)
  (if (and(<= 3600 sec)(> 86400 sec))
      #t
      #f))
(define (days? sec)
   (if (and(<= 86400 sec)(> 604800 sec))
      #t
      #f))
(define (weeks? sec)
   (if (and(<= 604800 sec)(> 2419200 sec))
      #t
      #f))
(define (months? sec)
   (if (and(<= 2419200 sec)(> 29030400 sec))
      #t
      #f))
(define (years? sec)
   (if (and(<= 29030400 sec)(> 2903040000 sec)) 
      #t
      #f))
(define (centuries? sec)
   (if (and(<= 2903040000 sec)(> 29030400000000 sec))
      #t
      #f))

(define (sec-min-max-set? sec min max)
  (if(and(and(<= min sec)(>=(* 2 sec)))(<= sec max))
     #t
     #f))

(define (plural sec wd min max)
  (if(sec-min-max-set? sec min max)
     (word wd)
     (if(ends-cons-and-y? wd)
        (word (bl wd) 'ies)
        (word wd 's))))


(define (ends-cons-and-y? wd)
  (if(and(consonant?(word(last(bl wd))))(last-y?(word(last wd))))
     #t
     #f))

(define (last-y? wd)
  (member? wd 'y))
(define (consonant? wd)
  (member? wd 'bcdfgjklmnpqstvxzhry))


(define (describe-time sec)
  (cond
    ((seconds? sec)
     (se sec (plural sec 'second 1 1)))
    ((minutes? sec)
     (se(exact->inexact(/ sec 60))
        (plural sec 'minute 60 119)))
    ((hours? sec)
     (se(exact->inexact(/ sec 3600))
        (plural sec 'hour 60 7199)))
    ((days? sec)
     (se(exact->inexact(/ sec 86400))
        (plural sec 'day 3600 172799)))
    ((weeks? sec)
     (se(exact->inexact(/(/ sec 86400)7))
        (plural sec 'week 3600 1209599)))
    ((months? sec)
     (se(exact->inexact(/(/ sec 604800)4))
        (plural sec 'month 3600 4838399)))
    ((years? sec)
     (se(exact->inexact(/(/ sec 2419200)12))
        (plural sec 'year 3600 58060799)))
    ((centuries? sec)
     (se(exact->inexact(/ sec 3.154e+9))   
        (plural sec 'century 3600 5806079999)))
    (else
     '(you got to the end of the universe))))


(describe-time 2)
