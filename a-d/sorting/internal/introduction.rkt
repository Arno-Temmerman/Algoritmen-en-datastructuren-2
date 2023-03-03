;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*                                                                 *-*-
;-*-*              Different Styles of Moving&Comparing               *-*-
;-*-*                                                                 *-*-
;-*-*                       Wolfgang De Meuter                        *-*-
;-*-*                 2018 Programming Technology Lab                 *-*-
;-*-*                   Vrije Universiteit Brussel                    *-*-
;-*-*                                                                 *-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-

#lang r7rs

(import (scheme base)
        (a-d sorting internal comparative quicksort))
(define playlist-1
  (vector (vector 1 "One More Time- Aerodynamic" "8:03" "Daft Punk")
          (vector 2 "Face to Face- Harder Better Faster Stronger" "4:55" "Daft Punk")	
          (vector 3 "Too Long" "5:10"	"Daft Punk ")
          (vector 4 "Around the World- Harder Better Faster Stronger" "7:27" "Daft Punk")	
          (vector 5 "Steam Machine" "1:39" "Daft Punk")
          (vector 6 "Crescendolls-Too Long- High Life" "7:41" "Daft Punk")	
          (vector 7 "Television Rules the Nation" "2:47" "Daft Punk")	
          (vector 8 "Technologic" "5:29" "Daft Punk ")
          (vector 9 "Superheroes- Human After All" "6:13" "Daft Punk")
          (vector 10 "Da Funk" "5:59" "Daft Punk")
          (vector 11 "The Brainwasher- The Primetime of Your Life- Steam Machine" "12:37" "Daft Punk")
          (vector 12 "Robot Rock-Oh Yeah" "6:36" "Daft Punk")))
(define playlist-2
  (vector (vector 1 2 3 4 5 6 7 8 9 10 11 12)
          (vector "One More Time- Aerodynamic" "Face to Face- Harder Better Faster Stronger"
                  "Too Long" "Around the World- Harder Better Faster Stronger"
                  "Steam Machine" "Crescendolls-Too Long- High Life" "Television Rules the Nation"	
                  "Technologic" "Superheroes- Human After All" "Da Funk"
                  "The Brainwasher- The Primetime of Your Life- Steam Machine" "Robot Rock-Oh Yeah")
          (vector "8:03" "4:55" "5:10" "7:27" "1:39" "7:41" "2:47" "5:29" "6:13" "5:59" "12:37" "6:36")
          (vector "Daft Punk" "Daft Punk" "Daft Punk" "Daft Punk" "Daft Punk" "Daft Punk"
                  "Daft Punk" "Daft Punk" "Daft Punk" "Daft Punk" "Daft Punk" "Daft Punk")))
  
(define persons (vector '("Paul" 41) '("Anna-Mae" 44) '("Selma" 23) 
                        '("Kenny" 68) '("Roger" 41) '("Paul" 22)))

(sort persons (lambda (p1 p2) (string<? (car p1) (car p2))))
(sort persons (lambda (p1 p2) (< (cadr p1) (cadr p2))))

(define compare-1
  (lambda (i j)
    (string<? (vector-ref (vector-ref playlist-1 i) 1)
              (vector-ref (vector-ref playlist-1 j) 1))))
(define compare-2
  (lambda (i j)
    (string<? (vector-ref (vector-ref playlist-2 1) i)
              (vector-ref (vector-ref playlist-2 1) j))))

(define (swap-1 i j)
  (let ((song (vector-ref playlist-1 i)))
    (vector-set! playlist-1 i (vector-ref playlist-1 j))
    (vector-set! playlist-1 j song)))

(define (swap-2 i j)
  (let ((nr (vector-ref (vector-ref playlist-2 0) i))
        (title (vector-ref (vector-ref playlist-2 1) i))
        (time (vector-ref (vector-ref playlist-2 2) i))
        (artist (vector-ref (vector-ref playlist-2 3) i)))
    (vector-set! (vector-ref playlist-2 0) i (vector-ref (vector-ref playlist-2 0) j))
    (vector-set! (vector-ref playlist-2 1) i (vector-ref (vector-ref playlist-2 1) j))
    (vector-set! (vector-ref playlist-2 2) i (vector-ref (vector-ref playlist-2 2) j))
    (vector-set! (vector-ref playlist-2 3) i (vector-ref (vector-ref playlist-2 3) j))
    (vector-set! (vector-ref playlist-2 0) j nr)
    (vector-set! (vector-ref playlist-2 1) j title)
    (vector-set! (vector-ref playlist-2 2) j time)
    (vector-set! (vector-ref playlist-2 3) j artist)))
  