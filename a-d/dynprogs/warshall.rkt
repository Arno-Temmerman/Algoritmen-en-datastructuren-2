#lang r7rs

(import (scheme base)
        (scheme write)
        (a-d graph unweighted config)
        (a-d graph examples directed-unweighted)
        (a-d scheme-tools)
        (a-d graph-algorithms directed basic)
        (prefix (only (a-d graph-algorithms directed traclo-unweighted) traclo-warshall) g:))

; In deze file "distilleer" ik het Warshall algoritme (= dynamic programming) uit een makkelijk maar naief-recursieve algoritme

(define g0 sedgewick172)
(define g1 (copy g0))
(define g2 (copy g1))
(define g3 (copy g2))

; Poging 1 : Naief algoritme:
;   - we vallen de graf recursief aan door telkens minder knopen op het pad tot te laten als tussenliggende knopen. Van n-1 tot 0.
;   - een pad dat knopen 0 tot via mag gebruiken gaat ofwel langs via (t.t.z. van from naar via en via naar to) ofwel niet.

(define (traclo-recursive g)
  (define n (order g))
  (define res (make-2D-vector n n (lambda (i j) '())))
  (define (rec from to via)
    (if (= -1 via)
        (adjacent? g from to)
        (or (rec from to  (- via 1))
            (and (rec from via (- via 1))
                 (rec via  to  (- via 1))))))
  (for-each-node
   g
   (lambda (from)
     (for-each-node
      g
      (lambda (to)
        (ij! res from to (rec from to (- n 1)))))))
  res)

; Poging 2 : Memoizen van bovenstaand algoritme. We zien in dat vele recursieve aanroepen van rec heel dikwijls hetzelfde gaan doen.

(define (traclo-memoize g)
  (define n (order g))
  (define res (make-2D-vector n n (lambda (i j) '()))) 
  (define (rec from to via)
    (if (or (null? (ij? res from to))
            (not (ij? res from to)))
        (let ((tst (if (= -1 via)
                       (adjacent? g from to)
                       (or (rec from to (- via 1))
                           (and (rec from via (- via 1))
                                (rec via  to  (- via 1)))))))
          (ij! res from to tst)
          tst)
        (ij? res from to)))
  (for-each-node
   g
   (lambda (from)
     (for-each-node
      g
      (lambda (to)
        (rec from to (- n 1))))))
  res)

; Poging 3 : De gememoizede versie bestaat uit het constant vermijden van de boomrecursie. Wat reeds gedaan is doen we geen 2 keer.
; Dus kunnen we de recursie "binnenste buiten draaien" en een bottom-up iteratie doen i.p.v. een top-down recursie

(define (traclo-warshall g)
   (define res (make-2D-vector (order g) (order g) (lambda (i j) (adjacent? g i j))))
  (for-each-node
   g
   (lambda (via)
     (for-each-node
       g
       (lambda (from)
         (for-each-node
          g
          (lambda (to)
            (if (or (ij? res from to)
                    (and (ij? res from via)
                         (ij? res via to)))
                (ij! res from to #t))))))))
  res)

; Poging 4 = In bovenstaand algoritme leveren we een 2D matrix op; in Het algoritme van de cursus leveren we een graaf op.


(define m0 (traclo-recursive g0))
(display m0)(newline)
(display "----------------------")(newline)
(define m1 (traclo-memoize   g1))
(display m1)(newline)
(display "----------------------")(newline)
(define m2 (traclo-warshall  g2))
(display m2)(newline)
(display "----------------------")(newline)
(define m3 (g:traclo-warshall  g3))
(display m3)(newline)
