#lang r7rs

(import (scheme base)
        (scheme write)
        (a-d graph unweighted config)
        (a-d graph-traversing dft-unweighted)
        (a-d graph examples undirected-unweighted)
        (prefix (a-d stack linked) stack:))

;;;; Vraag 2
;; Pas deze functie aan volgens de opgave

(define (biconnected-components g)
  (define preorder-time 0)
  (define preorder-numbers (make-vector (order g) 0))
  (define parents (make-vector (order g) -1)) ; nodig om in edge-bumped tree-edges te filteren
  (define highest-back-edges (make-vector (order g) -1))
  (define articulation-points (make-vector (order g) #f))
  (define current-root '()) ; we onthouden de root
  (define branch-count 0) ; we tellen hoeveel tree-edges de root heeft

  
  (define nr-of-comps 0)
  (dft g
       ;; root-discovered
       ;; registreer de huidige root en zet zijn
       ;; branch count op 0
       (lambda (root)
         (set! current-root root)
         (set! branch-count 0))
       ;; node-discovered
       ;; registreer het pre-order nummer van de knoop
       ;; en zet de hoogste terugboog op jezelf
       (lambda (node)
         (vector-set! preorder-numbers node preorder-time)
         (vector-set! highest-back-edges node preorder-time)
         (set! preorder-time (+ preorder-time 1)))
       ;; node-processed
       ;; kijk of de afgewerkte knoop de wortel is
       ;; zoja, kijk dan of die 2 of meer branches heeft
       ;; indien dit het geval is, dan is de wortel
       ;; een scharnierpunt
       (lambda (node)
         (if (= node current-root)
             (vector-set! articulation-points node
                          (>= branch-count 2))))
       ;; edge-discovered
       ;; registreer dat 'from' de parent is van 'to'
       ;; en als 'from' de root is, verhoog zijn
       ;; branch count
       (lambda (from to)
         (vector-set! parents to from)
         (if (= from current-root)
             (set! branch-count (+ branch-count 1))))
       ;; edge-processed
       ;; de knoop 'to' is volledig afgewerkt. Update de
       ;; hoogste terugboog van 'from' en kijk of 'from'
       ;; een articulatiepunt is
       (lambda (from to)
         (vector-set! highest-back-edges from
                      (min (vector-ref highest-back-edges to)
                           (vector-ref highest-back-edges from)))
         ;; Ofwel is 'from' een "echt" scharnierpunt, maar
         ;; deze check is ook altijd waar als 'from' de wortel is!
         ;; Geen enkele knoop kan aan een kleiner pre-order nummer
         ;; dan de wortel geraken. Dus we zetten de wortel als
         ;; scharnierpunt, maar dit kan nog overschreven worden
         ;; in node-processed.
         (when (>= (vector-ref highest-back-edges to)
                   (vector-ref preorder-numbers from))
           (vector-set! articulation-points from #t)))
           (set! nr-of-comps (+ nr-of-comps 1))
       ;; edge-bumped
       ;; een boog naar een reeds bezochte knoop! als dit niet
       ;; je parent is, dan is het een terugboog. Kijk of je
       ;; een hogere terugboog gevonden hebt.
       (lambda (from to)
         (if (not (eq? (vector-ref parents from) to))
             (vector-set! highest-back-edges from
                          (min (vector-ref preorder-numbers to)
                               (vector-ref highest-back-edges from))))))
  articulation-points)

;; Extra graaf
(define zwaluw
  (let ((g (new #f 8)))
    (add-edge! g 0 1)
    (add-edge! g 1 2)
    (add-edge! g 2 0)
    (add-edge! g 0 3)
    (add-edge! g 3 4)
    (add-edge! g 4 5)
    (add-edge! g 5 3)
    (add-edge! g 0 6)
    (add-edge! g 6 7)
    (add-edge! g 7 0)
    g))

;; "Tests"
(display "Bigeconnecteerdheid:")(newline)

; {#(#f #t #f #f #f #t #f #f #f #f #f)
;  5 #({2} {2 1} {1} {1} {4} {4 3} {3} {5} {5} {5} {5})}
(display "three-cc: ")(display (biconnected-components three-cc))(newline)

; {#(#f #f #f #f #f #f #f #t) 2 #({2} {1} {2} {2} {2} {2} {2} {2 1})}
(display "connected: ")(display (biconnected-components connected))(newline)

; {#(#f #t #f #f #f #f #f #f #f) 2 #({2} {2 1} {2} {1} {1} {1} {1} {1} {1})}
(display "kite: ")(display (biconnected-components kite))(newline)

; {#(#t #f #f #t #f #f #f #f) 4 #({4 3 1} {4} {4} {3 2} {2} {2} {1} {1})}
(display "zwaluw: ")(display (biconnected-components zwaluw))(newline)
