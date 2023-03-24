#lang r7rs

(import (scheme base)
        (scheme write)
        (a-d graph unweighted config)
        (a-d graph-traversing dft-unweighted)
        (a-d graph examples undirected-unweighted)
        (prefix (a-d stack linked) stack:))

;;;; Vraag 1
;; Pas deze functie aan volgens de opgave

(define (edge-connected-components g)
  (define preorder-time 0)
  (define preorder-numbers (make-vector (order g) -1))
  (define parents (make-vector (order g) -1)) ; nodig om in edge-bumped de tree-edges te filteren
  (define highest-back-edges (make-vector (order g) -1))
  (define bridges '())

  (define nr-of-comps 0)
  (define curr-comp   0)
  (define comp-vector (make-vector (order g) -1))
  (define stack (stack:new))

  ;; Hulpmethode om curr-comps van component te corrigeren
  (define (split-comp! endnode)
  (let ((top (stack:top stack)))
    (vector-set! comp-vector top nr-of-comps)
    (if (not (eq? (stack:pop! stack) endnode))
        (split-comp! endnode))))
  
  (dft g
       ;; root-discovered
       (lambda (root)
         (set! nr-of-comps (+ nr-of-comps 1))
         (set! curr-comp   nr-of-comps))
       ;; node-discovered
       ;; registreer het pre-order nummer van de knoop
       ;; en zet de hoogste terugboog op jezelf
       (lambda (node)
         (vector-set! preorder-numbers node preorder-time)
         (vector-set! highest-back-edges node preorder-time)
         (set! preorder-time (+ preorder-time 1))
         (vector-set! comp-vector node curr-comp)
         (stack:push! stack node))
       ;; node-processed
       node-nop
       ;; edge-discovered
       ;; registreer dat from de parent is van to (nodig voor edge-bumped)
       (lambda (from to)
         (vector-set! parents to from))
       ;; edge-processed
       ;; knoop 'to' is volledig afgewerkt. Update de hoogste
       ;; terugboog van 'from' en kijk of 'from-to' een brug is
       (lambda (from to)
         (vector-set! highest-back-edges from
                      (min (vector-ref highest-back-edges to)
                           (vector-ref highest-back-edges from)))
         (when (= (vector-ref highest-back-edges to)
                  (vector-ref preorder-numbers to))
           (set! bridges (cons (cons from to) bridges))
           (set! nr-of-comps (+ nr-of-comps 1))
           (split-comp! to)))
       ;; edge-bumped
       ;; boog ontdekt naar een reeds bezochte knoop!
       ;; als het niet je ouder is, dan kan het een terugboog zijn
       ;; kijk of je een terugboog naar een hogere knoop
       ;; gevonden hebt
       (lambda (from to)
         (if (not (eq? (vector-ref parents from) to))
             (vector-set! highest-back-edges from
                          (min (vector-ref preorder-numbers to)
                               (vector-ref highest-back-edges from))))))
  (list bridges nr-of-comps comp-vector))

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
(display "Bridges Tests:")(newline)

; {{{4 . 5} {5 . 6} {0 . 1}} 6 #(1 2 2 2 3 5 4 6 6 6 6)}
(display "three-cc: ")(display (edge-connected-components three-cc))(newline)

; {{{7 . 1}} 2 #(1 2 1 1 1 1 1 1)}
(display "connected: ")(display (edge-connected-components connected))(newline)

; {() 1 #(1 1 1 1 1 1 1 1 1)}
(display "kite: ")(display (edge-connected-components kite))(newline)

; {{{0 . 3}} 2 #(1 1 1 2 2 2 1 1)}
(display "zwaluw ")(display (edge-connected-components zwaluw))(newline)
