#lang r7rs

(import (scheme base)
        (scheme write)
        (prefix (a-d stack linked) stack:)
        (a-d graph-algorithms directed basic)
        (a-d graph-algorithms directed connectivity)
        (a-d graph unweighted config)
        (a-d graph-traversing dft-unweighted)
        (a-d graph examples directed-unweighted))

; Net zoals bij de algoritmes van boogsamenhangendheid en bigeconnecteerdheid in ONgerichte grafen,
; worden er om sterke samenhangendheid te gaan bepalen, preorder-nummers uitgedeeld aan iedere knoop en
; wordt er bekeken wat het kleinste preorder-nummer is dat we vanuit een knoop kunnen bereiken
; (cref. principe van Hopcroft-Tarjan)

(define (scc-tarjan-vectorstack g)
  (define preorder-time 0)
  (define preorder-numbers (make-vector (order g) -1))
  (define highest-back-edge (make-vector (order g) -1))
  (define sc-components (make-vector (order g) -1))
  (define nr-of-components 0)
  (define sv (make-vector (order g) '())) ; stack om huidig afgelegd pad bij te houden
  (define top '())
  (dft
   g
   root-nop
   (lambda (node)                                       ; node-discovered
     (vector-set! sv node top)
     (set! top node)
     (vector-set! preorder-numbers node preorder-time)  ; deel preorder-nummers uit
     (vector-set! highest-back-edge node preorder-time) ; tijdens het afdalen is de node (voorlopig) zelf het kleinste preorder-nummer dat we kunnen bereiken
     (set! preorder-time (+ preorder-time 1)))
   (lambda (node)                                    ; node-processed
     (when (= (vector-ref highest-back-edge node)    ; we kunnen vanuit deze knoop enkel maar terug tot deze knoop geraken
              (vector-ref preorder-numbers node))    ; als kleinste preorder-nummer ==> We hebben een "afgewerkt" component gevonden
       (set! nr-of-components (+ 1 nr-of-components))
       (let loop
         ((t top))                       ; pop het pad dat werd afgelegd SINDS het discover'en van de node  
         (set! top (vector-ref sv t))
         (vector-set! sc-components t nr-of-components) ; iedere knoop op dat pad behoort tot het sterk samenhangend component
         (vector-set! sv t 'included)                    ; registreer dat de knoop reeds tot een component behoort om kruisbogen te filteren
         (unless (eq? t node)                           ; we blijven dit doen tot het hele pad tot en met de node van de stack is gehaald
           (loop top)))))
   edge-nop          ; edge-discovered
   (lambda (from to) ; edge-processed
     (if (not (eq? 'included (vector-ref sv to)))
         (vector-set! highest-back-edge
                      from (min (vector-ref highest-back-edge from)
                                (vector-ref highest-back-edge to)))))
   (lambda (from to)                    ; edge-bumped 
     (if (not (eq? 'included (vector-ref sv to))) ; avoid cross-edges!!
         (vector-set! highest-back-edge
                      from (min (vector-ref highest-back-edge from)
                                (vector-ref preorder-numbers to))))))
  (cons nr-of-components sc-components))

;;TESTS
(display (list "SCC-tarj    sedgewick172" (scc-tarjan             sedgewick172)))
(newline)  ;{3 . #(2 2 2 3 1 1)}
(display (list "SCC-tarj-vs sedgewick172" (scc-tarjan-vectorstack sedgewick172)))
(newline)  ;{3 . #(2 2 2 3 1 1)}
(display (list "SCC-tarj    full-cycle"   (scc-tarjan             full-cycle)))
(newline)  ;;{1 . #(1 1 1 1 1 1)}
(display (list "SCC-tarj-vs full-cycle"   (scc-tarjan-vectorstack full-cycle)))
(newline)  ;{1 . #(1 1 1 1 1 1)}
(display (list "SCC-tarj    a-list"       (scc-tarjan             a-list)))
(newline)  ;{6 . #(6 5 4 1 2 3)}
(display (list "SCC-tarj-vs a-list"       (scc-tarjan-vectorstack a-list)))
(newline)  ;{6 . #(6 5 4 1 2 3)}
(display (list "SCC-tarj    scc4"         (scc-tarjan             scc4)))
(newline)  ;{4 . #(3 1 3 3 3 3 3 4 4 2 2 2 2)}
(display (list "SCC-tarj-vs scc4"         (scc-tarjan-vectorstack scc4)))
(newline)  ;{4 . #(3 1 3 3 3 3 3 4 4 2 2 2 2)}