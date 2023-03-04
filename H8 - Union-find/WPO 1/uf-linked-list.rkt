#lang r7rs

; ADT voor een enkelgelinkte lijst zoals beschreven in de opgave dat één verzameling voorstelt.
(define-library (uf-linked-list)
  (export new from-scheme-list add! length append! list-leader head list-node-val)
  (import (except (scheme base) length map))
  (begin

    ; De gelinkte lijst houdt bij wat zijn grootte is,
    ; wie het eerste en laatste element is en welke
    ; functie gebruikt kan worden om de elementen van
    ; de lijst met elkaar te vergelijken.
    (define-record-type uf-linked-list
      (make s h r e)
      uf-linked-list?
      (s size size!)
      (h head head!)
      (r rear rear!)
      (e equality equality!))

    ; Een list-node heeft een waarde, een pointer naar
    ; de volgende node en een pointer naar de leider
    ; van de lijst
    (define-record-type uf-list-node
      (make-list-node v n l)
      list-node?
      (v list-node-val list-node-val!)
      (n list-node-next list-node-next!)
      (l list-leader list-leader!))

    ; new
    ; ( ( V V ➙ boolean ) ➙ uf-linked-list<V> )
    ; Maak een nieuwe gelinkte lijst (leeg)
    (define (new ==?)
      (make 0 '() '() ==?))

    ; from-scheme-list
    ; ( pair ( V V ➙ boolean ) ➙ uf-linked-list<V> )
    ; Maak een nieuwe gelinkte lijst van een bestaande Scheme lijst
    (define (from-scheme-list slst ==?)
      (define result (make 0 '() '() ==?))
      (if (not (null? slst))
          (let ((first (make-list-node (car slst) '() '())))
            (head! result first)
            (size! result 1)
            (rear!
             result
             (let loop
               ((scml (cdr slst))
                (curr first))
               (if (null? scml)
                   curr
                   (let ((node (make-list-node (car scml) '() first)))
                     (list-node-next! curr node)
                     (size! result (+ 1 (size result)))
                     (loop (cdr scml) node)))))))
      result)

    ; length
    ; ( uf-linked-list<V> ➙ number)
    (define (length ll)
      (size ll))

    ; full?
    ; ( uf-linked-list<V> ➙ boolean)
    (define (full? ll)
      #f)
 
    ; empty?
    ; ( uf-linked-list<V> ➙ boolean)
    (define (empty? ll)
      (null? (head ll)))

    ; add!
    ; ( uf-linked-list<V> V ➙ uf-linked-list<V>)
    ; Voegt een waarde 'val' toe achteraan de gelinkte lijst en geeft de gelinkte lijst terug als resultaat
    (define (add! ll val)
      (define new-node (make-list-node val '() '()))
      (if (empty? ll)
          (begin (head! ll new-node)
                 (rear! ll new-node)
                 (list-leader! new-node new-node))
          (begin (list-node-next! (rear ll) new-node)
                 (list-leader! new-node (head ll))
                 (rear! ll new-node)))
      (size! ll (+ (size ll) 1))
      ll)

    ;; TOEGEVOEGD: voor union! moet je gelinkte lijsten kunnen concateneren.
    (define (append! ll-1 ll-2)
      (let loop
        ((curr (head ll-2)))
        (if (not (null? curr))
            (begin (list-leader! curr (head ll-1))
                   (loop (list-node-next curr)))))
      (list-node-next! (rear ll-1) (head ll-2))
      (rear! ll-1 (rear ll-2))
      (size! ll-1 (+ (size ll-1) (size ll-2)))
      ll-1)
    ))