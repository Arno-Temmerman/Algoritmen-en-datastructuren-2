#lang r7rs

;; Algoritmen en datastructuren II
;; Vraag 2 - DFT & BFT als routeplanner

;; a) Adjacency list representatie is het meest geschikt voor het opslaan van
;;    wegennetwerken omdat een wegnetwerk een ijle graaf vormt. Slechts enkele
;;    steden zijn rechtstreeks met elkaar verbonden via een snelweg.
;;    Bij deze representatie is het gebruikte geheugen voor het voorstellen van de bogen
;;    evenredig met het aantal effectief bestaande bogen. Bij de adjacency
;;    matrix representatie is dit geheugengebruik altijd evenredig aan het 
;;    maximaal aantal mogelijke knopen omdat ook "niet-bogen" opgeslagen worden.

(import (scheme base)    
        (scheme write)
        (a-d graph labeled config)
        (except (a-d graph-traversing dft-labeled)
                root-nop node-nop edge-nop)
        (a-d graph-traversing bft-labeled)
        (a-d graph examples undirected-labeled-unweighted)) 

;; Hulpfunctie om routes af te beelden:
(define (display-route route-graph town-nodes)
  (define (iter towns count)
    (let ((from (car towns)))
      (display (label route-graph from))
      (if (not (null? (cdr towns)))
          (let* ((to (cadr towns)))
            (display " --[")
            (display (edge-label route-graph from to))
            (display "]--> ")
            (iter (cdr towns) (+ count 1)))
          count)))
  (if (and (list? town-nodes) (not (null? town-nodes))
           (not (null? (cdr town-nodes))))             ;minstens 2 nodes
      (let ((towns-in-between (- (iter town-nodes 0) 1)))
        (display " (")
        (display towns-in-between)
        (display " towns in between)")(newline))
      (display "Not a valid route!")))

(define (routeplanner-dft g start goal)
  (define route '())
  ;; Ga diepte eerst door de graaf en
  ;; geef een route van start naar goal
  (dft g
       root-nop                      ;root-discovered
       (lambda (node node-label)     ;node-discovered
         (set! route (cons node route))
         (not (eq? node goal)))      ;stop on #f
       node-nop                      ;node-processed
       edge-nop                      ;edge-discovered
       (lambda (from to edge-label)  ;edge-processed
         (set! route (cdr route)))
       edge-nop                      ;edge-bumped
       (list start))                 ;geef start-node als enige root!
  
  (display-route g (reverse route)))
       

;;Testen:
(display "DFT:")(newline)
(routeplanner-dft highways-belgium Oostende Aarlen)(newline)
(routeplanner-dft highways-belgium La_Louviere Zeebrugge)(newline)
(newline)


(define (routeplanner-bft g start goal)
  (define parent-vector (make-vector (order g) #f))
  ;; Ga breedte-eerst door de graaf en
  ;; geef een route van start naar goal
  
  (define (route-list from-node)
    (define route '())
    (define (iter node)
      (let ((parent (vector-ref parent-vector node)))
        (set! route (cons node route))
        (if parent
            (iter parent))))
    (iter from-node)
    route)
  
  (bft g
       root-nop                      ;root-discovered
       (lambda (node label)          ;node-discovered
         (not (eq? node goal)))      ;stopt op #f
       (lambda (from to edge-label)  ;edge-discovered
         (vector-set! parent-vector to from))
       edge-nop                      ;edge-bumped
       (list start))                 ;geef start-node als enige root!
  
  (display-route g (route-list goal)))


;;Testen:
(display "BFT:")(newline)
(routeplanner-bft highways-belgium Oostende Aarlen)(newline)
(routeplanner-bft highways-belgium La_Louviere Zeebrugge)(newline)
(newline)
