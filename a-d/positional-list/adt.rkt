#lang r7rs

;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*                                                                 *-*-
;-*-*                  Positional List code shared                    *-*-
;-*-*                   among all implementations                     *-*-
;-*-*                       Wolfgang De Meuter                        *-*-
;-*-*                   2018  Software Languages Lab                  *-*-
;-*-*                   Vrije Universiteit Brussel                    *-*-
;-*-*                                                                 *-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-

(define-library (positional-list adt)
  (export new from-scheme-list positional-list?
          next previous 
          map for-each
          find delete! peek update! add-before! add-after!
          first last has-next? has-previous?
          length empty? full?)
  (import (except (scheme base) length list? map for-each) 
          ;(a-d positional-list with-sentinel))
          (a-d positional-list without-sentinel))
  (begin
    (define (from-scheme-list slst ==?)
      (define result (new ==?))
      (if (null? slst)
          result
          (let for-all
            ((orig (cdr slst))
             (curr (first (add-after! result (car slst)))))
            (cond
              ((not (null? orig))
               (add-after! result (car orig) curr)
               (for-all (cdr orig) (next result curr)))
              (else
               result)))))
 
    (define (map plst f ==?)
      (define result (new ==?))
      (if (empty? plst)
          result
          (let for-all
            ((orig (first plst))
             (curr (first 
                    (add-after! result (f (peek plst (first plst)))))))
            (if (has-next? plst orig)
                (for-all (next plst orig) 
                  (next (add-after! result
                                    (f (peek plst (next plst orig))) 
                                    curr)
                        curr))
                result))))
 
    (define (for-each plst f)
      (if (not (empty? plst))
          (let for-all
            ((curr (first plst)))
            (f (peek plst curr))
            (if (has-next? plst curr)
                (for-all (next plst curr)))))
      plst)
 
    (define (add-before! plst val . pos)
      (define optional? (not (null? pos)))
      (cond 
        ((and (empty? plst) optional?)
         (error "illegal position (add-before!)" plst))
        ((or (not optional?) (eq? (car pos) (first plst)))
         (attach-first! plst val))
        (else
         (attach-middle! plst val (previous plst (car pos)))))
      plst)
 
    (define (add-after! plst val . pos)
      (define optional? (not (null? pos)))
      (cond
        ((and (empty? plst) optional?)
         (error "illegal position (add-after!)" plst))
        ((not optional?)
         (attach-last! plst val))
        (else
         (attach-middle! plst val (car pos))))
      plst)
 
    (define (delete! plst pos)
      (cond 
        ((eq? pos (first plst))
         (detach-first! plst))
        ((not (has-next? plst pos))
         (detach-last! plst pos))
        (else
         (detach-middle! plst pos)))
      plst)))