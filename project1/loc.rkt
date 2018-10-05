#lang typed/racket

(require "../include/cs151-core.rkt")

;; ==== ==== ==== ====
;; external interface

(provide File
         Rank
         (struct-out Loc)
         loc=?)

;; ==== ==== ==== ====
;; data definitions

(define-type File (U 'A 'B 'C 'D 'E 'F 'G 'H))
(define-type Rank (U 1 2 3 4 5 6 7 8))

(define-struct Loc
  ([file : File]
   [rank : Rank]))

;; ==== ==== ==== ====
;; operations

(: loc=? : Loc Loc -> Boolean)
(define (loc=? loc1 loc2)
  (match* (loc1 loc2)
    [((Loc f1 r1) (Loc f2 r2))
     (and (symbol=? f1 f2) (= r1 r2))]))