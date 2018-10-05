#lang typed/racket

(require typed/test-engine/racket-tests)

(require "../include/cs151-core.rkt")
(require "../include/cs151-image.rkt")
(require "../include/cs151-universe.rkt")
(require "../project2/chess-logic.rkt")
(require "../project1/optional.rkt")
(require "../project1/loc.rkt")

;; edited chess graphics
(: inchecktestboard : Board)
;; a board designed to use in eyeball tests  
(define inchecktestboard 
(append (list 'None 'None
      (Some (Piece 'King 'White)) (Some (Piece 'Rook 'White))
      'None (Some (Piece 'Bishop 'White)) 'None
      (Some (Piece 'Rook 'White)))
      (make-list 3 (Some (Piece 'Pawn 'White))) (list 'None 'None)
      (make-list 3 (Some (Piece 'Pawn 'White)))
      (make-list 2 'None)
      (list (Some (Piece 'Knight 'White))) (make-list 17 'None)
      (list
       (Some (Piece 'Queen 'Black))
       'None (Some (Piece 'Bishop 'White))
       'None 'None 'None
      (Some (Piece 'Pawn 'Black)) 'None (Some (Piece 'Pawn 'Black))
      'None 'None 'None)
      (make-list 2 (Some (Piece 'Pawn 'Black))) (list 'None 'None 'None) 
      (make-list 3 (Some (Piece 'Pawn 'Black))) (list (Some (Piece 'Rook 'Black)))
      (list (Some (Piece 'Knight 'Black))
      (Some (Piece 'Bishop 'Black)) 
      (Some (Piece 'King 'Black)) 'None (Some (Piece 'Bishop 'Black)) 
      'None (Some (Piece 'Rook 'Black)))))

(: square-string : Square -> String)
;; draws square as an image
(define (square-string s)
  (match s
    ['None " "]
    [(Some (Piece t 'White))
           (match t
             ['King "♔"]
             ['Queen "♕"]
             ['Rook "♖"]
             ['Bishop "♗"]
             ['Knight "♘"]
             ['Pawn "♙"])]
    [(Some (Piece t 'Black))
           (match t
             ['King "♚"]
             ['Queen "♛"]
             ['Rook "♜"]
             ['Bishop "♝"]
             ['Knight "♞"]
             ['Pawn "♟"])]))


(: singlesq : Square Image-Color Integer -> Image)
;; helper function that outputs a single square with piece on it
(define (singlesq s c n)
(overlay (text (square-string s) (if (and (> (quotient n 2) 0) (< (quotient n 2) 255))
                                     (cast (quotient n 2) Byte) 
                                     255) "black") (square n "solid" c)))

;;eyeball tests 
(singlesq (Some (Piece 'Queen 'Black)) "brown" 35)
(singlesq (Some (Piece 'Queen 'Black)) "beige" 35)
(singlesq (Some (Piece 'Queen 'White)) "brown" 35)
(singlesq (Some (Piece 'Queen 'White)) "beige" 35)

(: onerow : (Listof Square) Image-Color Image-Color Integer -> Image)
;; helper function that outputs a single row of squares 
(define (onerow ls c1 c2 n)
  (match ls
    ['() empty-image]
    [(cons f r)
     (beside (singlesq f c1 n)
             (singlesq (list-ref ls 1) c2 n) (onerow (drop ls 2) c1 c2 n))]))
;; eyeball test
(onerow inchecktestboard "beige" "brown" 35)

(: eightrows : (Listof Square) Image-Color Image-Color Integer -> Image)
;; helper function that outputs eight rows of alternating
;; colors on the board
(define (eightrows ls c1 c2 n) 
 (if (>= (length ls) 8) 
  (above (onerow (take ls 8) c1 c2 n) (eightrows (drop ls 8) c2 c1 n)) empty-image)) 

;; eyeball test
(eightrows inchecktestboard "beige" "brown" 35)

(: sqrorder : Board -> Board)
;; helper function that outputs the right order for the
;; putting of squares on the board 
(define (sqrorder ls)
    (local
      {(define lq (reverse ls))}
      (local
        {(: kk : (Listof Square) -> (Listof Square))
         (define (kk lm)
  (match lm
    ['() '()]
    [_ (append (reverse (take lm 8)) (kk (drop lm 8)))]))}
        (kk lq)))) 
;;eyeball test
(sqrorder inchecktestboard)

(: board->image : Board Integer -> Image)
;; draws the board
(define (board->image b n)
(eightrows (sqrorder b) "beige" "brown" n))

(provide board->image)

;; Eyeball test for sample board 
(board->image inchecktestboard 35)