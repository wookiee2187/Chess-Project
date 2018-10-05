
#lang typed/racket
(require typed/test-engine/racket-tests)

(require "../include/cs151-core.rkt")
(require "../include/cs151-image.rkt")
(require "../include/cs151-universe.rkt")
(require "../project2/chess-logic.rkt")
(require "../project1/optional.rkt")
(require "../project1/loc.rkt")
(require "../project2/chess-graphics.rkt")
(require typed/test-engine/racket-tests)


(define-struct ChessWorld
  ([cg : ChessGame]
   [s : Integer]
   [x : Integer]
   [y : Integer]
   [cl : (Listof Boolean)]
   [p : (Optional Move)]))

(: new-chess-world : Integer -> ChessWorld)
;; function that takes in an integer and returns a chessworld of that size
(define (new-chess-world n)
  (ChessWorld new-game n 0 0 (make-list 64 #f) 'None))

(check-expect (new-chess-world 20)
              (ChessWorld new-game 20 0 0 (make-list 64 #f) 'None))
(check-expect (new-chess-world 40)
              (ChessWorld new-game 40 0 0 (make-list 64 #f) 'None))
    
(: world-from-game : ChessGame Integer -> ChessWorld)
;; function that takes in an integer
;; and chessgame and returns a chessworld of that size of that chessgame
(define (world-from-game cg n)
  (match cg
    [(ChessGame b _)
  (ChessWorld cg n 0 0 (make-list (length b) #f) 'None)]))

(check-expect (world-from-game new-game 40)
              (ChessWorld new-game 40 0 0 (make-list 64 #f) 'None))

(check-expect (world-from-game tstgm3 60)
              (ChessWorld tstgm3 60 0 0 (make-list 64 #f) 'None))

(: ListofLocs : (Listof Loc))
;; list of possible locs to be used in a helper function 
(define ListofLocs
  (list (Loc 'A 8) (Loc 'B 8) (Loc 'C 8) (Loc 'D 8) (Loc 'E 8) (Loc 'F 8)
        (Loc 'G 8) (Loc 'H 8)
        (Loc 'A 7) (Loc 'B 7) (Loc 'C 7) (Loc 'D 7) (Loc 'E 7) (Loc 'F 7)
        (Loc 'G 7) (Loc 'H 7)
        (Loc 'A 6) (Loc 'B 6) (Loc 'C 6) (Loc 'D 6) (Loc 'E 6) (Loc 'F 6)
        (Loc 'G 6) (Loc 'H 6)
        (Loc 'A 5) (Loc 'B 5) (Loc 'C 5) (Loc 'D 5) (Loc 'E 5) (Loc 'F 5)
        (Loc 'G 5) (Loc 'H 5)
        (Loc 'A 4) (Loc 'B 4) (Loc 'C 4) (Loc 'D 4) (Loc 'E 4) (Loc 'F 4)
        (Loc 'G 4) (Loc 'H 4)
        (Loc 'A 3) (Loc 'B 3) (Loc 'C 3) (Loc 'D 3) (Loc 'E 3) (Loc 'F 3)
        (Loc 'G 3) (Loc 'H 3)
        (Loc 'A 2) (Loc 'B 2) (Loc 'C 2) (Loc 'D 2) (Loc 'E 2) (Loc 'F 2)
        (Loc 'G 2) (Loc 'H 2)
        (Loc 'A 1) (Loc 'B 1) (Loc 'C 1) (Loc 'D 1) (Loc 'E 1) (Loc 'F 1)
        (Loc 'G 1) (Loc 'H 1)))

(: listorder : All (A) (Listof A) -> (Listof A))
;; helper function that outputs the right order for the
;; putting of squares on the board 
(define (listorder ls)
    (local
      {(define lq (reverse ls))}
      (local
        {(: kk : (Listof A) -> (Listof A))
         (define (kk lm)
  (match lm
    ['() '()]
    [_ (append (reverse (take lm 8)) (kk (drop lm 8)))]))}
        (kk lq))))

(check-expect (listorder ListofLocs)
              (list
 (Loc 'A 1)
 (Loc 'B 1)
 (Loc 'C 1)
 (Loc 'D 1)
 (Loc 'E 1)
 (Loc 'F 1)
 (Loc 'G 1)
 (Loc 'H 1)
 (Loc 'A 2)
 (Loc 'B 2)
 (Loc 'C 2)
 (Loc 'D 2)
 (Loc 'E 2)
 (Loc 'F 2)
 (Loc 'G 2)
 (Loc 'H 2)
 (Loc 'A 3)
 (Loc 'B 3)
 (Loc 'C 3)
 (Loc 'D 3)
 (Loc 'E 3)
 (Loc 'F 3)
 (Loc 'G 3)
 (Loc 'H 3)
 (Loc 'A 4)
 (Loc 'B 4)
 (Loc 'C 4)
 (Loc 'D 4)
 (Loc 'E 4)
 (Loc 'F 4)
 (Loc 'G 4)
 (Loc 'H 4)
 (Loc 'A 5)
 (Loc 'B 5)
 (Loc 'C 5)
 (Loc 'D 5)
 (Loc 'E 5)
 (Loc 'F 5)
 (Loc 'G 5)
 (Loc 'H 5)
 (Loc 'A 6)
 (Loc 'B 6)
 (Loc 'C 6)
 (Loc 'D 6)
 (Loc 'E 6)
 (Loc 'F 6)
 (Loc 'G 6)
 (Loc 'H 6)
 (Loc 'A 7)
 (Loc 'B 7)
 (Loc 'C 7)
 (Loc 'D 7)
 (Loc 'E 7)
 (Loc 'F 7)
 (Loc 'G 7)
 (Loc 'H 7)
 (Loc 'A 8)
 (Loc 'B 8)
 (Loc 'C 8)
 (Loc 'D 8)
 (Loc 'E 8)
 (Loc 'F 8)
 (Loc 'G 8)
 (Loc 'H 8)))
                         
(: ordrloc : (Listof Loc))
;; listof loc stored in order
(define ordrloc
  (listorder ListofLocs))

(: numtofile : Integer -> File)
;; nums conversted to files  
(define (numtofile n)
  (match n 
    [1 'A]
    [2 'B]
    [3 'C]
    [4 'D]
    [5 'E]
    [6 'F]
    [7 'G]
    [8 'H]
    [_ (error "numtofile: doesn't work")]))

(check-expect (numtofile 3) 'C)
(check-error (numtofile 13) "numtofile: doesn't work")

(: filetonum : File -> Integer)
;; files converted to num
(define (filetonum n)
  (match n 
    ['A 1]
    ['B 2]
    ['C 3]
    ['D 4]
    ['E 5]
    ['F 6]
    ['G 7]
    ['H 8]
    ))
(check-expect (filetonum 'C) 3)
(check-expect (filetonum 'A) 1)

(: numtorank : Integer -> Rank)
;; converts nums to rank 
(define (numtorank n)
  (match n 
    [1 1]
    [2 2]
    [3 3]
    [4 4]
    [5 5]
    [6 6]
    [7 7]
    [8 8]
    [_ (error "numtorank: doesn't work")]))

(check-expect (numtorank 2) 2)
(check-error (numtorank 14) "numtorank: doesn't work")

(: numtoopprank : Integer -> Rank)
;; converts nums to opposite rank 
(define (numtoopprank n)
  (match n 
    [1 8]
    [2 7]
    [3 6]
    [4 5]
    [5 4]
    [6 3]
    [7 2]
    [8 1]
    [_ (error "numtoopprank: doesn't work")]))

(check-expect (numtoopprank 2) 7)
(check-error (numtoopprank 14) "numtoopprank: doesn't work")

(: ranktonum : Rank -> Integer)
;; converts rank to num
;; written separately because ranks are
;; giving type errors if taken as nums
(define (ranktonum n)
  (match n 
    [1 1]
    [2 2]
    [3 3]
    [4 4]
    [5 5]
    [6 6]
    [7 7]
    [8 8]
    ))
(check-expect (ranktonum 2) 2)
(check-expect (ranktonum 4) 4)

(: r1 : Loc -> Loc)
;; changes the rs of the locs to opposite 
(define (r1 l)
  (match l
    [(Loc f r)
  (Loc f (match r
    [1 8]
    [2 7]
    [3 6]
    [4 5]
    [5 4]
    [6 3]
    [7 2]
    [8 1]))]))
(check-expect (r1 (Loc 'E 2)) (Loc 'E 7))
(check-expect (r1 (Loc 'D 3)) (Loc 'D 6))

(: locinordr : Loc -> Loc)
;; gives out the loc in right order 
(define (locinordr l)
  (local {(: kk : (Listof Loc) -> Integer)
          (define (kk ls)
            (match ls
              [(cons f r)
               (if (loc=? f l) 0 (+ 1 (kk r)))]))}
  (list-ref ordrloc (kk ListofLocs))))

(check-expect (locinordr (Loc 'E 2)) (Loc 'E 7))
(check-expect (locinordr (Loc 'D 3)) (Loc 'D 6))

(: off-grid? : Integer Integer Integer -> Boolean)
;; checks if ints make it off grid of chessboard
(define (off-grid? x y s)
     (or (> x (* 8 s)) (> y (* 8 s))))

(check-expect (off-grid? 2000 2000 50) #t)
(check-expect (off-grid? 20 20 50) #f)

(: matchloc : Integer Integer Integer -> Loc)
;; matches x and y to the loc on board 
(define (matchloc s x y)
       (Loc (numtofile (+ 1 (truncate (/ x s))))
            (numtoopprank (+ 1 (truncate (/ y s))))))

(check-expect (matchloc 10 10 50) (Loc 'B 3))
(check-expect (matchloc 20 1 50) (Loc 'A 6))

(: firstt : (Listof Boolean) -> Integer)
;; finds where the first t is 
(define (firstt l)
   (match l
    ['() (error "firstt: no ts")]
    [(cons #t _) 0]
    [(cons f r) (+ 1 (firstt r))]))

(check-expect (firstt '(#f #t)) 1)
(check-error (firstt '(#f #f)) "firstt: no ts")
(check-expect (firstt '(#f #f #t)) 2)
(check-expect (firstt '(#t #f #t)) 0)

(: reverserankandfile : Loc -> Loc)
;; reverses the rank and file of the loc 
(define (reverserankandfile l)
  (match l
    [(Loc f r)
     (Loc (numtofile (ranktonum r)) (numtorank (filetonum f)))]))

(check-expect (reverserankandfile (Loc 'E 7)) (Loc 'G 5))
(check-expect (reverserankandfile (Loc 'F 7)) (Loc 'G 6))
     

(: handle-click : ChessWorld Integer Integer Mouse-Event -> ChessWorld)
;; clicking the world highlights it
;; overlay with the list of booleans of clicking 
(define (handle-click w x y me)
  (match* (w me)
    [((ChessWorld cg s x1 y1 cl pl) "button-down")
     (if (off-grid? x y s) w
     (match cg
       [(ChessGame b h)
     (if (andmap false? cl)
     (match (matchloc s x y)
       [(Loc f1 r1)
        (ChessWorld cg s x1 y1
                    (map (lambda ([l : Loc])
                           (match l
                             [(Loc f r)
                              (and (symbol=? f f1) (= r r1))
                                 ]
                             )) ListofLocs) pl)])
      (match (board-ref
                  b 
                  (list-ref
                   ListofLocs (firstt cl)))
      ['None (ChessWorld cg s x1 y1 (make-list 64 #f) 'None)]
      [_ 
       (if  (need-promote? (Move 
                            (list-ref ListofLocs (firstt cl))
                            (matchloc s x y)
                             (val-of (board-ref
                              b 
                              (list-ref
                               ListofLocs (firstt cl))))   
                                   (board-ref b (matchloc s x y)) 'None))
           (ChessWorld cg s x1 y1 cl (Some (Move 
                            (list-ref ListofLocs (firstt cl))
                            (matchloc s x y)
                             (val-of (board-ref
                              b 
                              (list-ref
                               ListofLocs (firstt cl))))   
                                   (board-ref b (matchloc s x y)) 'None)))
                 (ChessWorld
       (if (dstinlist (Some (matchloc s x y))
                      (en-passant cg
                               (list-ref ListofLocs (firstt cl))))
          (apply-move cg (val-of (dstoutlist (Some (matchloc s x y))
                      (en-passant cg
                               (list-ref ListofLocs (firstt cl)))))) 
       (if (legal-move? cg (Move 
                            (list-ref ListofLocs (firstt cl))
                            (matchloc s x y)
                             (val-of (board-ref
                              b 
                              (list-ref
                               ListofLocs (firstt cl))))   
                                   (board-ref b (matchloc s x y)) 'None))  
                     (apply-move cg (Move 
                            (list-ref ListofLocs (firstt cl))
                            (matchloc s x y)
                            (val-of
                             (board-ref
                              b 
                              (list-ref
                               ListofLocs (firstt cl)))) 
                               (board-ref b (matchloc s x y)) 'None))
         cg)) s x1 y1 (make-list 64 #f) 'None))]))
     ]))] 
       [(_ _) w]))
;; no checks as it outputs a chessworld and depends on interaction
;; did check extensively with trying different moves myself 

(: promoteto : ChessWorld String -> ChessWorld)
;; if a pawn has reached the end gives the option of
;; promoting the pawn by pressing a key
(define (promoteto cw st)
  (match cw
    [(ChessWorld cg sd x y cl pl) 
          (if (not (move? pl)) cw
              (match pl
              [(Some (Move s d m c p))
               (ChessWorld (match st 
                  ["q"
                   (if (legal-move? cg (Move s d m c (Some 'Queen)))
                       (apply-move cg (Move s d m c (Some 'Queen))) cg)]
                  ["n" 
                    (if (legal-move? cg (Move s d m c (Some 'Knight)))
                       (apply-move cg (Move s d m c (Some 'Knight))) cg)]
                  ["r"
                   (if (legal-move? cg (Move s d m c (Some 'Rook)))
                       (apply-move cg (Move s d m c (Some 'Rook))) cg)]
                  ["b"
                    (if (legal-move? cg (Move s d m c (Some 'Bishop)))
                       (apply-move cg (Move s d m c (Some 'Bishop))) cg)]
                  [_ cg]) sd x y cl
                                                             'None)]
                ['None cw]))]
                ))
;; no checks as outputs world 

(: move? : (Optional Move) -> Boolean)
;; checks if there is a move in the optional move
(define (move? m)
  (match m
    ['None #f]
    [_ #t]))

(check-expect
 (move? (Some (Move (Loc 'A 2) (Loc 'A 3) (Piece 'Pawn 'White)
                    'None 'None))) #t)

(check-expect
 (move? 'None) #f)


(: onesqr : Boolean Integer -> Image)
;; gives out a square highlighted if clicked
;; nothing if not 
(define (onesqr b s)
  (match b
    [#t (square s "solid"
          (color 100 200 0 128))]
    [#f (square s "outline" "black")]))

;;eyeball tests

(onesqr #f 20)
(onesqr #t 30)
(onesqr #f 60)
(onesqr #t 80)

(: cldraw : (Listof Boolean) Integer -> Image)
;; chessworld draws only the cl from the chessworld 
(define (cldraw cl s)
  (match cl
    ['() empty-image]
    [(cons f r)
     (beside (onesqr f s)
             (onesqr (list-ref cl 1) s)
             (cldraw (drop cl 2) s))]))
;;eyeball tests
(cldraw '(#t #f #f #f #t #f) 20)
(cldraw '(#f #f #f #f #f #f) 20)
(cldraw '(#t #t #t #t #t #t) 20)

(: clbrd : (Listof Boolean) Integer -> Image)
;; cl as a board
(define (clbrd cl s)
  (if (>= (length cl) 8)
      (above (cldraw (take cl 8) s)
             (clbrd (drop cl 8) s)) empty-image))
;;eyeball tests
(clbrd '(#t #f #f #f #t #f) 20)
(clbrd '(#f #f #f #f #f #f) 20)
(clbrd '(#t #t #t #t #t #t) 20)

(: oppcolor : Player -> Player)
;; gives opp color
(define (oppcolor p)
  (match p
    ['Black 'White]
    ['White 'Black]))

(check-expect (oppcolor 'Black) 'White)
(check-expect (oppcolor 'White) 'Black)

(: draw-chess-world : ChessWorld -> Image)
;; draws the chess world 
(define (draw-chess-world cw)
  (match cw
    [(ChessWorld cg s _ _ cl pl)
     (match cg
       [(ChessGame b h)
        (above
         (if (not (empty? h))
                 (if (move? pl)
                     (above (text "Press key to promote" 20 "blue")
                     (text "n- Knight q- Queen b- Bishop" 20 "blue")
                     (text "r- Rook" 20 "blue"))
                            (overlay (clbrd cl s) (board->image b s)))
             (overlay (clbrd cl s) (board->image starting-board s)))  
         (local {
                 (define s 
                   (if (empty? h) "White"
                       (match (oppcolor
                               (Piece-color (Move-moved (first h))))
                         ['White "White"]
                         ['Black "Black"])))
                 (define k
                   (if (in-check? cg) 
                       "True" "False"))
                 (define l
                   (if (checkmate? cg)
                       (match cg
                         [(ChessGame b h)
                          (match (first h)
                            [(Move _ _ mvd _ _)
                             (string-append "True : "
                                            (playertostring
                                             (Piece-color mvd)))])])
                        "False"))
                 (define o
                   (if (not (and (checkmate? cg)
                            (stalemate? cg))) "True" "False"))
                 (define p
                   (if (stalemate? cg)  
                       "True: Draw" "False"))
                 }
           (above (text (string-append "Turn: " s) 20 "black")
           (text (string-append " Game in progress: " o) 20 "black")
           (text (string-append " In-check: " k) 20 "black")
           (text (string-append " Checkmate: " l) 20 "black")
           (text (string-append " Stalemate: " p) 20 "black")
           )
           ))])]))
;; no checks because it is an image
(: playertostring : Player -> String)
(define (playertostring p)
  (match p
    ['Black "Black"]
    ['White "White"]))
        
(: play-new : Integer -> ChessWorld)
;; play new game of this size 
(define (play-new n)   
  (big-bang (new-chess-world n) : ChessWorld
            [to-draw draw-chess-world]
            [on-mouse handle-click]
            [on-key promoteto]))
;; no checks because chessworld

(: play-from : ChessGame Integer -> ChessWorld)
;; play from a starting chessgame of this size
(define (play-from cg n)   
  (big-bang (world-from-game cg n) : ChessWorld
            [to-draw draw-chess-world]
            [on-mouse handle-click]
            [on-key promoteto]))
;; no checks because chessworld

(: quit-when : ChessWorld -> Boolean)
;; quit when checkmate or stalemate
(define (quit-when cw)
  (match cw
    [(ChessWorld cg _ _ _ _ _)
     (or (checkmate? cg) (stalemate? cg))]))

(test)