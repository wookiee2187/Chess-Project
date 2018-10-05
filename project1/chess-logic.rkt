#lang typed/racket

(require typed/test-engine/racket-tests)

(require "../include/cs151-core.rkt")
(require "../include/cs151-image.rkt")

(require "../project1/optional.rkt")
(require "../project1/loc.rkt")


;; ==== ==== ==== ====
;; external interface
;; USED IN HOUSE SOLUTION for part 2 of project 
(provide PieceType
         Player
         (struct-out Piece)
         Square
         Board
         (struct-out Move)
         PromoteTo
         (struct-out ChessGame)
         starting-board ; : Board
         new-game       ; : ChessGame
         board-ref      ; : Board Loc -> Square
         board-update   ; : Board Loc Square -> Board
         in-check?      ; : ChessGame -> Boolean
         legal-move?    ; : ChessGame Move -> Boolean
         moves-piece    ; : ChessGame Loc -> (Listof Move)
         moves-player   ; : ChessGame -> (Listof Move)
         checkmate?     ; : ChessGame -> Boolean
         stalemate?     ; : ChessGame -> Boolean
         apply-move     ; : ChessGame Move -> ChessGame
         strings->board ; : (Listof String) -> Board
         )

;; ==== ==== ==== ====
;; data definitions

(define-type PieceType
  (U 'Pawn 'Bishop 'Knight 'Rook 'King 'Queen))

(define-type Player
  (U 'Black 'White))

(define-struct Piece
  ([type  : PieceType]
   [color : Player]))

(define-type Square
  (Optional Piece))

(define-type Board
  (Listof Square))

(define-type PromoteTo
  (U 'Queen 'Rook 'Bishop 'Knight))

(define-struct Move
  ([src        : Loc]
   [dst        : Loc]
   [moved      : Piece]
   [captured   : (Optional Piece)]
   [promote-to : (Optional PromoteTo)]))

(define-struct ChessGame
  ([board : Board]
   [history : (Listof Move)]))

;; ==== ==== ==== ====
;; supplemental data definitions

;; The Vec data structure is used to enact moves to other locations.
;; In some cases it represents a direction, like (Vec 1 1) for "northeast";
;; in other cases it represents a displacement, like (Vec 2 1) for a knight.

(define-struct Vec
  ([delta-rank : (U -2 -1 0 1 2)]
   [delta-file : (U -2 -1 0 1 2)]))

;; ==== ==== ==== ====
;; === option operations
;; NOTE it might make sense to put these in optional.rkt

(: none? (All (T) ((Optional T) -> Boolean)))
;; test if an option is 'None
(define (none? opt)
  (match opt ['None #t] [_ #f]))

(: some? (All (T) ((Optional T) -> Boolean)))
;; test if an option is Some
(define (some? opt)
  (not (none? opt)))

(: opt=? (All (T) ((T T -> Boolean) (Optional T) (Optional T) -> Boolean)))
;; equality test, with option wrapper taken into account
(define (opt=? eq opt1 opt2)
  (match* (opt1 opt2)
    [('None 'None) #t]
    [((Some a) (Some b)) (eq a b)]
    [(_ _) #f]))
   
;; === general purpose utilities

(: find : All (A B) (A A -> Boolean) A (Listof (List A B)) -> (Optional B))
;; find value associated with key, if there is one
;; note: List is here used as a pair constructor and is distinct from Listof
(define (find eq key finite-map)
  (local
    {(: fnd : (Listof (List A B)) -> (Optional B))
     (define (fnd pairs)
       (match pairs
         ['() 'None]
         [(cons (list k v) rest) (if (eq k key) (Some v) (fnd rest))]))}
    (fnd finite-map)))

(: list-update (All (A) ((Listof A) Integer A -> (Listof A))))
;; functional list update by position
;; ex: (list-update '(A B C D) 2 'X) ==> '(A B X D)
(define (list-update xs i x)
  (if (zero? i)
      (cons x (rest xs))
      (cons (first xs) (list-update (rest xs) (sub1 i) x))))

(: position-of : All (A) (A A -> Boolean) A (Listof A) -> (Optional Integer))
;; find the position of first occurence of item in list, if present 
(define (position-of eq x items)
  (local
    {(: pos : (Listof A) Integer -> (Optional Integer))
     (define (pos xs i)
       (match xs
         ['() 'None]
         [(cons first rest)
          (if (eq first x) (Some i) (pos rest (add1 i)))]))}
    (pos items 0)))

(: index-of : All (A) (A -> Boolean) (Listof A) -> (Optional Integer))
;; find position of first item to pass the test in list, if there is one
;; note: position-of is for equality, index-of is generalized to any property
(define (index-of f xs)
  (local
    {(: index : (Listof A) Integer -> (Optional Integer))
     (define (index xs i)
       (match xs
         ['() 'None]
         [(cons first rest)
          (if (f first) (Some i) (index rest (add1 i)))]))}
    (index xs 0)))

(: map-append : All (A B) (A -> (Listof B)) (Listof A) -> (Listof B))
;; gather a list of list results into one list
;;   - like map, but using append in place of cons
;; ex: (map-append countdown '(3 2 3)) ==> '(3 2 1 2 1 3 2 1)
;;   - assuming (countdown 3) ==> '(3 2 1) etc.
;; cf: (map countdown '(3 2 3)) ==> '((3 2 1) (2 1) (3 2 1))
(define (map-append f xs)
  (match xs
    ['() '()]
    [(cons first rest) (append (f first) (map-append f rest))]))

;; === board construction from strings

;; a mapping of characters to piece types
(define piece-map
  (list (list #\p 'Pawn)
        (list #\r 'Rook)
        (list #\b 'Bishop)
        (list #\n 'Knight)
        (list #\k 'King)
        (list #\q 'Queen)))

(: char->piece : Char -> Piece)
(define (char->piece c)
  (match (find char=? (char-downcase c) piece-map)
    ['None (error (string-append "no such piece: " (string c)))]
    [(Some type) (Piece type (if (char-upper-case? c) 'Black 'White))]))

(: char->square : Char -> Square)
(define (char->square c)
  (match c
    [#\- 'None]
    [_ (Some (char->piece c))]))

(: strings->board : (Listof String) -> Board)
;; turn a list of eight strings into a board
;; see starting-board below for an example
(define (strings->board ss)
  (local
    {(: string->squares : String -> (Listof Square))
     (define (string->squares s)
       (map char->square (string->list s)))}
  (map-append string->squares (reverse ss))))

;; === new chess game

(: starting-board : Board)
(define starting-board
  (strings->board (list "RNBQKBNR"
                        "PPPPPPPP"
                        "--------"
                        "--------"
                        "--------"
                        "--------"
                        "pppppppp"
                        "rnbqkbnr")))

(: new-game : ChessGame)
(define new-game
  (ChessGame starting-board '()))

;; === basic chess operations

(: rank->int : Rank -> Integer)
;; convert a rank on [1,8] to an Integer on [0,7]
(define (rank->int r)
  (sub1 r))

(: int->rank : Integer -> Rank)
;; convert an Integer on [0-7] to a rank [1,8]
(define (int->rank i)
  (cast (add1 i) Rank))

(: file->int : File -> Integer)
;; convert a file, from 'A-'H, to an Integer [0,7]
(define (file->int f)
  (val-of (position-of symbol=? f '(A B C D E F G H))))

(: int->file : Integer -> File)
;; convert an Integer on [0,7] to a file ['A,'H]
(define (int->file i)
  (list-ref '(A B C D E F G H) i))

(: add1-rank : Rank -> (Optional Rank))
;; increment a rank, return 'None if off the board
(define (add1-rank r)
  (if (< r 8) (Some (cast (add1 r) Rank)) 'None))

(: sub1-rank : Rank -> (Optional Rank))
;; decrement a rank, return 'None if off the board
(define (sub1-rank r)
  (if (> r 1) (Some (cast (sub1 r) Rank)) 'None))

(: add-rank : Rank Integer -> (Optional Rank))
;; compute a new rank, return 'None if off the board
(define (add-rank r d)
  (if (<= 1 (+ r d) 8)
      (Some (cast (+ r d) Rank))
      'None))

(: add1-file : File -> (Optional File))
;; increment a file, return 'None if off the board
(define (add1-file f)
  (match f
    ['A (Some 'B)]
    ['B (Some 'C)]
    ['C (Some 'D)]
    ['D (Some 'E)]
    ['E (Some 'F)]
    ['F (Some 'G)]
    ['G (Some 'H)]
    ['H 'None]))

(: sub1-file : File -> (Optional File))
;; decrement a file, return 'None if off the board
(define (sub1-file f)
  (match f
    ['A 'None]
    ['B (Some 'A)]
    ['C (Some 'B)]
    ['D (Some 'C)]
    ['E (Some 'D)]
    ['F (Some 'E)]
    ['G (Some 'F)]
    ['H (Some 'G)]))

(: add-file : File Integer -> (Optional File))
;; compute a new file, return 'None if off the board
(define (add-file f d)
  (cond
    [(zero? d) (Some f)]
    [(positive? d)
      (match (add1-file f)
        ['None 'None]
        [(Some f+1) (add-file f+1 (sub1 d))])]
    [else
      (match (sub1-file f)
        ['None 'None]
        [(Some f-1) (add-file f-1 (add1 d))])]))

(: add-loc : Loc Integer Integer -> (Optional Loc))
;; compute Some new location by adding to rank and file,
;; return 'None if off the board
(define (add-loc loc delta-rank delta-file)
  (match loc
    [(Loc file rank)
      (match (add-rank rank delta-rank)
        ['None 'None]
        [(Some rank~)
          (match (add-file file delta-file)
            ['None 'None]
            [(Some file~) (Some (Loc file~ rank~))])])]))

(: int->loc : Integer -> Loc)
;; 0 => A1, 1=>B1, ..., 8=>A2, ..., 63=>H8
(define (int->loc i)
  (if (or (< i 0) (> i 63))
      (error "int->loc: out of range")
      (Loc (int->file (remainder i 8))
           (int->rank (quotient i 8)))))

(: every-loc : (Listof Loc))
(define every-loc
  (build-list 64 int->loc))

(: loc->index : Loc -> Integer)
;; convert a Loc to a list index in the 1-D board representation
(define (loc->index loc)
  (match loc
    [(Loc file rank)
      (+ (* (rank->int rank) 8) (file->int file))]))

(: board-ref : Board Loc -> Square)
;; return the (Optional Piece) at the given location
(define (board-ref b loc)
  (list-ref b (loc->index loc)))

(: piece-at : Board Loc -> Piece)
;; similar to board ref, but return the piece or raise error
(define (piece-at b loc)
  (match (board-ref b loc)
    [(Some p) p]
    ['None (error "empty square")]))

(: unocc? : Board Loc -> Boolean)
;; return true if the location is unoccupied
(define (unocc? b loc)
  (none? (board-ref b loc)))

(: occ? : Board Loc -> Boolean)
;; return true if the location is occupied
(define (occ? b loc)
  (some? (board-ref b loc)))
             
(: board-update : Board Loc Square -> Board)
;; update the board location
(define (board-update b loc s)
  (list-update b (loc->index loc) s))

;; === general-purpose chess and chess move widgets

(: piece-type=? : PieceType PieceType -> Boolean)
;; checks if the piece types are the same 
(define (piece-type=? p1 p2)
  (symbol=? p1 p2))

(: player=? : Player Player -> Boolean)
;; checks if the player types are the same 
(define (player=? p1 p2)
  (symbol=? p1 p2))

(: piece=? : Piece Piece -> Boolean)
(define (piece=? p1 p2)
  (and (symbol=? (Piece-type p1) (Piece-type p2))
       (symbol=? (Piece-color p1) (Piece-color p2))))

(: move=? (Move Move -> Boolean))
;; equality tests for moves
(define (move=? m1 m2)
  (match* (m1 m2)
    [((Move s1 d1 m1 c1 p1) (Move s2 d2 m2 c2 p2))
     (and (loc=? s1 s2)
          (loc=? d1 d2)
          (piece=? m1 m2)
          (opt=? piece=? c1 c2)
          (opt=? symbol=? p1 p2))]))

(: opponent : Player -> Player)
;; return the other player
(define (opponent p)
  (match p
    ['Black 'White]
    ['White 'Black]))

(: opponents? : Board Loc Loc -> Boolean)
;; are the pieces on the squares opponents?
(define (opponents? b loc1 loc2)
  (match* ((board-ref b loc1) (board-ref b loc2))
    [((Some (Piece _  color1)) (Some (Piece _ color2)))
     (player=? color2 (opponent color1))]
    [(_ _) (error "one or both locations empty")]))

(: move-if-unocc : Board Loc Integer Integer -> (Listof Move))
;; given a board, a location, a delta-rank and a delta-file,
;; if the new location is unoccupied, return it in a singleton list
;; NOTE: this returns either empty or a list of length 1
;; -- this convention is for use in various calls to append below
(define (move-if-unocc b src delta-rank delta-file)
  (match (add-loc src delta-rank delta-file)
    ['None '()]
    [(Some dst)
     (match (board-ref b dst)
       ['None (list (Move src dst (piece-at b src) 'None 'None))]
       [(Some _) '()])]))

(: capture-if-occ : Board Loc Integer Integer -> (Listof Move))
;; given a board, a location, a delta-rank and a delta-file,
;; if the new location is unoccupied by an opponent's piece,
;; return it in a singleton list
;; NOTE: this returns either empty or a list of length 1
;; -- this convention is for use in various calls to append below
(define (capture-if-occ b loc delta-rank delta-file)
  (match (add-loc loc delta-rank delta-file)
    ['None '()]
    [(Some loc~)
     (if (and (occ? b loc~) (opponents? b loc loc~))
         (list (Move loc
                     loc~
                     (val-of (board-ref b loc))
                     (board-ref b loc~)
                     'None))
         '())]))

(: capture-if-unocc : Board Loc Integer Integer -> (Listof Move))
;; given a board, a location, a delta-rank and a delta-file,
;; if the new location is unoccupied by an opponent's piece, captures it
;; this is a new helper function written for en passant 
(define (capture-if-unocc b loc delta-rank delta-file)
  (match (add-loc loc delta-rank delta-file) 
    ['None '()]
    [(Some loc~)
     (match loc~
       [(Loc f r)
     (if (unocc? b loc~)
         (match (board-ref b loc)
           ['None '()]
           [(Some (Piece type player))
            (local {(define kk
                      (match player
                        ['Black 1]
                        ['White -1]))}
            (list (Move loc
                     loc~
                     (val-of (board-ref b loc))
                     (board-ref b (Loc f (val-of (add-rank r kk))))
                     'None)))])
         '())])]))

 
(: passtk2 : ChessGame Loc -> (Listof Move))
;; my en-passant function takes in chessgame
;; and loc and returns a list of move 
(define (passtk2 g l)
  (match g
    [(ChessGame b h)
     (match (board-ref b l)
       ['None '()]
       [(Some (Piece (not 'Pawn) _)) '()]
       [(Some (Piece 'Pawn player))
        (if (empty? h) '()
            (match (first h)
              [(Move s d mvd c p)
               (local {(define kk
                         (match player
                           ['Black -1]
                           ['White 1]))}
               (match mvd
                 [(Piece 'Pawn k)
                  (if (not (symbol=? k player))
                  (append
                    (if (and (= (abs (ranksbtr s d)) 2)
                       (opt=? loc=? (add-loc l 0 1) (Some d)))
                     (capture-if-unocc b l kk 1) '())
                    (if (and (= (abs (ranksbtr s d)) 2)
                        (opt=? loc=? (add-loc l 0 -1) (Some d)))
                        (capture-if-unocc b l kk -1) '())) '())]
                 [_ '()]
                 ))]))])]))
(provide passtk2)


(: dstinlist : (Optional Loc) (Listof Move) -> Boolean)
;; checks if a loc is in a list of moves as dst
(define (dstinlist l lm)
  (match lm
    ['() #f]
    [_ (ormap (lambda ([m : Move])
              (match m
                [(Move _ d _ _ _)
                 (opt=? loc=? (Some d) l)])) lm)]))

(provide dstinlist)


(: dstoutlist : (Optional Loc) (Listof Move) -> (Optional Move))
;; checks if a loc is in a list of moves as dst
(define (dstoutlist l lm)
  (match lm
    ['() 'None]
    [(cons f r)
     (match f
       [(Move _ d _ _ _) 
        (if (opt=? loc=? (Some d) l) (Some f) (dstoutlist l r))])]))

(provide dstoutlist)



(: en? : ChessGame Move -> Boolean)
;; checks if the move made was en passant 
(define (en? cg mv)
  (match cg
    [(ChessGame b h)
     (match h
       ['() #f]
       [_ (match mv
        [(Move s1 d1 mvd1 c1 p1)
        (contains? mv (passtk2 cg s1))]
            )]
       )]))

     
(: contains? : Move (Listof Move) -> Boolean)
;; checks if a move is in a list of moves
(define (contains? mv lmv)
  (match lmv
    ['() #f]
    [_ (ormap (lambda ([m : Move])
              (move=? m mv)) lmv)]))

                    
(: ranksbtr : Loc Loc -> Integer)
;; gives out the ranks sub as an integer
(define (ranksbtr l1 l2)
  (match* (l1 l2)
      [((Loc _ r1) (Loc _ r2))
       (- r1 r2)]))
                      

(: move-if-possible : Board Loc Vec -> (Listof Move))
;; move to given other location, directly, with capture if possible
;; (this is useful for kings and knights)
;; NOTE: this returns either empty or a list of length 1
;; -- this convention is for use in various calls to append below
(define (move-if-possible b loc vec)
  (match vec
    [(Vec delta-rank delta-file)
     (local
       {(: mv : Loc -> (Listof Move))
        ;; construct a Move for the moving piece
        (define (mv dst)
          (list (Move loc dst (piece-at b loc) (board-ref b dst) 'None)))}
       (match (add-loc loc delta-rank delta-file)
         ['None '()]
         [(Some loc~)
          (cond
            [(and (occ? b loc~) (opponents? b loc loc~)) (mv loc~)]
            [(unocc? b loc~) (mv loc~)]
            [else '()])]))]))

(: explore (Board Loc Vec -> (Listof Move)))
;; explore in the given direction until the moving piece
;; either runs into a teammate, captures an opponent, or
;; walks off the board
(define (explore b loc vec)
  (match vec
    [(Vec delta-rank delta-file)
     (if (and (zero? delta-rank) (zero? delta-file))
         (error "explore: infinite loop")
         (local
           {(: mv : Loc -> Move)
            ;; construct a move to open location
            (define (mv dst)
              (Move loc dst (piece-at b loc) 'None 'None))
            (: capture : Loc -> Move)
            ;; construct a capture
            (define (capture dst)
              (Move loc dst (piece-at b loc) (board-ref b dst) 'None))
            (: lp : Loc -> (Listof Move))
            (define (lp curr)
              (match (add-loc curr delta-rank delta-file)
                ['None '()]
                [(Some loc~)
                 (cond
                   [(occ? b loc~) (if (opponents? b loc loc~)
                                      (list (capture loc~))
                                      '())]
                   [(unocc? b loc~) (cons (mv loc~) (lp loc~))]
                   [else (error "this branch unreachable")])]))}
           (lp loc)))]))

(define castle-vecs
  (list (Vec 0 2) (Vec 0 -2)))
   
(define rook-vecs
  (list (Vec 1 0) (Vec 0 1) (Vec -1 0) (Vec 0 -1)))

(define bishop-vecs
  (list (Vec 1 1) (Vec -1 1) (Vec -1 -1) (Vec 1 -1)))

(define knight-vecs
;; in the case of knights, these are not vectors but displacements
  (list (Vec -2 -1) (Vec -2 1)
        (Vec -1 -2) (Vec -1 2)
        (Vec  1 -2) (Vec  1 2)
        (Vec  2 -1) (Vec  2 1)))

(define queen-vecs
  (append rook-vecs bishop-vecs))


(: piece-at? : Board Piece (Optional Loc) -> Boolean)
;; check if there is a particular piece at the given loc
(define (piece-at? b pc opt-loc)
  (match opt-loc
    ['None #f]
    [(Some loc)
     (match (board-ref b loc)
       [(Some pc~) (piece=? pc pc~)]
       [_ #f])]))

(: piece-along? : Board Loc Piece Vec -> Boolean)
;; move along the board until finding a piece or not
(define (piece-along? b loc pc vec)
  (match vec
    [(Vec delta-rank delta-file)
     (local
       {(: lp : Loc -> Boolean)
        (define (lp loc)
          (match (add-loc loc delta-rank delta-file)
            ['None #f]
            [(Some loc~) 
             (match (board-ref b loc~)
               ['None (lp loc~)]
               [(Some pc~) (piece=? pc pc~)])]))}
       (lp loc))]))

(: firstpiecealong : Board Loc Vec -> (Optional Loc))
;; helper function that moves
;; along board and outputs the loc of first piece along
(define (firstpiecealong b l v)
  (match v
  [(Vec delta-rank delta-file)
     (local
       {(: lp : Loc -> (Optional Loc))
        (define (lp loc)
          (match (add-loc loc delta-rank delta-file)
            ['None 'None]
            [(Some loc~)
             (match (board-ref b loc~)
               ['None (lp loc~)]
               [(Some pc~) (Some loc~)])]))} (lp l))]))
             
    
;; === move logic for the different pieces
 
(: moves-king (Board Loc -> (Listof Move)))
;; return the list of moves available to a king
;; NOTE: this doesn't account for check
(define (moves-king b loc)
  (match (board-ref b loc)
    [(Some (Piece (not 'King) _)) (error "not king")]
    ['None (error "empty square")]
    [_ (map-append (λ ([v : Vec]) (move-if-possible b loc v))
                   queen-vecs)]))

(: moves-knight (Board Loc -> (Listof Move)))
;; return the list of moves available to a knight
;; NOTE: this doesn't account for check
(define (moves-knight b loc)
  (match (board-ref b loc)
    [(Some (Piece (not 'Knight) _)) (error "not a knight")]
    ['None (error "location unoccupied")]
    [(Some (Piece 'Knight player))
     (map-append (λ ([v : Vec]) (move-if-possible b loc v))
                 knight-vecs)]))
            
(: moves-pawn (Board Loc -> (Listof Move)))
;; return the list of moves available to a pawn
;; NOTE: this doesn't account for whether this exposes check
;; TODO: en passant
(define (moves-pawn b loc)
 (match (board-ref b loc)
    [(Some (Piece (not 'Pawn) _)) (error "not a pawn")]
    ['None (error "location unoccupied")]
    [(Some (Piece 'Pawn player))
     (local
       {(define delta-rank
          (match player ['Black -1] ['White 1]))}
       (match loc
         [(Loc _ '1) (error "pawn cannot be in rank 1")]
         [(Loc _ '8) (error "pawn cannot be in rank 8")]
         [(Loc _ rank)
          (append (capture-if-occ b loc delta-rank -1)
                  (move-if-unocc b loc delta-rank 0)
                  (if (and (= rank (match player ['Black '7] ['White '2]))
                           (unocc? b (val-of (add-loc loc delta-rank 0))))
                      (move-if-unocc b loc (* 2 delta-rank) 0)
                      '())
                  (capture-if-occ b loc delta-rank 1))]))]))


(: moves-along-vectors : PieceType (Listof Vec) Board Loc -> (Listof Move))
;; generalizes queens, bishops, and rooks, which are similar
;; NOTE: this doesn't account for check
(define (moves-along-vectors type vecs b loc)
  (match (board-ref b loc)
    ['None (error "location unoccupied")]
    [(Some (Piece type~ player))
     (if (not (piece-type=? type type~))
         (error (string-append "expected piece type "
                               (symbol->string type)
                               ", got "
                               (symbol->string type~)))
         (map-append (λ ([v : Vec]) (explore b loc v)) vecs))]))

(: moves-rook (Board Loc -> (Listof Move)))
;; return the list of moves available to a rook
;; NOTE: this doesn't account for check
(define (moves-rook b loc)
  (moves-along-vectors 'Rook rook-vecs b loc))
;; moves piece should show a list of possible if promoted

(: moves-bishop (Board Loc -> (Listof Move)))
;; return the list of moves available to a bishop
;; NOTE: this doesn't account for check
(define (moves-bishop b loc)
  (moves-along-vectors 'Bishop bishop-vecs b loc))

(: moves-queen (Board Loc -> (Listof Move)))
;; return the list of moves available to a queen
;; NOTE: this doesn't account for check
(define (moves-queen b loc)
  (moves-along-vectors 'Queen queen-vecs b loc))

;; === checking for check

(: threatened-along-vectors? : PieceType (Listof Vec) Board Loc -> Boolean)
;; generalizes bishop, rook, queen
(define (threatened-along-vectors? type vecs b loc)
  (match (board-ref b loc)
    ['None (error "empty square")]
    [(Some (Piece _ p))
     (local
       {(define enemy (Piece type (opponent p)))
        (define (enemy? [vec : Vec])
          (piece-along? b loc enemy vec))}
       (ormap enemy? vecs))]))

(: bishop-threatens? : Board Loc -> Boolean)
;; does an enemy bishop threaten the piece at the location?
(define (bishop-threatens? b loc)
  (threatened-along-vectors? 'Bishop bishop-vecs b loc))

(: rook-threatens? : Board Loc -> Boolean)
;; does an enemy rook threaten the piece at the location?
(define (rook-threatens? b loc)
  (threatened-along-vectors? 'Rook rook-vecs b loc))

(: queen-threatens? : Board Loc -> Boolean)
;; does an enemy queen threaten the piece at the location?
(define (queen-threatens? b loc)
  (threatened-along-vectors? 'Queen queen-vecs b loc))

(: pawn-threatens? : Board Loc -> Boolean)
;; does an enemy pawn threaten the piece at the location?
(define (pawn-threatens? b loc)
  (match (board-ref b loc)
    ['None (error "empty square")]
    [(Some (Piece _ p))
     (local
       {(define delta-rank (match p ['Black -1] ['White 1]))
        (define opp-pawn (Piece 'Pawn (opponent p)))}
       (or (piece-at? b opp-pawn (add-loc loc delta-rank -1))
           (piece-at? b opp-pawn (add-loc loc delta-rank 1))))]))

(: knight-threatens? : Board Loc -> Boolean)
;; does an enemy knight threaten the piece at the location?
(define (knight-threatens? b loc)
  (match (board-ref b loc)
    ['None (error "empty square")]
    [(Some (Piece _ p))
     (local
       {(define k (Piece 'Knight (opponent p)))
        (define (enemy-knight? [vec : Vec])
          (match vec
            [(Vec delta-rank delta-file)
             (piece-at? b k (add-loc loc delta-rank delta-file))]))}
       (ormap enemy-knight? knight-vecs))]))

(: king-threatens? : Board Loc -> Boolean)
;; does the enemy king threaten the piece at the location?
(define (king-threatens? b loc)
  (match (board-ref b loc)
    ['None (error "empty square")]
    [(Some (Piece _ p))
     (local
       {(define k (Piece 'King (opponent p)))
        (define (enemy-king? [vec : Vec])
          (match vec
            [(Vec delta-rank delta-file)
             (piece-at? b k (add-loc loc delta-rank delta-file))]))}
       (ormap enemy-king? queen-vecs))]))

(: king-location : Board Player -> Loc)
;; return the location of the given king
(define (king-location board color)
  (local
    {(: k? : Square -> Boolean)
     (define (k? sq)
       (match sq
         ['None #f]
         [(Some piece) (piece=? piece (Piece 'King color))]))}
    (int->loc (val-of (index-of k? board)))))

(: whose-turn : ChessGame -> Player)
;; determine who moves next in the given game
;; NOTE: most recent move is first in the history
(define (whose-turn g)
  (match (ChessGame-history g)
    [(cons (Move _ _ (Piece _ 'White) _ _) _) 'Black]
    [_ 'White]))

(: in-check? : ChessGame -> Boolean)
;; Is the current player in check?
(define (in-check? game)
  (match game
    [(ChessGame board _)
     (local {(define king (king-location board (whose-turn game)))}
       (ormap (λ ([test : (Board Loc -> Boolean)]) (test board king))
              (list pawn-threatens?
                    bishop-threatens?
                    rook-threatens?
                    queen-threatens?
                    knight-threatens?
                    king-threatens?)))]))

;; === available moves and legality


(: mover : PieceType -> (Board Loc -> (Listof Move)))
;; match a piece type to a "mover" function
(define (mover type)
  (match type
    ['Pawn   moves-pawn]
    ['Knight moves-knight]
    ['Bishop moves-bishop]
    ['Rook   moves-rook]
    ['Queen  moves-queen]
    ['King   moves-king]))



(: moves-piece : ChessGame Loc -> (Listof Move))
;; return the moves a piece can make
;; note: it must be that player's turn for any moves to be available
;; TODO: castling
;; TODO: en passant
;; TODO: promotion
(define (moves-piece game loc)
  (map-append (lambda ([m : Move])
             (possible-promote m))
                   (match game
    [(ChessGame board _)
     (match (board-ref board loc)
       ['None '()]
       [(Some (Piece type p~))
        (if (not (player=? (whose-turn game) p~))
            '()
            (local 
              {(: legal? (Move -> Boolean))
               (define (legal? m)
                 (match m
                   [(Move src dst _ _ _)
                    (not (in-check?
                          (relocate-piece game src dst)))]))}
              (filter legal? (append ((mover type) board loc) (castling game loc)
                                     (passtk2 game loc))
                                     )
                                     ))])])))


(: castled? : Move -> Boolean)
;; checks if the move made was a castling move
(define (castled? mv)
  (match mv
    [(Move s d (Piece 'King _) 'None 'None)
     (= (abs (locsubtr s d)) 2)]
    [_ #f]))

     
(: locsubtr : Loc Loc -> Integer)
;; subtracts the files of two locs 
(define (locsubtr l1 l2)
  (match* (l1 l2)
    [((Loc f1 r1) (Loc f2 r2))
     (- (file->int f1) (file->int f2))]))


(: castling : ChessGame Loc -> (Listof Move))
;; Gives possible castling moves if they exist 
(define (castling cg l)
  (match cg
    [(ChessGame b h)
     (match l
       [(Loc f r)
        (match (board-ref b l)
          [(Some (Piece 'King player))
             (append
              (if (and (not (exist? (Some l) h))
                       (not (exist? (firstpiecealong b l (Vec 0 1)) h))
                       (piece-along? b l (Piece 'Rook player) (Vec 0 1)))
                  (list (Move l (Loc (val-of (add-file f 2)) r)
                        (val-of (board-ref b l)) 'None 'None)) '())
              (if (and (not (exist? (Some l) h))
                       (not (exist? (firstpiecealong b l (Vec 0 -1)) h))
                       (piece-along? b l (Piece 'Rook player) (Vec 0 -1)))
                  (list (Move l (Loc (val-of (add-file f -2)) r)
                        (val-of (board-ref b l)) 'None 'None)) '()))]
          [_ '()])])]))

        
(: exist? : (Optional Loc) (Listof Move) -> Boolean)
;; checks if a loc was in the src of any of the list of moves 
(define (exist?  l ms)
  (ormap (lambda ([m : Move])
           (match m
             [(Move s d mvd c p)
           (opt=? loc=? l (Some s))])) ms))



(: legal-move? : ChessGame Move -> Boolean)
;; a Move is legal if
;; - the named piece is actually at the source location,
;; - the named piece is capable of moving to the dest location per chess rules,
;; - the captured piece is actually at the dest location, and
;; - the move does not expose the moving player to check
(define (legal-move? g m)
  (match g
    [(ChessGame b h)
   (match m
     [(Move src dst piece cap 'None)
      (and (piece-at? (ChessGame-board g) piece (Some src))
           (ormap (λ ([m~ : Move]) (move=? m m~))
                  (moves-piece g src))
           (match cap  
             ['None (unocc? (ChessGame-board g) dst)]
             [(Some c) 
              (match (board-ref b src)
                [(Some (Piece type player))
                 (local
                   {(define delta-rank
                      (match player
                        ['Black 1]
                        ['White -1]))}
                   (if (en? g m)
                       (piece-at? (ChessGame-board g) c (add-loc dst delta-rank 0))
                     (piece-at? (ChessGame-board g) c (Some dst))))])])
           (not (in-check? (relocate-piece g src dst))))]
     [(Move src dst piece cap k)
      (match (board-ref b src)
        ['None #f]
        [(Some (Piece type player))
      (and (piece-at? (ChessGame-board g) piece (Some src))
           (need-promote? m))])])]))



(: relocate-piece : ChessGame Loc Loc -> ChessGame)
;; uncritically relocate piece without updating history
;; - this is useful for exploring hypotheticals
(define (relocate-piece g src dst)
  (match g
    [(ChessGame b hist)
     (match (board-ref b src)
       ['None (error "no piece at source location")]
       [(Some pc)
        (local
          {(define b~ (board-update (board-update b src 'None) dst (Some pc)))}
        (ChessGame b~ hist))])]))

(: need-promote? : Move -> Boolean)
;; checks if black at 1 or white at 8 being moved, and the promote
;; is shown as none
(define (need-promote? mv)
  (match mv
    [(Move s d mvd cp 'None)
     (or (and (symbol=? (Piece-color mvd) 'Black)
              (symbol=? (Piece-type mvd) 'Pawn)
              (= (Loc-rank d) 1))
         (and (symbol=? (Piece-color mvd) 'White)
              (symbol=? (Piece-type mvd) 'Pawn)
              (= (Loc-rank d) 8)))]
    [_ #f]
    ))
(provide need-promote?)

(: possible-promote : Move -> (Listof Move))
;; gives out a list of possible moves if the
;; move is to some place the pawn needs promotion
(define (possible-promote mv)
         (match mv
           [(Move s d mvd k l)
            (if (need-promote? mv)
                (list (Move s d mvd k (Some 'Queen))
                      (Move s d mvd k (Some 'Rook))
                      (Move s d mvd k (Some 'Bishop))
                      (Move s d mvd k (Some 'Knight))) (list mv))]))
            
;; given a move and a singleton list checks if they are equal
(define (mvlst mv ls)
  (match ls
    ['() #f]
    [(list mv2)
     (move=? mv2 mv)]
    [_ #f]))


(: apply-move : ChessGame Move -> ChessGame)
;; apply move if legal
(define (apply-move g m)
  (if (not (legal-move? g m))
      (error "illegal move") 
      (match g
        [(ChessGame b hist)
      (match m
        [(Move src dst pc k 'None)
         (match (board-ref b src)
           [(Some (Piece type player))
            (local {(define kk
                      (match player
                        ['Black 1]
                        ['White -1]))}
                 (if (en? g m)
             (ChessGame (board-update
              (board-update (board-update b src 'None) dst (Some pc))
              (val-of (add-loc dst kk 0)) 'None) (cons m hist)) 
                     (if (not (castled? m))
             (ChessGame (board-update (board-update b src 'None) dst (Some pc))
                                         (cons m hist))
             (ChessGame (board-update (board-update 
                               (board-update (board-update b src 'None) dst (Some pc))
                               (match (locsubtr src dst)
                                 [2 (val-of (add-loc src 0 -4))]
                                 [-2 (val-of (add-loc src 0 3))]) 'None)
                                      (match (locsubtr src dst)
                                        [2 (val-of (add-loc dst 0 1))]
                                        [-2 (val-of (add-loc dst 0 -1))])
                                      (Some (Piece 'Rook player)))                    
                        (cons m hist)))))])]
           [(Move src dst pc k pr)
            (match pc
              ['None g]
              [(Piece type player)
               (ChessGame 
                        (board-update (board-update b src 'None)
                                      dst (Some (Piece (val-of pr) player))) (cons m hist))
            ]
              )])])))

;; checked for en passant, promotion and castling
;;mostly as this is an in house solution


            
(: of-color? (Player Piece -> Boolean))
;; checks of all of the same color 
(define (of-color? color piece)
  (player=? (Piece-color piece) color))


(: moves-player : ChessGame -> (Listof Move))
;; collect all the available moves for the current player
(define (moves-player game)
  (match game
    [(ChessGame board history)
     (local
       {(define current-color (whose-turn game))
        (: moves : Loc -> (Listof Move))
        (define (moves loc)
          (match (board-ref board loc)
            [(Some piece)
             (if (of-color? current-color piece)
                 (moves-piece game loc)
                 '())]
            ['None '()]))}
       (map-append moves every-loc))]))

;; === end game conditions

(: checkmate? : ChessGame -> Boolean)
(define (checkmate? g)
  (and (in-check? g)
       (empty? (moves-player g))))

(: stalemate? (ChessGame -> Boolean))
(define (stalemate? g)
  (and (not (in-check? g))
       (empty? (moves-player g))))





;#lang typed/racket
;
;(require typed/test-engine/racket-tests)
;
;(require "../include/cs151-core.rkt")
;(require "../include/cs151-image.rkt")
;
;
;(require "optional.rkt")
;(require "loc.rkt")
;
;;; ==== ==== ==== ====
;;; external interface
;
;(provide PieceType
;         Player
;         (struct-out Piece)
;         Square
;         Board
;         (struct-out Move)
;         PromoteTo
;         (struct-out ChessGame)
;          starting-board ; : Board
;          new-game       ; : ChessGame
;          board-ref      ; : Board Loc -> Square
;          board-update   ; : Board Loc Square -> Board
;          in-check?      ; : ChessGame -> Boolean
;          legal-move?    ; : ChessGame Move -> Boolean
;          moves-piece    ; : ChessGame Loc -> (Listof Move)
;          moves-player   ; : ChessGame -> (Listof Move)
;          checkmate?     ; : ChessGame -> Boolean
;          stalemate?     ; : ChessGame -> Boolean
;          apply-move     ; : ChessGame Move -> ChessGame
;          strings->board ; : (Listof String) -> Board
;         )
;
;;; ==== ==== ==== ====
;;; data definitions
;
;(define-type PieceType
;  (U 'Pawn 'Bishop 'Knight 'Rook 'King 'Queen))
;
;(define-type Player
;  (U 'Black 'White))
;
;(define-struct Piece
;  ([type  : PieceType]
;   [color : Player]))
;
;(define-type Square
;  (Optional Piece))
;
;(define-type Board
;  (Listof Square))
;
;(define-type PromoteTo
;  (U 'Queen 'Rook 'Bishop 'Knight))
;
;(define-struct Move
;  ([src        : Loc]
;   [dst        : Loc]
;   [moved      : Piece]
;   [captured   : (Optional Piece)]
;   [promote-to : (Optional PromoteTo)]))
;
;(define-struct ChessGame
;  ([board : Board]
;   [history : (Listof Move)]))
;
;(: move->str (Move -> String))
;;; build a string version of the move, for purposes of comparison
;;; note: there is a bijection between moves and strings (and must be)
;(define (move->str m)
;  (match m
;    [(Move src dst moved captured promote-to)
;     (pipes (list "Move"
;                  (loc->str src)
;                  (loc->str dst)
;                  (piece->str moved)
;                  (get-opt (opt-map piece->str captured) "None")
;                  (get-opt (opt-map symbol->string promote-to) "None")))]))
;
;(: loc->str (Loc -> String))
;;; return string representation of location
;(define (loc->str loc)
;  (match loc
;    [(Loc f r)
;     (string-append (symbol->string f) (number->string r))]))
;
;(: piece->str (Piece -> String))
;;; return string representation of piece
;(define (piece->str p)
;  (match p
;    [(Piece t pl)
;     (string-append "Piece:"
;                    (symbol->string t)
;                    ","
;                    (symbol->string pl))]))
;
;(: pipes ((Listof String) -> String))
;;; connect strings with | character in between
;;; ex: (pipes (list "a" "bb" "ccc")) ==> "a|bb|ccc"
;(define (pipes ss)
;  (match ss
;    ['() ""]
;    [(list s) s]
;    [(cons s r) (string-append s "|" (pipes r))]))
;
;(: move<? (Move Move -> Boolean))
;;; move comparison for the purposes of sorting
;(define (move<? m1 m2)
;  (string<? (move->str m1) (move->str m2)))
;
;(: sort-moves : (Listof Move) -> (Listof Move))
;;; sort a list of moves into a canonical order
;;; allowing for comparison with check-expect
;;; note: uses the built-in sort operation
;(define (sort-moves moves)
;  (sort moves move<?))
;
;(: starting-board : Board)
;;; define the original starting board
;(define starting-board
;  (append (list (Some (Piece 'Rook 'White)) (Some (Piece 'Knight 'White))
;                (Some (Piece 'Bishop 'White)) (Some (Piece 'Queen 'White))
;                (Some (Piece 'King 'White)) (Some (Piece 'Bishop 'White))
;                (Some (Piece 'Knight 'White)) (Some (Piece 'Rook 'White)))
;          (make-list 8 (Some (Piece 'Pawn 'White)))
;          (make-list 32 'None)
;          (make-list 8 (Some (Piece 'Pawn 'Black)))
;          (list (Some (Piece 'Rook 'Black)) (Some (Piece 'Knight 'Black))
;                (Some (Piece 'Bishop 'Black)) (Some (Piece 'Queen 'Black))
;                (Some (Piece 'King 'Black)) (Some (Piece 'Bishop 'Black))
;                (Some (Piece 'Knight 'Black)) (Some (Piece 'Rook 'Black)))))
;
;(: tstbrd1 : Board)
;;; pawn check
;(define tstbrd1
; (append (list 'None 'None 'None (Some (Piece 'King 'White)))
;          (make-list 6 'None)
;          (list (Some (Piece 'Pawn 'Black)))
;          (make-list 53 'None)))
;
;(: tstbrd2 : Board)
;;; queen check
;(define tstbrd2
;  (append (list 'None 'None 'None (Some (Piece 'King 'White)))
;          (make-list 17 'None) (list (Some (Piece 'Queen 'Black)))
;          (make-list 42 'None)))
;
;(: tstbrd3 : Board)
;;; bishop check
;(define tstbrd3
;  (append (list 'None 'None 'None (Some (Piece 'King 'White)))
;          (make-list 17 'None) (list (Some (Piece 'Bishop 'Black)))
;          (make-list 42 'None)))
;
;(: tstbrd4 : Board)
;;; knight check
;(define tstbrd4
;  (append (list 'None 'None 'None (Some (Piece 'King 'White)))
;          (make-list 16 'None) (list (Some (Piece 'Knight 'Black)))
;          (make-list 43 'None)))
;
;(: tstbrd5 : Board)
;;; rook check
;(define tstbrd5
;  (append (list 'None 'None 'None (Some (Piece 'King 'White)))
;          (make-list 15 'None) (list (Some (Piece 'Rook 'Black)))
;          (make-list 44 'None)))
;
;(: stringtosquare : String -> (Listof Square))
;;; character to square
;(define (stringtosquare s)
;  (match s
;    ["" '()]
;    [k   (append
;    (match (string-ref k 0)
;  [#\P (list (Some (Piece 'Pawn 'Black)))]
;  [#\R (list (Some (Piece 'Rook 'Black)))]
;  [#\B (list (Some (Piece 'Bishop 'Black)))] 
;  [#\N (list (Some (Piece 'Knight 'Black)))]
;  [#\K (list (Some (Piece 'King 'Black)))]
;  [#\Q (list (Some (Piece 'Queen 'Black)))]
;  [#\p (list (Some (Piece 'Pawn 'White)))]
;  [#\r (list (Some (Piece 'Rook 'White)))]
;  [#\b (list (Some (Piece 'Bishop 'White)))]
;  [#\n (list (Some (Piece 'Knight 'White)))]
;  [#\k (list (Some (Piece 'King 'White)))]
;  [#\q (list (Some (Piece 'Queen 'White)))]  
;  [#\- (list 'None)]
;  [_ (error "doesn't work")])
;      (stringtosquare (substring k 1)))]
; ))
;
; (check-expect
;  (stringtosquare "PRBNKQprbnkq-")
;  (list
; (Some (Piece 'Pawn 'Black))
; (Some (Piece 'Rook 'Black))
; (Some (Piece 'Bishop 'Black))
; (Some (Piece 'Knight 'Black))
; (Some (Piece 'King 'Black))
; (Some (Piece 'Queen 'Black))
; (Some (Piece 'Pawn 'White))
; (Some (Piece 'Rook 'White))
; (Some (Piece 'Bishop 'White))
; (Some (Piece 'Knight 'White))
; (Some (Piece 'King 'White))
; (Some (Piece 'Queen 'White))
; 'None))
;
;(check-expect
; (stringtosquare "--------")
; (make-list 8 'None))
;
;( : strings->board : ((Listof String) -> Board))
;;Create a board based on the given list of strings
;;this function is useful for creating test boards.
;;; This function was put out of order because it was more useful here 
;(define (strings->board l)
;  (foldr (lambda ([s : String] [b : Board])
;         (append (stringtosquare s) b))
; '() l))  
;                  
;(check-expect
; (strings->board (list "-------K" "PNQK----"))
;(list
; 'None
; 'None
; 'None
; 'None
; 'None
; 'None
; 'None
; (Some (Piece 'King 'Black))
; (Some (Piece 'Pawn 'Black))
; (Some (Piece 'Knight 'Black))
; (Some (Piece 'Queen 'Black))
; (Some (Piece 'King 'Black))
; 'None
; 'None
; 'None
; 'None))
;
;(check-expect
; (strings->board (list "PRBNKQprbnkq-" "PNQK----"))
;(append (list
; (Some (Piece 'Pawn 'Black))
; (Some (Piece 'Rook 'Black))
; (Some (Piece 'Bishop 'Black))
; (Some (Piece 'Knight 'Black))
; (Some (Piece 'King 'Black))
; (Some (Piece 'Queen 'Black))
; (Some (Piece 'Pawn 'White))
; (Some (Piece 'Rook 'White))
; (Some (Piece 'Bishop 'White))
; (Some (Piece 'Knight 'White))
; (Some (Piece 'King 'White))
; (Some (Piece 'Queen 'White))
; 'None) (list (Some (Piece 'Pawn 'Black))
; (Some (Piece 'Knight 'Black))
; (Some (Piece 'Queen 'Black))
; (Some (Piece 'King 'Black))
; 'None
; 'None
; 'None
; 'None)))
;
;(: inchecktestboard : Board)
;;; a more complex board designed to see if in-check is working 
;(define inchecktestboard 
;(append (list 'None 'None
;      (Some (Piece 'King 'White)) (Some (Piece 'Rook 'White))
;      'None (Some (Piece 'Bishop 'White)) 'None
;      (Some (Piece 'Rook 'White)))
;      (make-list 3 (Some (Piece 'Pawn 'White))) (list 'None 'None)
;      (make-list 3 (Some (Piece 'Pawn 'White)))
;      (make-list 2 'None)
;      (list (Some (Piece 'Knight 'White))) (make-list 17 'None)
;      (list
;       (Some (Piece 'Queen 'Black))
;       'None (Some (Piece 'Bishop 'White))
;       'None 'None 'None
;      (Some (Piece 'Pawn 'Black)) 'None (Some (Piece 'Pawn 'Black))
;      'None 'None 'None)
;      (make-list 2 (Some (Piece 'Pawn 'Black))) (list 'None 'None 'None) 
;      (make-list 3 (Some (Piece 'Pawn 'Black)))
;      (list (Some (Piece 'Rook 'Black)))
;      (list (Some (Piece 'Knight 'Black))
;      (Some (Piece 'Bishop 'Black)) 
;      (Some (Piece 'King 'Black)) 'None (Some (Piece 'Bishop 'Black)) 
;      'None (Some (Piece 'Rook 'Black)))))
;
; 
;(: testcg : ChessGame)
;;; test chess game
;(define testcg 
;  (ChessGame inchecktestboard
;             (list
;              (Move (Loc 'F 4) (Loc 'G 5)
;                    (Piece 'Bishop 'White)
;                    'None 'None))))
;
;(: testcg2 : ChessGame)
;;; test chess game
;(define testcg2
;(ChessGame (strings->board (list "----b-k-"
;                  "-r-npp-p"
;                  "p--p----"
;                  "--P-----"
;                  "n---r---"
;                  "--BP----"
;                  "P---q-P-"
;                  "N-RK---Q"))
;           (list (Move (Loc 'A 2) (Loc 'A 3) (Piece 'Pawn 'White)
;                                          'None 'None))
;           ))
;(: testcg3 : ChessGame)
;;; test chess game
;(define testcg3
;(ChessGame (strings->board (list 
;                  "-------k"
;                  "--------"
;                  "------Q-"
;                  "p-------"
;                  "P-------"
;                  "--------"
;                  "K-------"
;                  "--------"
;                  ))
;           (list (Move (Loc 'A 6) (Loc 'A 5) (Piece 'Pawn 'Black)
;                                          'None 'None))
;           ))
;
;(: testcg4 : ChessGame)
;;; test chess game
;(define testcg4
;(ChessGame
; (strings->board (list
;                  "--------"
;                  "--------"
;                  "--------"
;                  "--------"
;                  "--------"
;                  "------q-"
;                  "-----k--"
;                  "-------K"))
; (list (Move (Loc 'G 5) (Loc 'G 6) (Piece 'Queen 'White)
;                                         'None 'None))))
;
;(: testcg5 : ChessGame)
;;; test chess game
;(define testcg5
;(ChessGame
; (strings->board (list
;                  "k------r"
;                  "--------"
;                  "--------"
;                  "--------"
;                  "-----q-K"
;                  "--------"
;                  "--------"
;                  "--------"))
; (list (Move (Loc 'G 1) (Loc 'H 1) (Piece 'Rook 'White)
;                                         'None 'None))))
;
;(: testcg6 : ChessGame)
;(define testcg6
;  (ChessGame
;   (strings->board
;    (list "--------"
;          "---K--N-"
;          "--------"
;          "--------"
;          "-r-Q----"
;          "--------"
;          "--------"
;          "b------k"))
;   (list (Move (Loc 'D 4) (Loc 'D 5) (Piece 'Queen 'Black) 'None 'None))))
;  
;(: testcg7 : ChessGame)
;(define testcg7
;  (ChessGame
;   (strings->board
;    (list "--------"
;          "---K--N-"
;          "--------"
;          "--------"
;          "-r-Q----"
;          "--------"
;          "--------"
;          "b-------"))
;   (list
;    (Move (Loc 'B 4) (Loc 'B 5) (Piece 'Rook 'White) 'None 'None))))
;
;                  
;(: new-game : ChessGame)
;;;This value contains a starting board and empty move history.
;(define new-game
;  (ChessGame
;   starting-board 
;   '()))
;
;(: refofloconboard : Board Loc -> Integer)
;;; helper function gives ref of loc on board
;(define (refofloconboard b l)
;  (- (match l
;  [(Loc f r) ( +
;              (match f
;                ['A 1]
;                ['B 2]
;                ['C 3]
;                ['D 4]
;                ['E 5]
;                ['F 6]
;                ['G 7]
;                ['H 8])
;              (* (- r 1) 8))]) 1))
;
;(check-expect (refofloconboard starting-board (Loc 'A 1))
;              0)
;(check-expect (refofloconboard starting-board (Loc 'C 8))
;              58)
;(check-expect (refofloconboard starting-board (Loc 'C 5))
;              34)
;(check-expect (refofloconboard starting-board (Loc 'D 8))
;              59)
;(check-expect (refofloconboard starting-board (Loc 'F 2))
;              13)
;
;(: board-ref : Board Loc -> Square)
;;; Given a board and a location on it, this function returns the
;;; contents of the specified square in the board.
;(define (board-ref b l)
;  (list-ref b
;    (refofloconboard b l)))
;
;(check-expect (board-ref starting-board (Loc 'A 1))
;              (Some (Piece 'Rook 'White)))
;(check-expect (board-ref starting-board (Loc 'C 8))
;              (Some (Piece 'Bishop 'Black)))
;(check-expect (board-ref starting-board (Loc 'C 5))
;              'None)
;(check-expect (board-ref starting-board (Loc 'D 8))
;              (Some (Piece 'Queen 'Black)))
;(check-expect (board-ref starting-board (Loc 'F 2))
;              (Some (Piece 'Pawn 'White)))
;
(: color=? : Square Square -> Boolean)
;; helper function that checks for color of piece equality
(define (color=? s1 s2)
  (match* (s1 s2)
    [((Some (Piece t1 c1)) (Some (Piece t2 c2))) 
      (symbol=? c1 c2)]
    [(_ _) #f]))
;
;(check-expect (color=? (Some (Piece 'Rook 'Black))
;                       (Some (Piece 'Queen 'Black))) #t)
;(check-expect (color=? (Some (Piece 'Rook 'Black))
;                       (Some (Piece 'Rook 'Black))) #t)
;(check-expect (color=? (Some (Piece 'Rook 'Black))
;                       (Some (Piece 'Queen 'White))) #f)
;(check-expect (color=? (Some (Piece 'Rook 'White))
;                       (Some (Piece 'Queen 'White))) #t)
;
(: type=? : Square Square -> Boolean)
;; helper function that checks for type of piece equality 
(define (type=? s1 s2)
  (match* (s1 s2)
    [((Some (Piece t1 c1)) (Some (Piece t2 c2))) 
     (symbol=? t1 t2)]
    [(_ _) #f]))

;(check-expect (type=? (Some (Piece 'Rook 'Black))
;                      (Some (Piece 'Rook 'Black))) #t)
;(check-expect (type=? (Some (Piece 'Rook 'White))
;                      (Some (Piece 'Rook 'Black))) #t)
;(check-expect (type=? (Some (Piece 'Rook 'Black))
;                      (Some (Piece 'Queen 'Black))) #f)
;(check-expect (type=? (Some (Piece 'Rook 'Black))
;                      (Some (Piece 'Rook 'White))) #t)
;
;(: square=? : Square Square -> Boolean)
;;; helper function that checks for squares equality 
;(define (square=? s1 s2)
;  (and (color=? s1 s2) (type=? s1 s2)))
;
;(check-expect (square=? (Some (Piece 'Rook 'Black))
;                        (Some (Piece 'Rook 'Black))) #t)
;(check-expect (square=? (Some (Piece 'Rook 'White))
;                        (Some (Piece 'Rook 'Black))) #f)
;(check-expect (square=? (Some (Piece 'Rook 'Black))
;                        (Some (Piece 'Queen 'Black))) #f)
;(check-expect (square=? (Some (Piece 'Rook 'Black))
;                        (Some (Piece 'Rook 'White))) #f)
;
;(: piece=? : Piece Piece -> Boolean)
;;; helper function that checks for piece equality 
;(define (piece=? p1 p2)
;  (match* (p1 p2)
;    [((Piece t1 c1) (Piece t2 c2))
;     (and (symbol=? t1 t2) (symbol=? c1 c2))]
;    [(_ _) #f]))
;
;(check-expect (piece=?  (Piece 'Rook 'Black)
;                        (Piece 'Rook 'Black)) #t)
;(check-expect (piece=?  (Piece 'Queen 'Black)
;                        (Piece 'Rook 'Black)) #f)
;
;(: board-update : Board Loc Square -> Board)
;;; uses the loc and the square to update the board
;(define (board-update b l s)
;(append 
;  (take b (refofloconboard b l))
;          (list s)
;          (drop b (+ (refofloconboard b l) 1)))) 
;
;(check-expect (board-update starting-board (Loc 'A 1)
;              (Some (Piece 'King 'White)))
;              (list
; (Some (Piece 'King 'White))
; (Some (Piece 'Knight 'White))
; (Some (Piece 'Bishop 'White))
; (Some (Piece 'Queen 'White))
; (Some (Piece 'King 'White))
; (Some (Piece 'Bishop 'White))
; (Some (Piece 'Knight 'White))
; (Some (Piece 'Rook 'White))
; (Some (Piece 'Pawn 'White))
; (Some (Piece 'Pawn 'White))
; (Some (Piece 'Pawn 'White))
; (Some (Piece 'Pawn 'White))
; (Some (Piece 'Pawn 'White))
; (Some (Piece 'Pawn 'White))
; (Some (Piece 'Pawn 'White))
; (Some (Piece 'Pawn 'White))
; 'None
; 'None
; 'None
; 'None
; 'None
; 'None
; 'None
; 'None
; 'None
; 'None
; 'None
; 'None
; 'None
; 'None
; 'None
; 'None
; 'None
; 'None
; 'None
; 'None
; 'None
; 'None
; 'None
; 'None
; 'None
; 'None
; 'None
; 'None
; 'None
; 'None
; 'None
; 'None
; (Some (Piece 'Pawn 'Black))
; (Some (Piece 'Pawn 'Black))
; (Some (Piece 'Pawn 'Black))
; (Some (Piece 'Pawn 'Black))
; (Some (Piece 'Pawn 'Black))
; (Some (Piece 'Pawn 'Black))
; (Some (Piece 'Pawn 'Black))
; (Some (Piece 'Pawn 'Black))
; (Some (Piece 'Rook 'Black))
; (Some (Piece 'Knight 'Black))
; (Some (Piece 'Bishop 'Black))
; (Some (Piece 'Queen 'Black))
; (Some (Piece 'King 'Black))
; (Some (Piece 'Bishop 'Black))
; (Some (Piece 'Knight 'Black))
; (Some (Piece 'Rook 'Black))))
;
;(check-expect (board-update starting-board (Loc 'A 2)
;              (Some (Piece 'King 'White)))
;              (list
; (Some (Piece 'Rook 'White))
; (Some (Piece 'Knight 'White))
; (Some (Piece 'Bishop 'White))
; (Some (Piece 'Queen 'White))
; (Some (Piece 'King 'White))
; (Some (Piece 'Bishop 'White))
; (Some (Piece 'Knight 'White))
; (Some (Piece 'Rook 'White))
; (Some (Piece 'King 'White))
; (Some (Piece 'Pawn 'White))
; (Some (Piece 'Pawn 'White))
; (Some (Piece 'Pawn 'White))
; (Some (Piece 'Pawn 'White))
; (Some (Piece 'Pawn 'White))
; (Some (Piece 'Pawn 'White))
; (Some (Piece 'Pawn 'White))
; 'None
; 'None
; 'None
; 'None
; 'None
; 'None
; 'None
; 'None
; 'None
; 'None
; 'None
; 'None
; 'None
; 'None
; 'None
; 'None
; 'None
; 'None
; 'None
; 'None
; 'None
; 'None
; 'None
; 'None
; 'None
; 'None
; 'None
; 'None
; 'None
; 'None
; 'None
; 'None
; (Some (Piece 'Pawn 'Black))
; (Some (Piece 'Pawn 'Black))
; (Some (Piece 'Pawn 'Black))
; (Some (Piece 'Pawn 'Black))
; (Some (Piece 'Pawn 'Black))
; (Some (Piece 'Pawn 'Black))
; (Some (Piece 'Pawn 'Black))
; (Some (Piece 'Pawn 'Black))
; (Some (Piece 'Rook 'Black))
; (Some (Piece 'Knight 'Black))
; (Some (Piece 'Bishop 'Black))
; (Some (Piece 'Queen 'Black))
; (Some (Piece 'King 'Black))
; (Some (Piece 'Bishop 'Black))
; (Some (Piece 'Knight 'Black))
; (Some (Piece 'Rook 'Black))))
;
;(check-expect (board-update starting-board (Loc 'A 2)
;              (Some (Piece 'King 'Black)))
;              (list
; (Some (Piece 'Rook 'White))
; (Some (Piece 'Knight 'White))
; (Some (Piece 'Bishop 'White))
; (Some (Piece 'Queen 'White))
; (Some (Piece 'King 'White))
; (Some (Piece 'Bishop 'White))
; (Some (Piece 'Knight 'White))
; (Some (Piece 'Rook 'White))
; (Some (Piece 'King 'Black))
; (Some (Piece 'Pawn 'White))
; (Some (Piece 'Pawn 'White))
; (Some (Piece 'Pawn 'White))
; (Some (Piece 'Pawn 'White))
; (Some (Piece 'Pawn 'White))
; (Some (Piece 'Pawn 'White))
; (Some (Piece 'Pawn 'White))
; 'None
; 'None
; 'None
; 'None
; 'None
; 'None
; 'None
; 'None
; 'None
; 'None
; 'None
; 'None
; 'None
; 'None
; 'None
; 'None
; 'None
; 'None
; 'None
; 'None
; 'None
; 'None
; 'None
; 'None
; 'None
; 'None
; 'None
; 'None
; 'None
; 'None
; 'None
; 'None
; (Some (Piece 'Pawn 'Black))
; (Some (Piece 'Pawn 'Black))
; (Some (Piece 'Pawn 'Black))
; (Some (Piece 'Pawn 'Black))
; (Some (Piece 'Pawn 'Black))
; (Some (Piece 'Pawn 'Black))
; (Some (Piece 'Pawn 'Black))
; (Some (Piece 'Pawn 'Black))
; (Some (Piece 'Rook 'Black))
; (Some (Piece 'Knight 'Black))
; (Some (Piece 'Bishop 'Black))
; (Some (Piece 'Queen 'Black))
; (Some (Piece 'King 'Black))
; (Some (Piece 'Bishop 'Black))
; (Some (Piece 'Knight 'Black))
; (Some (Piece 'Rook 'Black))))
;              
;  
;(: file+ : Loc -> Loc)
;;; helper function that increases file by one
;(define (file+ l)
;(match l
;    [(Loc f r)
;     (Loc
;      (match f 
;    ['A 'B]
;    ['B 'C]
;    ['C 'D]
;    ['D 'E]
;    ['E 'F]
;    ['F 'G]
;    ['G 'H]
;    [_ (error "file+ : not in range")]) r)]))
;
;(check-expect (file+ (Loc 'F 2)) (Loc 'G 2))
;(check-expect (file+ (Loc 'A 2)) (Loc 'B 2))
;(check-error (file+ (Loc 'H 2)) "file+ : not in range")
;(check-expect (file+ (Loc 'D 8)) (Loc 'E 8))
;
;(: file- : Loc -> Loc)
;;; helper function that decreases file by one
;(define (file- l)
;  (match l
;    [(Loc f r)
;     (Loc
;      (match f 
;        ['B 'A]
;        ['C 'B]
;        ['D 'C]
;        ['E 'D]
;        ['F 'E]
;        ['G 'F]
;        ['H 'G]
;        [_ (error "file- : not in range")])
;      r)]))
;
;(check-expect (file- (Loc 'F 2)) (Loc 'E 2))
;(check-expect (file- (Loc 'B 2)) (Loc 'A 2))
;(check-error (file- (Loc 'A 2)) "file- : not in range")
;
;(: rank+ : Loc -> Loc)
;;; helper function that increases rank by one
;(define (rank+ l)
;  (match l
;    [(Loc f r)
;     (Loc f
;      (match r
;        [1 2]
;        [2 3]
;        [3 4]
;        [4 5]
;        [5 6]
;        [6 7]
;        [7 8]
;        [_ (error "rank+ : not in range")]))]))
;
;(check-expect (rank+ (Loc 'F 2)) (Loc 'F 3))
;(check-expect (rank+ (Loc 'B 2)) (Loc 'B 3))
;(check-error (rank+ (Loc 'B 8)) "rank+ : not in range")
;
;(: rank- : Loc -> Loc)
;;; helper function that decreases rank by one
;(define (rank- l)
;  (match l
; [(Loc f r)
;     (Loc f
;      (match r
;        [1 (error "rank- : not in range")]
;        [2 1]
;        [3 2]
;        [4 3]
;        [5 4]
;        [6 5]
;        [7 6]
;        [8 7]
;        [_ (error "rank+ : not in range")]))]))
;
;(check-expect (rank- (Loc 'F 2)) (Loc 'F 1))
;(check-expect (rank- (Loc 'B 5)) (Loc 'B 4))
;(check-error (rank- (Loc 'B 1)) "rank- : not in range")
;
;(: vertically+ : Board Loc -> (Listof Loc))
;;; helper outputs all vertical locs possible to move to 
;;; stops after first piece encountered. Ends with first piece in list
;;; if same color doesn't add to list of moves possible 
;(define (vertically+ b l)
;  (match (board-ref b l)
;    [(Some (Piece _ c1))
;     (local
;       {(: otherst : Board Loc -> (Listof Loc))
;        (define (otherst b l)
;          (match l
;            [(Loc f r)
;             (if (= r 8) '()
;                 (match (board-ref b (rank+ l))
;                   ['None
;                    (append (list (rank+ l))
;                            (otherst b (rank+ l)))]
;                   [(Some (Piece _ c2))
;                    (if (symbol=? c1 c2) '()
;                        (list (rank+ l)))])
;                 )]))} (otherst b l))]
;    [_ '()]))
;
;(check-expect (vertically+ tstbrd2 (Loc 'F 3))
;              (list (Loc 'F 4) (Loc 'F 5) (Loc 'F 6)
;                    (Loc 'F 7) (Loc 'F 8)))
;
;(check-expect (vertically+ tstbrd5 (Loc 'D 1))
;              (list (Loc 'D 2) (Loc 'D 3)))
;
;(check-expect (vertically+ starting-board (Loc 'A 2))
;      (list (Loc 'A 3) (Loc 'A 4) (Loc 'A 5) (Loc 'A 6) (Loc 'A 7)))
;
;(check-expect (vertically+ starting-board (Loc 'A 2))
;      (list (Loc 'A 3) (Loc 'A 4) (Loc 'A 5) (Loc 'A 6) (Loc 'A 7)))
;
;(check-expect (vertically+ starting-board (Loc 'B 2))
;(list (Loc 'B 3) (Loc 'B 4) (Loc 'B 5) (Loc 'B 6) (Loc 'B 7)))
;
;         
;(: vertically- : Board Loc -> (Listof Loc))
;;; helper outputs all vertical down moves possible
;;; stops after first piece encountered. Ends with first piece in list
;;; if same color doesn't add to list of moves possible 
;(define (vertically- b l)
;  (match (board-ref b l)
;    [(Some (Piece _ c1))
;     (local {(: otherst : Board Loc -> (Listof Loc))
;             (define (otherst b l)
;               (match l
;                 [(Loc f r)
;                  (if (= r 1) '()
;                      (match (board-ref b (rank- l))
;                        ['None
;                         (append (list (rank- l))
;                                 (otherst b (rank- l)))
;                         ]
;                        [(Some (Piece _ c2))
;                         (if (symbol=? c1 c2) '()
;                             (list (rank- l)))])
;                      )]))} (otherst b l))]
;    [_ '()]))
;
;(check-expect (vertically- tstbrd2 (Loc 'F 3))
;              (list (Loc 'F 2) (Loc 'F 1)))
;
;(check-expect (vertically- tstbrd5 (Loc 'D 1))
;'())
;(check-expect (vertically- tstbrd5 (Loc 'D 3))
;(list (Loc 'D 2) (Loc 'D 1)))
;
;(check-expect (vertically- starting-board (Loc 'A 2))
;      '())        
;
;(: horizontally+ : Board Loc -> (Listof Loc))
;;; all horizontal moves up possible
;;; stops when blocked
;;; if same color doesn't add to list of moves possible 
;(define (horizontally+ b l)
;  (match (board-ref b l)
;    [(Some (Piece _ c1))
;     (local {(: otherst : Board Loc -> (Listof Loc))
;             (define (otherst b l)
;               (match l
;                 [(Loc f r)
;                  (if (symbol=? f 'H) '()
;                      (match (board-ref b (file+ l))
;                        ['None
;                         (append (list (file+ l))
;                                 (otherst b (file+ l)))
;                         ]
;                        [(Some (Piece _ c2))
;                         (if (symbol=? c1 c2) '()
;                             (list (file+ l)))])
;                      )]))} (otherst b l))]
;    [_ '()]))
;
;(check-expect (horizontally+ starting-board (Loc 'E 7)) '())
;
;(check-expect (horizontally+ starting-board (Loc 'A 2)) '())
;
;(check-expect (horizontally+ tstbrd1 (Loc 'C 2))
;(list (Loc 'D 2) (Loc 'E 2) (Loc 'F 2) (Loc 'G 2) (Loc 'H 2)))
;
;(check-expect (horizontally+ tstbrd2 (Loc 'F 3))
;              (list (Loc 'G 3) (Loc 'H 3)))
;
;
;
;(: horizontally- : Board Loc -> (Listof Loc))
;;; all horizontal moves down possible
;;; stops when blocked 
;(define (horizontally- b l)
;  (match (board-ref b l)
;    [(Some (Piece _ c1))
;     (local {(: otherst : Board Loc -> (Listof Loc))
;             (define (otherst b l)
;               (match l
;                 [(Loc f r)
;                  (if (symbol=? f 'A) '()
;                      (match (board-ref b (file- l))
;                        ['None
;                         (append (list (file- l))
;                                 (otherst b (file- l)))
;                         ]
;                        [(Some (Piece _ c2))
;                         (if (symbol=? c1 c2) '()
;                             (list (file- l)))])
;                      )]))} (otherst b l))]
;    [_ '()]))
;
;(check-expect (horizontally- starting-board (Loc 'A 2)) '())
;
;(check-expect (horizontally- tstbrd2 (Loc 'F 3))
;              (list (Loc 'E 3) (Loc 'D 3) (Loc 'C 3)
;                    (Loc 'B 3) (Loc 'A 3)))
;
;(check-expect (horizontally- tstbrd1 (Loc 'C 2))
;              (list (Loc 'B 2) (Loc 'A 2)))
;
;
;
;(: diagonallyr+ : Board Loc -> (Listof Loc))
;;; all diagonal moves right up moves possible
;(define (diagonallyr+ b l)
;  (match (board-ref b l)
;    [(Some (Piece _ c1))
;     (local {(: otherst : Board Loc -> (Listof Loc))
;             (define (otherst b l)
;               (match l
;                 [(Loc f r)
;                  (if (or (symbol=? f 'H) (= r 8)) '()
;                      (match (board-ref b (rank+ (file+ l)))
;                        ['None
;                         (append (list (rank+ (file+ l)))
;                                 (otherst b (rank+ (file+ l))))
;                         ]
;                        [(Some (Piece _ c2))
;                         (if (symbol=? c1 c2) '()
;                             (list (rank+ (file+ l))))]
;                        ))]))} (otherst b l))]
;    [_ '()]))
;
;
;(check-expect (diagonallyr+ tstbrd2 (Loc 'F 3))
;              (list (Loc 'G 4) (Loc 'H 5)))
;
;(check-expect (diagonallyr+ tstbrd1 (Loc 'D 1))
;              (list (Loc 'E 2) (Loc 'F 3) (Loc 'G 4) (Loc 'H 5)))
;
;(check-expect (diagonallyr+ starting-board (Loc 'A 2))
;              (list (Loc 'B 3) (Loc 'C 4) (Loc 'D 5)
;                    (Loc 'E 6) (Loc 'F 7)))
;
;(check-expect (diagonallyr+ starting-board (Loc 'A 1)) '())
;
;(: diagonallyl+ : Board Loc -> (Listof Loc))
;;; all diagonal moves left up moves possible
;(define (diagonallyl+ b l)
;  (match (board-ref b l)
;    [(Some (Piece _ c1))
;     (local {(: otherst : Board Loc -> (Listof Loc))
;             (define (otherst b l)
;               (match l
;                 [(Loc f r)
;                  (if (or (symbol=? f 'A) (= r 8)) '()
;                      (match (board-ref b (rank+ (file- l)))
;                        ['None
;                         (append (list (rank+ (file- l)))
;                                 (otherst b
;                                          (rank+ (file- l))))
;                         ]
;                        [(Some (Piece _ c2))
;                         (if (symbol=? c1 c2) '()
;                             (list (rank+ (file- l))))]
;                        ))]))} (otherst b l))]
;    [_ '()]))
;  
;
;(check-expect (diagonallyl+ tstbrd2 (Loc 'F 3))
;              (list (Loc 'E 4) (Loc 'D 5)
;                    (Loc 'C 6) (Loc 'B 7) (Loc 'A 8)))
;
;(check-expect (diagonallyl+ tstbrd1 (Loc 'D 1))
;              (list (Loc 'C 2)))
;
;(check-expect (diagonallyl+ starting-board (Loc 'G 2))
;(list (Loc 'F 3) (Loc 'E 4) (Loc 'D 5) (Loc 'C 6) (Loc 'B 7)))
;
;(check-expect (diagonallyl+ starting-board (Loc 'A 1)) '())
;
;(: diagonallyr- : Board Loc -> (Listof Loc))
;;; all diagonal moves right down moves possible
;(define (diagonallyr- b l)
;  (match (board-ref b l)
;    [(Some (Piece _ c1))
;     (local {(: otherst : Board Loc -> (Listof Loc))
;             (define (otherst b l)
;               (match l
;                 [(Loc f r)
;                  (if (or (symbol=? f 'H) (= r 1)) '()
;                      (match
;                          (board-ref b (rank- (file+ l)))
;                        ['None
;                         (append (list (rank- (file+ l)))
;                                 (otherst b (rank- (file+ l))))
;                         ]
;                        [(Some (Piece _ c2))
;                         (if (symbol=? c1 c2) '()
;                             (list (rank- (file+ l))))]
;                        ))]))} (otherst b l))]
;    [_ '()]))
;
;
;(check-expect (diagonallyr- starting-board (Loc 'B 7))
;              (list (Loc 'C 6) (Loc 'D 5) (Loc 'E 4) (Loc 'F 3) (Loc 'G 2)))
;
;(check-expect (diagonallyr- tstbrd1 (Loc 'C 2))
;              (list (Loc 'D 1)))
;
;(check-expect (diagonallyr- starting-board (Loc 'G 7))
;(list (Loc 'H 6)))
;
;(check-expect (diagonallyr- starting-board (Loc 'A 1)) '())
;
;(: diagonallyl- : Board Loc -> (Listof Loc))
;;; all diagonal moves left down moves possible
;(define (diagonallyl- b l)
;  (match (board-ref b l)
;    [(Some (Piece _ c1))
;     (local
;       {(: otherst : Board Loc -> (Listof Loc))
;        (define (otherst b l)
;          (match l
;            [(Loc f r)
;             (if (or (symbol=? f 'A) (= r 1)) '()
;                 (match (board-ref b (rank- (file- l)))
;                   ['None
;                    (append (list (rank- (file- l)))
;                            (otherst b (rank- (file- l))))
;                    ]
;                   [(Some (Piece _ c2))
;                    (if (symbol=? c1 c2) '()
;                        (list (rank- (file- l))))]
;                   ))]))} (otherst b l))]
;    [_ '()]))
;
;(check-expect (diagonallyl- starting-board (Loc 'E 7))
;(list (Loc 'D 6) (Loc 'C 5) (Loc 'B 4) (Loc 'A 3)))
;
;(check-expect
; (diagonallyl- starting-board (Loc 'G 7))
;(list (Loc 'F 6) (Loc 'E 5) (Loc 'D 4) (Loc 'C 3) (Loc 'B 2)))
;
;(check-expect
; (diagonallyl- starting-board (Loc 'C 7))
;(list (Loc 'B 6) (Loc 'A 5)))
;
;(check-expect (diagonallyl- starting-board (Loc 'A 1)) '())
;
;(: queen-possible : Board Loc -> (Listof Loc))
;;; gives all the possible possible moves
;;; if the given piece is a queen
;(define (queen-possible b l)
;  (append (vertically+ b l)
;          (vertically- b l)
;          (horizontally+ b l)
;          (horizontally- b l)
;          (diagonallyr+ b l)
;          (diagonallyr- b l)
;          (diagonallyl+ b l)
;          (diagonallyl- b l)))
;;; note: there is no checking if the piece is particularly a
;;;queen 
;;;because I utilize the function in my in-check
;(check-expect
; (queen-possible starting-board (Loc 'D 2))
; (list
; (Loc 'D 3)
; (Loc 'D 4)
; (Loc 'D 5)
; (Loc 'D 6)
; (Loc 'D 7)
; (Loc 'E 3)
; (Loc 'F 4)
; (Loc 'G 5)
; (Loc 'H 6)
; (Loc 'C 3)
; (Loc 'B 4)
; (Loc 'A 5)))
;
;(check-expect
; (queen-possible tstbrd2 (Loc 'F 3))
; (list
; (Loc 'F 4)
; (Loc 'F 5)
; (Loc 'F 6)
; (Loc 'F 7)
; (Loc 'F 8)
; (Loc 'F 2)
; (Loc 'F 1)
; (Loc 'G 3)
; (Loc 'H 3)
; (Loc 'E 3)
; (Loc 'D 3)
; (Loc 'C 3)
; (Loc 'B 3)
; (Loc 'A 3)
; (Loc 'G 4)
; (Loc 'H 5)
; (Loc 'G 2)
; (Loc 'H 1)
; (Loc 'E 4)
; (Loc 'D 5)
; (Loc 'C 6)
; (Loc 'B 7)
; (Loc 'A 8)
; (Loc 'E 2)
; (Loc 'D 1)))
;
;(: bishop-possible : Board Loc -> (Listof Loc))
;;; gives all the possible possible moves
;;; if the given piece is a bishop
;(define (bishop-possible b l)
;  (append (diagonallyr+ b l)
;          (diagonallyr- b l)
;          (diagonallyl+ b l)
;          (diagonallyl- b l)))
;
;(check-expect
; (bishop-possible starting-board (Loc 'D 2))
; (list (Loc 'E 3) (Loc 'F 4) (Loc 'G 5)
;       (Loc 'H 6) (Loc 'C 3) (Loc 'B 4) (Loc 'A 5)))
;
;(check-expect
; (bishop-possible tstbrd3 (Loc 'F 3))
; (list
; (Loc 'G 4)
; (Loc 'H 5)
; (Loc 'G 2)
; (Loc 'H 1)
; (Loc 'E 4)
; (Loc 'D 5)
; (Loc 'C 6)
; (Loc 'B 7)
; (Loc 'A 8)
; (Loc 'E 2)
; (Loc 'D 1)))
;
;(: rook-possible : Board Loc -> (Listof Loc))
;;; gives all the possible possible moves
;;; if the given piece is a rook
;(define (rook-possible b l)
;  (append (vertically+ b l)
;          (vertically- b l)
;          (horizontally+ b l)
;          (horizontally- b l)))
;
;(check-expect
; (rook-possible starting-board (Loc 'D 2))
; (list (Loc 'D 3) (Loc 'D 4) (Loc 'D 5) (Loc 'D 6) (Loc 'D 7)))
;
;(check-expect
; (rook-possible tstbrd5 (Loc 'D 3))
; (list
; (Loc 'D 4)
; (Loc 'D 5)
; (Loc 'D 6)
; (Loc 'D 7)
; (Loc 'D 8)
; (Loc 'D 2)
; (Loc 'D 1)
; (Loc 'E 3)
; (Loc 'F 3)
; (Loc 'G 3)
; (Loc 'H 3)
; (Loc 'C 3)
; (Loc 'B 3)
; (Loc 'A 3)))
;
;(: knight-possible : Board Loc -> (Listof Loc))
;;; gives all the possible moves
;;; if the given piece is a knight
;(define (knight-possible b l)
;  (match (board-ref b l)
;    [(Some (Piece t1 c1))
;  (filter
;   (lambda ([l : Loc])
;            (match (board-ref b l)
;              [(Some (Piece t2 c2))
;               (not (symbol=? c1 c2))]
;              [_ #t]))
;              (match l
;    [(Loc f r)
;  (append
;   (if (or (= r 8) (symbol=? f 'G) (symbol=? f 'H))
;           '() (list (rank+ (file+ (file+ l)))))
;   
;   (if (or (= r 1) (symbol=? f 'G) (symbol=? f 'H))
;       '() (list (rank- (file+ (file+ l)))))
;   
;  (if (or (= r 7) (= r 8) (symbol=? f 'A)) '()
;   (list (file- (rank+ (rank+ l)))))
;  
;   (if (or (= r 7) (= r 8) (symbol=? f 'H)) '()
;       (list (file+ (rank+ (rank+ l)))))
;   
;   (if (or (= r 2) (= r 1) (symbol=? f 'H)) '()
;   (list (file+ (rank- (rank- l)))))
;   
;   (if (or (= r 2) (= r 1) (symbol=? f 'A)) '()
;       (list (file- (rank- (rank- l)))))
;   
;   (if (or (= r 8) (symbol=? f 'B) (symbol=? f 'A)) '()
;       (list (rank+ (file- (file- l)))))
;   
;   (if (or (= r 1) (symbol=? f 'B) (symbol=? f 'A)) '()
;       (list (rank- (file- (file- l)))))
;   
;   )]))]
;    [_ '()]))
;
;(check-expect (knight-possible starting-board (Loc 'D 2))
;(list (Loc 'F 3) (Loc 'C 4) (Loc 'E 4) (Loc 'B 3)))
;
;(check-expect (knight-possible tstbrd4 (Loc 'E 3))
;(list
; (Loc 'G 4)
; (Loc 'G 2)
; (Loc 'D 5)
; (Loc 'F 5)
; (Loc 'F 1)
; (Loc 'D 1)
; (Loc 'C 4)
; (Loc 'C 2)))
;
;(: pawn-possible : Board Loc -> (Listof Loc))
;;; gives all the possible moves
;;; if the given piece is a pawn
;;; if it is black the movement is reversed.
;;; can attack only diagonally
;(define (pawn-possible b l)
;  (match (board-ref b l) 
;     ['None '()]
;     [(Some (Piece _ 'White))
;      (match l
;        [(Loc f r)
;         (if (= r 8) '()
;             (append
;             (match (board-ref b (rank+ l))
;               ['None (list (rank+ l))]
;               [_ '()])
;             (match (diagonallyr+ b l)
;               ['() '()]
;               [ _
;                 (match (board-ref b (first (diagonallyr+ b l)))
;                   ['None '()]
;                   [_ (list (first (diagonallyr+ b l)))])])
;             (match (diagonallyl+ b l)
;               ['() '()]
;               [ _
;                 (match (board-ref b (first (diagonallyl+ b l)))
;                   ['None '()]
;                   [_ (list (first (diagonallyl+ b l)))])])
;             (if (= r 2)
;                 (match (board-ref b (rank+ (rank+ l)))
;                   ['None (list (rank+ (rank+ l)))]
;                   [_ '()])
;                  '())))])]
;    [ _
;
;       (match l
;         [(Loc f r)
;          (if (= r 1) '() 
;              (append (match (board-ref b (rank- l))
;                ['None (list (rank- l))]
;                [_ '()])
;              (match (diagonallyr- b l)
;                ['() '()]
;                [ _
;                  (match (board-ref b (first (diagonallyr- b l)))
;                    ['None '()]
;                    [_ (list (first (diagonallyr- b l)))])])
;              (match (diagonallyl- b l)
;                ['() '()]
;                [ _
;                  (match (board-ref b (first (diagonallyl- b l)))
;                    ['None '()]
;                    [_ (list (first (diagonallyl- b l)))])])
;              (if (= r 7)
;                  (match (board-ref b (rank- (rank- l)))
;                    ['None (list (rank- (rank- l)))]
;                    [_ '()]) '())))])]))
;   
;
;(check-expect (pawn-possible starting-board (Loc 'B 2))
;              (list (Loc 'B 3) (Loc 'B 4)))
;(check-expect
; (pawn-possible
;  (board-update starting-board (Loc 'B 3)
;                (Some (Piece 'Pawn 'White))) (Loc 'B 3))
; (list (Loc 'B 4)))
;
;(check-expect (pawn-possible
;  (board-update starting-board (Loc 'B 6)
;                (Some (Piece 'Pawn 'Black))) (Loc 'B 6))
;              (list (Loc 'B 5)))
;
;(check-expect (pawn-possible starting-board (Loc 'B 7))
;              (list (Loc 'B 6) (Loc 'B 5)))
;
;(: kingsquare-to-loc : Board Square -> Loc)
;;; helper function that finds the loc of the king on the board
;;; given the particular king with the color 
;(define (kingsquare-to-loc b s)
;  (match s
;    [(Some (Piece 'King _))
;  (local
;    {(: index-square-on-board : Board Square -> Integer)
;     ;; gives the index of the square on the board, only works for unique  
;     (define (index-square-on-board b s)
;       (match b
;         [(cons f r) 
;          (if (square=? f s) 0 (+ 1 (index-square-on-board r s)))]))}
;    (Loc (match (- (+ (index-square-on-board b s) 1)
;                   (* (quotient (index-square-on-board b s) 8) 8))
;      [1 'A]
;      [2 'B]
;      [3 'C]
;      [4 'D]
;      [5 'E]
;      [6 'F]
;      [7 'G]
;      [8 'H]
;      [_ (error "doesn't work")])
;      (match (quotient (index-square-on-board b s) 8)
;         [0 1]
;         [1 2]
;         [2 3]
;         [3 4]
;         [4 5]
;         [5 6]
;         [6 7]
;         [7 8]
;         [_ (error "doesn't work")])))]
;    [_ (error "doesn't work")]
;  ))
;      
;(check-expect (kingsquare-to-loc starting-board
;                                 (Some (Piece 'King 'White))) (Loc 'E 1))
;(check-expect (kingsquare-to-loc starting-board
;                                 (Some (Piece 'King 'Black))) (Loc 'E 8))
;(check-expect (kingsquare-to-loc tstbrd1
;                                 (Some (Piece 'King 'White))) (Loc 'D 1))
;
;(: in-check? : ChessGame -> Boolean)
;;; function checks if the current ChessGame is in check or not
;(define (in-check? cg)
;  (match cg
;   [(ChessGame b h)
;    (match h
;      ['() #f]
;      [_ 
;    (match (last h)
;      [(Move _ _ p _ _)
;    (local {(define c (Piece-color p))}
;              (local
;                {(define l
;                   (kingsquare-to-loc b
;                                      (Some
;                                       (Piece 'King (match c
;                                                      ['White 'Black]
;                                                      ['Black 'White])))))}
;                  (or
;                   (ormap (lambda ([l : Loc])
;                            (or (match (board-ref b l)
;                                  [(Some (Piece 'Queen _)) #t] 
;                                  [ _ #f])))
;                           (queen-possible b l))
;                   (ormap (lambda ([l : Loc])
;                            (or (match (board-ref b l)
;                                  [(Some (Piece 'Bishop _)) #t]
;                                  [ _ #f])))
;                          (bishop-possible b l))
;                   (ormap (lambda ([l : Loc])
;                            (or (match (board-ref b l)
;                                  [(Some (Piece 'Rook _)) #t]
;                                  [ _ #f])))
;                          (rook-possible b l))
;                   (ormap (lambda ([l : Loc])
;                            (or (match (board-ref b l)
;                                  [(Some (Piece 'Knight _)) #t]
;                                  [ _ #f])))
;                         (knight-possible b l))
;                   (if (symbol=? 'Black c)
;                   (or
;                    (and (= (length (diagonallyr+ b l)) 1)
;                       (square=? (board-ref b (first (diagonallyr+ b l))) 
;                          (Some (Piece 'Pawn 'Black))))
;
;                    (and (= (length (diagonallyl+ b l)) 1)
;                        (square=? (board-ref b (first (diagonallyl+ b l))) 
;                          (Some (Piece 'Pawn 'Black)))))
;                   (or 
;                   (and (= (length (diagonallyr- b l)) 1)
;                       (square=? (board-ref b (first (diagonallyr- b l))) 
;                          (Some (Piece 'Pawn 'White))))
;
;                    (and (= (length (diagonallyl- b l)) 1)
;                        (square=? (board-ref b (first (diagonallyl- b l))) 
;                          (Some (Piece 'Pawn 'White)))))
;                   ))))])])]))
;
;(check-expect (in-check?
;               (ChessGame tstbrd1
;                          (list (Move (Loc 'C 3)
;                                      (Loc 'C 2)
;                                      (Piece 'Pawn 'Black) 'None 'None))))
;              #t)
;(check-expect (in-check?
;               (ChessGame tstbrd2
;                          (list (Move (Loc 'C 3)
;                                      (Loc 'C 2)
;                                      (Piece 'Pawn 'Black) 'None 'None))))
;              #t)
;(check-expect (in-check?
;               (ChessGame tstbrd3
;                          (list (Move (Loc 'C 3)
;                                      (Loc 'C 2)
;                                      (Piece 'Pawn 'Black) 'None 'None))))
;              #t)
;(check-expect (in-check?
;               (ChessGame tstbrd4
;                          (list (Move (Loc 'C 3)
;                                      (Loc 'C 2)
;                                      (Piece 'Pawn 'Black) 'None 'None))))
;              #t)
;(check-expect (in-check?
;               (ChessGame tstbrd5
;                          (list (Move (Loc 'C 3)
;                                      (Loc 'C 2)
;                                      (Piece 'Pawn 'Black) 'None 'None))))
;              #t)
;(check-expect (in-check?
;               (ChessGame starting-board
;                          (list (Move (Loc 'C 3)
;                                      (Loc 'C 2)
;                                      (Piece 'Pawn 'Black) 'None 'None))))
;              #f)
;(check-expect (in-check?
;               testcg) #t)
;(check-expect (in-check? testcg2) #t)
;(check-expect (in-check? testcg3) #f)
;(check-expect (in-check? testcg4) #f)
;(check-expect (in-check? testcg5) #t)
;
;
;(: king-possible : ChessGame Loc -> (Listof Loc))
;;; gives all the possible moves
;;; if the given piece is a king
;(define (king-possible cg l)
;  (match cg
;    [(ChessGame b h)
;     (match (last h)
;       [(Move _ _ m _ _)
;        (if
;         (not
;          (symbol=? (Piece-color m) (Piece-color (val-of (board-ref b l)))))
;         (filter
;          (lambda ([lc : Loc])
;            (not (in-check?
;                 (ChessGame 
;                   (board-update
;                    (board-update b lc (board-ref b l)) l 'None) h)))) 
;          (append
;           ;; if empty return empty, else return first
;           (if (empty? (vertically+ b l)) '()
;               (list (first (vertically+ b l))))
;           (if (empty? (vertically- b l)) '()
;               (list (first (vertically- b l))))
;           (if (empty? (horizontally- b l)) '()
;               (list (first (horizontally- b l))))
;           (if (empty? (horizontally+ b l)) '()
;               (list (first (horizontally+ b l))))
;           (if (empty? (diagonallyr+ b l)) '()
;               (list (first (diagonallyr+ b l))))
;           (if (empty? (diagonallyl+ b l)) '()
;               (list (first (diagonallyl+ b l))))
;           (if (empty? (diagonallyl- b l)) '()
;               (list (first (diagonallyl- b l))))
;           (if (empty? (diagonallyr- b l)) '()
;               (list (first (diagonallyr- b l))))
;           ))
;         '())])]))
;
;(check-expect (king-possible testcg (Loc 'D 8)) (list (Loc 'E 8) (Loc 'C 7)))
;(check-expect (king-possible 
;               (ChessGame tstbrd1
;                          (list
;                           (Move (Loc 'C 3) (Loc 'C 2)
;                                 (Piece 'Pawn 'Black) 'None 'None))) (Loc 'D 1))
;              (list (Loc 'D 2) (Loc 'C 1) (Loc 'E 1) (Loc 'E 2) (Loc 'C 2)))
;
;(: legal-move? : ChessGame Move -> Boolean)
;;; checks if the move being made is legal 
;;;if the move dst is in possible moves then it is ok
;(define (legal-move? cg m)
;(match* (cg m)
;  [((ChessGame b h) (Move sr d m c p))
;(if (not (symbol=?
;   (match h
;     ['() 'Black]
;     [_ (match (last h)
;          [(Move _ _ m2 _ _)
;           (Piece-color m2)]
;          )])
;     (Piece-color (val-of (board-ref b sr)))))
;     (if (piece=? m (val-of (board-ref b sr)))
;       (if (not (in-check? cg))
;   (match (board-ref b sr) 
;     ['None #f]
;     [(Some (Piece 'Queen _))
;      (ormap (lambda ([l : Loc])
;            (loc=? d l))
;             (queen-possible b sr))]
;     [(Some (Piece 'King _))
;      (ormap (lambda ([l : Loc])
;               (loc=? d l))
;             (king-possible cg sr))]
;     [(Some (Piece 'Rook _))
;      (ormap (lambda ([l : Loc])
;               (loc=? d l))
;             (rook-possible b sr))]
;     [(Some (Piece 'Bishop _))
;      (ormap (lambda ([l : Loc])
;               (loc=? d l))
;             (bishop-possible b sr))]
;     [(Some (Piece 'Knight _))
;      (ormap (lambda ([l : Loc])
;               (loc=? d l))
;             (knight-possible b sr))]
;     [(Some (Piece 'Pawn _))
;      (ormap (lambda ([l : Loc])
;               (loc=? d l))
;             (pawn-possible b sr))])
;    (match (board-ref b sr)
;       [(Some (Piece 'King _))
;      (ormap (lambda ([l : Loc])
;               (loc=? d l))
;             (king-possible cg sr))]
;      [_ #f]))
;   (error "piece moved not on given loc"))
;     #f)]))
;(check-expect
; (legal-move? testcg (Move (Loc 'D 8) (Loc 'D 7)
;                           (Piece 'King 'Black) 'None 'None)) #f)
;(check-expect
; (legal-move? testcg (Move (Loc 'D 8) (Loc 'E 8)
;                           (Piece 'King 'Black) 'None 'None)) #t)
;(check-expect 
;(legal-move? testcg (Move (Loc 'F 7) (Loc 'F 6)
;                          (Piece 'Pawn 'Black) 'None 'None)) #f)
;(check-expect 
;(legal-move? testcg (Move (Loc 'F 7) (Loc 'E 6)
;                          (Piece 'Pawn 'Black) 'None 'None)) #f)
;(check-expect
;(legal-move? new-game (Move (Loc 'B 2) (Loc 'B 4)
;                            (Piece 'Pawn 'White) 'None 'None)) #t)
;(check-expect
;(legal-move? new-game (Move (Loc 'C 2) (Loc 'B 4)
;                            (Piece 'Pawn 'White) 'None 'None)) #f)
;(check-expect
;(legal-move? new-game (Move (Loc 'B 1) (Loc 'A 3)
;                            (Piece 'Knight 'White) 'None 'None)) #t)
;(check-expect
;(legal-move? new-game (Move (Loc 'B 1) (Loc 'B 2)
;                            (Piece 'Knight 'White) 'None 'None)) #f)
;(check-expect
;(legal-move? testcg7 (Move (Loc 'D 5) (Loc 'C 4)
;                           (Piece 'Queen 'Black) 'None 'None)) #t)
;(check-expect
;(legal-move? testcg7 (Move (Loc 'D 5) (Loc 'D 7)
;                           (Piece 'Queen 'Black) 'None 'None)) #t)
;(check-expect
;(legal-move? testcg7 (Move (Loc 'D 5) (Loc 'B 5) (Piece 'Queen 'Black)
;                           (Some (Piece 'Rook 'White)) 'None)) #t)
;(check-expect
;(legal-move? testcg7 (Move (Loc 'D 5) (Loc 'D 4)
;                           (Piece 'Queen 'Black) 'None 'None)) #t)
;(check-expect
;(legal-move? testcg7 (Move (Loc 'D 5) (Loc 'E 4)
;                           (Piece 'Queen 'Black) 'None 'None)) #t)
;(check-expect
;(legal-move? testcg7 (Move (Loc 'D 5) (Loc 'C 6)
;                           (Piece 'Queen 'Black) 'None 'None)) #t)
;(check-expect
;(legal-move? testcg7 (Move (Loc 'D 5) (Loc 'E 6)
;                           (Piece 'Queen 'Black) 'None 'None)) #t)
;(check-expect
;(legal-move? testcg7 (Move (Loc 'D 5) (Loc 'D 2)
;                           (Piece 'Queen 'Black) 'None 'None)) #f)
;(check-expect
;(legal-move? testcg6 (Move (Loc 'B 5) (Loc 'A 5)
;                           (Piece 'Rook 'White) 'None 'None)) #t)
;(check-expect
;(legal-move? testcg6 (Move (Loc 'B 5) (Loc 'E 5)
;                           (Piece 'Rook 'White) 'None 'None)) #f)
;(check-expect
;(legal-move? testcg6 (Move (Loc 'B 5) (Loc 'A 7)
;                           (Piece 'Rook 'White) 'None 'None)) #f)
;(check-expect
;(legal-move? testcg6 (Move (Loc 'A 8) (Loc 'B 7)
;                           (Piece 'Bishop 'White) 'None 'None)) #t)
;(check-expect
;(legal-move? testcg6 (Move (Loc 'A 8) (Loc 'A 7)
;                           (Piece 'Bishop 'White) 'None 'None)) #f)
;
;(: moves-piece : ChessGame Loc -> (Listof Move))
;;;Given a game and a particular piece (identified by its location),
;;;give a list of moves that can legally be made with that piece.
;;;If none, return the empty list.
;(define (moves-piece cg l)
;        (match cg
;          [(ChessGame b h)
;           (match (board-ref b l)
;             ['None '()]
;             [(Some p)
;              (match p
;             [(Piece 'Rook _)
;              (map (lambda ([lp : Loc])
;                     (match (board-ref b lp)
;                       ['None 
;                     (Move l lp p 'None 'None)]
;                       [_ (Move l lp p (board-ref b lp) 'None)]))
;               (rook-possible b l))]
;             [(Piece 'Queen _)
;              (map (lambda ([lp : Loc])
;                     (match (board-ref b lp)
;                       ['None 
;                     (Move l lp p 'None 'None)]
;                       [_ (Move l lp p (board-ref b lp) 'None)]))
;                   (queen-possible b l))]
;             [(Piece 'King _)
;             (map (lambda ([lp : Loc])
;                     (match (board-ref b lp)
;                       ['None 
;                     (Move l lp p 'None 'None)]
;                       [_ (Move l lp p (board-ref b lp) 'None)]))
;                   (king-possible cg l))]
;             [(Piece 'Knight _)
;              (map (lambda ([lp : Loc])
;                     (match (board-ref b lp)
;                       ['None 
;                     (Move l lp p 'None 'None)]
;                       [_ (Move l lp p (board-ref b lp) 'None)]))
;                   (knight-possible b l))]
;             [(Piece 'Bishop _)
;              (map (lambda ([lp : Loc])
;                     (match (board-ref b lp)
;                       ['None 
;                     (Move l lp p 'None 'None)]
;                       [_ (Move l lp p (board-ref b lp) 'None)]))
;                   (bishop-possible b l))]
;             [(Piece 'Pawn _)
;             (map (lambda ([lp : Loc])
;                     (match (board-ref b lp)
;                       ['None 
;                     (Move l lp p 'None 'None)]
;                       [_ (Move l lp p (board-ref b lp) 'None)]))
;                   (pawn-possible b l))])])]))
;
;(check-expect (sort-moves (moves-piece new-game (Loc 'A 2)))
;              (sort-moves (list
; (Move (Loc 'A 2) (Loc 'A 3) (Piece 'Pawn 'White) 'None 'None)
; (Move (Loc 'A 2) (Loc 'A 4) (Piece 'Pawn 'White) 'None 'None))))
;
;(check-expect (sort-moves (moves-piece testcg (Loc 'C 3)))
;              (sort-moves (list
; (Move (Loc 'C 3) (Loc 'E 4) (Piece 'Knight 'White) 'None 'None)
; (Move (Loc 'C 3) (Loc 'E 2) (Piece 'Knight 'White) 'None 'None)
; (Move (Loc 'C 3) (Loc 'B 5) (Piece 'Knight 'White) 'None 'None)
; (Move (Loc 'C 3) (Loc 'D 5) (Piece 'Knight 'White) 'None 'None)
; (Move (Loc 'C 3) (Loc 'B 1) (Piece 'Knight 'White) 'None 'None)
; (Move (Loc 'C 3) (Loc 'A 4) (Piece 'Knight 'White) 'None 'None))))
;
;(check-expect (sort-moves
;               (moves-piece testcg (Loc 'A 2)))
;               (sort-moves
;                (list (Move (Loc 'A 2)
;                            (Loc 'A 3)
;                            (Piece 'Pawn 'White) 'None 'None)
;                      (Move (Loc 'A 2) (Loc 'A 4)
;                            (Piece 'Pawn 'White) 'None 'None))))
; 
;(: ListofLocs : (Listof Loc))
;;; list of possible locs to be used in a helper function 
;(define ListofLocs
;  (list (Loc 'A 1) (Loc 'A 2) (Loc 'A 3) (Loc 'A 4) (Loc 'A 5) (Loc 'A 6)
;        (Loc 'A 7) (Loc 'A 8)
;        (Loc 'B 1) (Loc 'B 2) (Loc 'B 3) (Loc 'B 4) (Loc 'B 5) (Loc 'B 6)
;        (Loc 'B 7) (Loc 'B 8)
;        (Loc 'C 1) (Loc 'C 2) (Loc 'C 3) (Loc 'C 4) (Loc 'C 5) (Loc 'C 6)
;        (Loc 'C 7) (Loc 'C 8)
;        (Loc 'D 1) (Loc 'D 2) (Loc 'D 3) (Loc 'D 4) (Loc 'D 5) (Loc 'D 6)
;        (Loc 'D 7) (Loc 'D 8)
;        (Loc 'E 1) (Loc 'E 2) (Loc 'E 3) (Loc 'E 4) (Loc 'E 5) (Loc 'E 6)
;        (Loc 'E 7) (Loc 'E 8)
;        (Loc 'F 1) (Loc 'F 2) (Loc 'F 3) (Loc 'F 4) (Loc 'F 5) (Loc 'F 6)
;        (Loc 'F 7) (Loc 'F 8)
;        (Loc 'G 1) (Loc 'G 2) (Loc 'G 3) (Loc 'G 4) (Loc 'G 5) (Loc 'G 6)
;        (Loc 'G 7) (Loc 'G 8)
;        (Loc 'H 1) (Loc 'H 2) (Loc 'H 3) (Loc 'H 4) (Loc 'H 5) (Loc 'H 6)
;        (Loc 'H 7) (Loc 'H 8)))
;
;(: oppcolortolocs : Board Square -> (Listof Loc))
;;; outputs the squares of opp color as locs 
;(define (oppcolortolocs b s)
;   (local
;     {(: ll : (Listof Loc) Board Square -> (Listof Loc))
;      (define (ll locs b s)
;      (match locs
;        [(cons f r)
;         (if (not (color=? s (board-ref b f)))
;             (append (list f) (ll r b s))
;             (append '() (ll r b s)))]
;        ['() '()]))}
;     (ll ListofLocs b s)))
;
;(: moves-player : ChessGame -> (Listof Move))
;;For the player whose turn it currently is, determine a list of all legal
;;moves that player could make.
;(define (moves-player cg)
;(match cg
;  [(ChessGame b h)
;   (match (last h)
;     [(Move _ d _ _ _)
;      (match (board-ref b d)
;        ['None (error "player didn't move.")]
;        [s 
;         (local
;           {(: mp : Board Square (Listof Loc) -> (Listof Move))
;            (define (mp b s l)
;              (match l
;                [(cons f r)             
;                 (append (moves-piece cg f) (mp b s r))]
;                ['() '()]))}
;           (mp b s (oppcolortolocs b s)))])])]))
;
;(check-expect (sort-moves (moves-player testcg)) (sort-moves (list
; (Move (Loc 'A 7) (Loc 'A 6) (Piece 'Pawn 'Black) 'None 'None)
; (Move (Loc 'A 7) (Loc 'A 5) (Piece 'Pawn 'Black) 'None 'None)
; (Move (Loc 'B 7) (Loc 'B 6) (Piece 'Pawn 'Black) 'None 'None)
; (Move (Loc 'B 7) (Loc 'B 5) (Piece 'Pawn 'Black) 'None 'None)
; (Move (Loc 'B 8) (Loc 'D 7) (Piece 'Knight 'Black) 'None 'None)
; (Move (Loc 'B 8) (Loc 'A 6) (Piece 'Knight 'Black) 'None 'None)
; (Move (Loc 'C 6) (Loc 'C 5) (Piece 'Pawn 'Black) 'None 'None)
; (Move (Loc 'C 8) (Loc 'D 7) (Piece 'Bishop 'Black) 'None 'None)
; (Move (Loc 'D 8) (Loc 'E 8) (Piece 'King 'Black) 'None 'None)
; (Move (Loc 'D 8) (Loc 'C 7) (Piece 'King 'Black) 'None 'None)
; (Move (Loc 'E 5) (Loc 'E 4) (Piece 'Queen 'Black) 'None 'None)
; (Move (Loc 'E 5) (Loc 'E 3) (Piece 'Queen 'Black) 'None 'None)
; (Move (Loc 'E 5) (Loc 'E 2) (Piece 'Queen 'Black) 'None 'None)
; (Move (Loc 'E 5) (Loc 'E 1) (Piece 'Queen 'Black) 'None 'None)
; (Move (Loc 'E 5) (Loc 'F 5) (Piece 'Queen 'Black) 'None 'None)
; (Move (Loc 'E 5) (Loc 'G 5) (Piece 'Queen 'Black)
;       (Some (Piece 'Bishop 'White)) 'None)
; (Move (Loc 'E 5) (Loc 'D 5) (Piece 'Queen 'Black) 'None 'None)
; (Move (Loc 'E 5) (Loc 'C 5) (Piece 'Queen 'Black) 'None 'None)
; (Move (Loc 'E 5) (Loc 'B 5) (Piece 'Queen 'Black) 'None 'None)
; (Move (Loc 'E 5) (Loc 'A 5) (Piece 'Queen 'Black) 'None 'None)
; (Move (Loc 'E 5) (Loc 'F 6) (Piece 'Queen 'Black) 'None 'None)
; (Move (Loc 'E 5) (Loc 'F 4) (Piece 'Queen 'Black) 'None 'None)
; (Move (Loc 'E 5) (Loc 'G 3) (Piece 'Queen 'Black) 'None 'None)
; (Move (Loc 'E 5) (Loc 'H 2) (Piece 'Queen 'Black)
;       (Some (Piece 'Pawn 'White)) 'None)
; (Move (Loc 'E 5) (Loc 'D 6) (Piece 'Queen 'Black) 'None 'None)
; (Move (Loc 'E 5) (Loc 'C 7) (Piece 'Queen 'Black) 'None 'None)
; (Move (Loc 'E 5) (Loc 'D 4) (Piece 'Queen 'Black) 'None 'None)
; (Move (Loc 'E 5) (Loc 'C 3) (Piece 'Queen 'Black)
;       (Some (Piece 'Knight 'White)) 'None)
; (Move (Loc 'F 7) (Loc 'F 6) (Piece 'Pawn 'Black) 'None 'None)
; (Move (Loc 'F 7) (Loc 'F 5) (Piece 'Pawn 'Black) 'None 'None)
; (Move (Loc 'F 8) (Loc 'E 7) (Piece 'Bishop 'Black) 'None 'None)
; (Move (Loc 'F 8) (Loc 'D 6) (Piece 'Bishop 'Black) 'None 'None)
; (Move (Loc 'F 8) (Loc 'C 5) (Piece 'Bishop 'Black) 'None 'None)
; (Move (Loc 'F 8) (Loc 'B 4) (Piece 'Bishop 'Black) 'None 'None)
; (Move (Loc 'F 8) (Loc 'A 3) (Piece 'Bishop 'Black) 'None 'None)
; (Move (Loc 'G 7) (Loc 'G 6) (Piece 'Pawn 'Black) 'None 'None)
; (Move (Loc 'H 7) (Loc 'H 6) (Piece 'Pawn 'Black) 'None 'None)
; (Move (Loc 'H 7) (Loc 'H 5) (Piece 'Pawn 'Black) 'None 'None)
; (Move (Loc 'H 8) (Loc 'G 8) (Piece 'Rook 'Black) 'None 'None))))
;
;(check-expect (sort-moves (moves-player testcg2)) (sort-moves (list
; (Move (Loc 'A 7) (Loc 'A 6) (Piece 'Pawn 'Black) 'None 'None)
; (Move (Loc 'A 8) (Loc 'C 7) (Piece 'Knight 'Black) 'None 'None)
; (Move (Loc 'A 8) (Loc 'B 6) (Piece 'Knight 'Black) 'None 'None)
; (Move (Loc 'C 4) (Loc 'C 3) (Piece 'Pawn 'Black) 'None 'None)
; (Move (Loc 'C 4) (Loc 'D 3) (Piece 'Pawn 'Black)
;       (Some (Piece 'Pawn 'White)) 'None)
; (Move (Loc 'C 6) (Loc 'D 7) (Piece 'Bishop 'Black) 'None 'None)
; (Move (Loc 'C 6) (Loc 'E 8) (Piece 'Bishop 'Black) 'None 'None)
; (Move (Loc 'C 6) (Loc 'D 5) (Piece 'Bishop 'Black) 'None 'None)
; (Move (Loc 'C 6) (Loc 'E 4) (Piece 'Bishop 'Black) 'None 'None)
; (Move (Loc 'C 6) (Loc 'F 3) (Piece 'Bishop 'Black) 'None 'None)
; (Move (Loc 'C 6) (Loc 'G 2) (Piece 'Bishop 'Black) 'None 'None)
; (Move (Loc 'C 6) (Loc 'H 1) (Piece 'Bishop 'Black) 'None 'None)
; (Move (Loc 'C 6) (Loc 'B 7) (Piece 'Bishop 'Black) 'None 'None)
; (Move (Loc 'C 6) (Loc 'B 5) (Piece 'Bishop 'Black) 'None 'None)
; (Move (Loc 'C 6) (Loc 'A 4) (Piece 'Bishop 'Black) 'None 'None)
; (Move (Loc 'C 8) (Loc 'C 7) (Piece 'Rook 'Black) 'None 'None)
; (Move (Loc 'C 8) (Loc 'B 8) (Piece 'Rook 'Black) 'None 'None)
; (Move (Loc 'D 6) (Loc 'D 5) (Piece 'Pawn 'Black) 'None 'None)
; (Move (Loc 'D 6) (Loc 'E 5) (Piece 'Pawn 'Black)
;       (Some (Piece 'Rook 'White)) 'None)
; (Move (Loc 'G 7) (Loc 'G 6) (Piece 'Pawn 'Black) 'None 'None)
; (Move (Loc 'G 7) (Loc 'G 5) (Piece 'Pawn 'Black) 'None 'None)
; (Move (Loc 'H 8) (Loc 'H 7) (Piece 'Queen 'Black) 'None 'None)
; (Move (Loc 'H 8) (Loc 'H 6) (Piece 'Queen 'Black) 'None 'None)
; (Move (Loc 'H 8) (Loc 'H 5) (Piece 'Queen 'Black) 'None 'None)
; (Move (Loc 'H 8) (Loc 'H 4) (Piece 'Queen 'Black) 'None 'None)
; (Move (Loc 'H 8) (Loc 'H 3) (Piece 'Queen 'Black) 'None 'None)
; (Move (Loc 'H 8) (Loc 'H 2) (Piece 'Queen 'Black)
;       (Some (Piece 'Pawn 'White)) 'None)
; (Move (Loc 'H 8) (Loc 'G 8) (Piece 'Queen 'Black) 'None 'None)
; (Move (Loc 'H 8) (Loc 'F 8) (Piece 'Queen 'Black) 'None 'None)
; (Move (Loc 'H 8) (Loc 'E 8) (Piece 'Queen 'Black) 'None 'None))))
;
;
;
;(: destintolocs : (Listof Move) -> (Listof Loc))
;;; helper function where the list of move is converted to a list of loc
;(define (destintolocs lms)
;  (map (lambda ([m : Move])
;         (match m
;           [(Move _ d _ _ _) d])) lms))
;(check-expect
; (destintolocs (list (Move (Loc 'D 3) (Loc 'D 4)
;                           (Piece 'Pawn 'Black) 'None 'None)
;(Move (Loc 'D 6) (Loc 'D 7) (Piece 'Pawn 'Black) 'None 'None)))
;(list (Loc 'D 4) (Loc 'D 7)))
;
;(: contains? :  Loc (Listof Loc) -> Boolean)
;;; checks if the list of locs contains given loc
;(define (contains? l ll)
;  (ormap (lambda ([l1 : Loc])
;           (loc=? l1 l)) ll))
;
;(check-expect (contains? (Loc 'A 4)
;                         (list (Loc 'F 4) (Loc 'H 4) (Loc 'E 3))) #f)
;(check-expect (contains? (Loc 'F 4)
;                         (list (Loc 'F 4) (Loc 'H 4) (Loc 'E 3))) #t)
;
;(: piece? : Square -> Boolean)
;;; checks if square is a piece 
;(define (piece? s)
;  (match s
;    ['None #f]
;    [_ #t]))
;
;(check-expect (piece? (Some (Piece 'Pawn 'Black))) #t)
;(check-expect (piece? 'None) #f)
;
;(: pathtocanbeblocked : Board Loc -> (Listof Loc))
;;; path to defines the list of loc that are the path from the
;;; first loc to the next ones causing check 
;(define (pathtocanbeblocked b l1)
;  (local {(: fs : Board (Listof Loc) -> Symbol)
;          (define (fs b f)
;            (Piece-type (val-of (board-ref b (last f)))))}
;    (local {(: nelp : Board (Listof Loc) -> Boolean)
;            (define (nelp b f)
;              (and (not (empty? f))
;                   (piece? (board-ref b (last f)))))}                    
;      (append 
;       (if (nelp b (vertically+ b l1))
;           (if 
;            (or (symbol=? (fs b (vertically+ b l1)) 'Queen)
;                (symbol=? (fs b (vertically+ b l1)) 'Rook))
;            (vertically+ b l1) '()) '())
;       (if (nelp b (vertically- b l1))
;           (if 
;            (or (symbol=? (fs b (vertically- b l1)) 'Queen)
;                (symbol=? (fs b (vertically- b l1)) 'Rook))
;            (vertically- b l1) '()) '())
;       (if (nelp b (horizontally+ b l1))
;           (if 
;            (or (symbol=? (fs b (horizontally+ b l1)) 'Queen)
;                (symbol=? (fs b (horizontally+ b l1)) 'Rook))
;            (horizontally+ b l1) '()) '())
;       (if (nelp b (horizontally- b l1))
;           (if 
;            (or (symbol=? (fs b (horizontally- b l1)) 'Queen)
;                (symbol=? (fs b (horizontally- b l1)) 'Rook))
;            (horizontally- b l1) '()) '())
;       (if (nelp b (diagonallyl+ b l1))
;           (if 
;            (or (symbol=? (fs b (diagonallyl+ b l1)) 'Queen)
;                (symbol=? (fs b (diagonallyl+ b l1)) 'Bishop))
;            (diagonallyl+ b l1) '()) '())
;       (if (nelp b (diagonallyr+ b l1))
;           (if  
;            (or (symbol=? (fs b (diagonallyr+ b l1)) 'Queen)
;                (symbol=? (fs b (diagonallyr+ b l1)) 'Bishop))
;            (diagonallyr+ b l1) '()) '())
;       (if (nelp b (diagonallyl- b l1))
;           (if 
;            (or (symbol=? (fs b (diagonallyl- b l1)) 'Queen)
;                (symbol=? (fs b (diagonallyl- b l1)) 'Bishop))
;            (diagonallyl- b l1) '()) '())
;       (if (nelp b (diagonallyl+ b l1))
;           (if 
;            (or (symbol=? (fs b (diagonallyl+ b l1)) 'Queen)
;                (symbol=? (fs b (diagonallyl+ b l1)) 'Bishop))
;            (diagonallyl+ b l1) '()) '())
;       (if (nelp b (diagonallyr- b l1))
;           (if 
;            (or (symbol=? (fs b (diagonallyr- b l1)) 'Queen)
;                (symbol=? (fs b (diagonallyr- b l1)) 'Bishop))
;            (diagonallyr- b l1) '()) '())))))
;
;(: anysame? : (Listof Loc) (Listof Loc) -> Boolean)
;;; helper function that checks if any loc are the same
;;; between two lists 
;(define (anysame? l1 l2)
;(ormap (lambda ([l : Loc])
;         (contains? l l2)) l1))
;
;(check-expect (anysame? (list (Loc 'A 3) (Loc 'B 4)) (list (Loc 'A '3))) #t) 
;(check-expect (anysame? (list (Loc 'A 3) (Loc 'B 4)) (list (Loc 'B '3))) #f)
;
;(: checkmate? : ChessGame -> Boolean)
;;Return true if and only if the player whose turn it currently is, is checkmated.
;; checks if King is in-check and surrounded on all sides
;; or if the moves possible are in-check too, then it is checkmate
;(define (checkmate? cg)
;  (match cg
;    [(ChessGame b h)
;     (match h
;       ['() #f]
;       [ _
;         (match (last h)
;           [(Move _ _ m _ _)
;            (match m
;              [(Piece t c)
;            (local 
;            {(define l
;               (kingsquare-to-loc b
;                                  (Some
;                                   (Piece 'King (match c
;                                                         ['White 'Black]
;                                                         ['Black 'White])))))}
;              (if (in-check? cg)
;            (match (king-possible cg l)
;              ['()
;               (not (anysame? (pathtocanbeblocked b l)
;                         (destintolocs (moves-player cg))))]
;              [_ #f]) #f))])])])]))
;
;(check-expect (checkmate? testcg) #f)
;(check-expect (checkmate? testcg2) #t)
;(check-expect (checkmate? testcg3) #f)
;(check-expect (checkmate? testcg4) #f)
;(check-expect (checkmate? testcg5) #t)
;
;(: stalemate? : ChessGame -> Boolean)
;;Return true if and only if the player whose turn it currently is,
;;is not in check but yet has no available moves.
;;; checks if possible moves for all remaining pieces are '()
;(define (stalemate? cg)
;       (and (not (in-check? cg))
;            (match (moves-player cg)
;              ['() #t]
;              [_ #f])))
;
;(check-expect (stalemate? testcg) #f)
;(check-expect (stalemate? testcg2) #f)
;(check-expect (stalemate? testcg3) #t)
;(check-expect (stalemate? testcg4) #t)
;(check-expect (stalemate? testcg5) #f)
;
;(: apply-move : ChessGame Move -> ChessGame)
;;;Make the specified move for the player whose turn it is,
;;;modifying the board accordingly. Update the history of moves.
;;;Raise an error if the desired move is not legal according to legal-move?.
;(define (apply-move g m)
;  (if (not (legal-move? g m)) (error "not a legal move") 
;  (match* (g m)
;    [((ChessGame b h) (Move s d _ _ _))
;  (ChessGame
;   (board-update (board-update b d (Some (Move-moved m))) s 'None)
;   (append h (list m)))])))
;
;(check-expect (apply-move testcg (Move (Loc 'D 8)
;                                       (Loc 'E 8)
;                                       (Piece 'King 'Black) 'None 'None))
;(ChessGame
; (list
;  'None
;  'None
;  (Some (Piece 'King 'White))
;  (Some (Piece 'Rook 'White))
;  'None
;  (Some (Piece 'Bishop 'White))
;  'None
;  (Some (Piece 'Rook 'White))
;  (Some (Piece 'Pawn 'White))
;  (Some (Piece 'Pawn 'White))
;  (Some (Piece 'Pawn 'White))
;  'None
;  'None
;  (Some (Piece 'Pawn 'White))
;  (Some (Piece 'Pawn 'White))
;  (Some (Piece 'Pawn 'White))
;  'None
;  'None
;  (Some (Piece 'Knight 'White))
;  'None
;  'None
;  'None
;  'None
;  'None
;  'None
;  'None
;  'None
;  'None
;  'None
;  'None
;  'None
;  'None
;  'None
;  'None
;  'None
;  'None
;  (Some (Piece 'Queen 'Black))
;  'None
;  (Some (Piece 'Bishop 'White))
;  'None
;  'None
;  'None
;  (Some (Piece 'Pawn 'Black))
;  'None
;  (Some (Piece 'Pawn 'Black))
;  'None
;  'None
;  'None
;  (Some (Piece 'Pawn 'Black))
;  (Some (Piece 'Pawn 'Black))
;  'None
;  'None
;  'None
;  (Some (Piece 'Pawn 'Black))
;  (Some (Piece 'Pawn 'Black))
;  (Some (Piece 'Pawn 'Black))
;  (Some (Piece 'Rook 'Black))
;  (Some (Piece 'Knight 'Black))
;  (Some (Piece 'Bishop 'Black))
;  'None
;  (Some (Piece 'King 'Black))
;  (Some (Piece 'Bishop 'Black))
;  'None
;  (Some (Piece 'Rook 'Black)))
; (list
;  (Move (Loc 'F 4) (Loc 'G 5) (Piece 'Bishop 'White) 'None 'None)
;  (Move (Loc 'D 8) (Loc 'E 8) (Piece 'King 'Black) 'None 'None))))
;
;(check-expect (apply-move
; new-game (Move (Loc 'A 2) (Loc 'A 4) (Piece 'Pawn 'White) 'None 'None))
;              (ChessGame
; (list
;  (Some (Piece 'Rook 'White))
;  (Some (Piece 'Knight 'White))
;  (Some (Piece 'Bishop 'White))
;  (Some (Piece 'Queen 'White))
;  (Some (Piece 'King 'White))
;  (Some (Piece 'Bishop 'White))
;  (Some (Piece 'Knight 'White))
;  (Some (Piece 'Rook 'White))
;  'None
;  (Some (Piece 'Pawn 'White))
;  (Some (Piece 'Pawn 'White))
;  (Some (Piece 'Pawn 'White))
;  (Some (Piece 'Pawn 'White))
;  (Some (Piece 'Pawn 'White))
;  (Some (Piece 'Pawn 'White))
;  (Some (Piece 'Pawn 'White))
;  'None
;  'None
;  'None
;  'None
;  'None
;  'None
;  'None
;  'None
;  (Some (Piece 'Pawn 'White))
;  'None
;  'None
;  'None
;  'None
;  'None
;  'None
;  'None
;  'None
;  'None
;  'None
;  'None
;  'None
;  'None
;  'None
;  'None
;  'None
;  'None
;  'None
;  'None
;  'None
;  'None
;  'None
;  'None
;  (Some (Piece 'Pawn 'Black))
;  (Some (Piece 'Pawn 'Black))
;  (Some (Piece 'Pawn 'Black))
;  (Some (Piece 'Pawn 'Black))
;  (Some (Piece 'Pawn 'Black))
;  (Some (Piece 'Pawn 'Black))
;  (Some (Piece 'Pawn 'Black))
;  (Some (Piece 'Pawn 'Black))
;  (Some (Piece 'Rook 'Black))
;  (Some (Piece 'Knight 'Black))
;  (Some (Piece 'Bishop 'Black))
;  (Some (Piece 'Queen 'Black))
;  (Some (Piece 'King 'Black))
;  (Some (Piece 'Bishop 'Black))
;  (Some (Piece 'Knight 'Black))
;  (Some (Piece 'Rook 'Black)))
; (list (Move (Loc 'A 2) (Loc 'A 4) (Piece 'Pawn 'White) 'None 'None))))
;
;
;(test)
