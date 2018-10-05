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
