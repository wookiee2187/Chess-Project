#lang typed/racket

(require "../include/cs151-core.rkt")

;; ==== ==== ==== ====
;; external interface

(provide (struct-out Some)
         Optional
         get-opt
         val-of
         opt-map)

;; ==== ==== ==== ====
;; data definitions

(define-struct (Some T)
  ([value : T]))

(define-type (Optional T)
  (U 'None (Some T)))

;; ==== ==== ==== ====
;; operations

(: get-opt : All (T) (Optional T) T -> T)
;; unwrap Some or return default on 'None
(define (get-opt opt def)
  (match opt
    ['None def]
    [(Some x) x]))

(: val-of : All (T) (Optional T) -> T)
;; unwrap Some or raise error
(define (val-of opt)
  (match opt
    ['None (error "val-of: None")]
    [(Some x) x]))

(: opt-map : All (T U) (T -> U) (Optional T) -> (Optional U))
;; apply function to Some value; 'None to 'None
(define (opt-map f opt)
  (match opt
    ['None 'None]
    [(Some x) (Some (f x))]))

(define-type (Tree A)
  (U 'E (Nd A)))

(define-struct (Nd A)
  ([val : A]
   [next : (Tree A)]))