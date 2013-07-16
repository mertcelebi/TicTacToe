; ************************************************************************
; CS 201a

; ********************************************************
; Name: Feridun Mert Celebi
; Email address: feridun.celebi@yale.edu
; ********************************************************

; ************************************************************************

; Topics: vectors, objects and running times.

; ************************************************************************
; We include the following libraries.

; This command loads a library containing the random-integer procedure.
(#%require srfi/27)

; The following command loads a library of time procedures.
(#%require srfi/19)

; ************************************************************************
; Another Scheme data type: vectors.

; Scheme has 1-dimensional vectors, whose elements are
; accessed by a zero-based index.

; (make-vector n value) creates a vector of length n, filled with value.
; (vector-ref vec i) returns the value of the vector entry at index i.
; (vector-set! vec i value) sets the value of the vector entry at index i
;                           to be value.

; The operations of vector-ref and vector-set! take constant time.

; ************************************************************************
; ** problem 1 ** (15 points) - Done
; Write procedures using Scheme vectors 
; to create and access 2-dimensional arrays, as follows.

; (make-array m n value)
; (array-ref arr i j)
; (array-set! arr i j value)

; (make-array m n value)
; returns a representation of an array with m rows and n columns
; as a vector of length m whose entries are vectors of length n
; each of whose entries is equal to value.

; (array-ref arr i j) returns the value in row i and column j
; of the array arr.  Indexing is zero-based.

; (array-set! arr i j value) sets the value in row i and column j
; of the array arr to be value.  Indexing is zero-based.

; Examples:

;; > (define a1 (make-array 3 2 'x))
;; > a1
;; #(#(x x) #(x x) #(x x))
;; > (array-set! a1 2 0 5)
;; > a1  
;; #(#(x x) #(x x) #(5 x))
;; > (array-set! a1 1 1 -13)
;; > a1
;; #(#(x x) #(x -13) #(5 x))
;; > (array-set! a1 1 1 'hi!)
;; > a1
;; #(#(x x) #(x hi!) #(5 x))
;; > (array-ref a1 2 0)
;; 5
; ************************************************************************

(define make-array
  (lambda (m n value)
    (make-array-helper1 (make-vector m value) (- m 1) n value)))

; Receives the vector that would represent the columns of the array, m, n, value, index and a variable to represent void.
; Then this procedure basically operates the recursion and makes calls to the make-array-helper2 which updates the value of vec.
(define make-array-helper1
  (lambda (vec m n value)
    (cond
      ((equal? m -1) vec)
      (else
       (make-array-helper1 (make-array-helper2 vec m n value) (- m 1) n value)))))

; Receives the vector, m, n, value and index. Then sets the given index to a vector with length n and value val.
(define make-array-helper2
  (lambda (vec m n value)
    (vector-set! vec m (make-vector n value)) vec))

(define array-ref
  (lambda (arr i j)
    (vector-ref (vector-ref arr i) j)))

(define array-set!
  (lambda (arr i j value)
    (vector-set! (vector-ref arr i) j value)))

; ************************************************************************
; ** problem 2 ** (20 points) - Done
; Write a procedure

; (make-ttt-game)

; that takes no arguments and returns
; a procedure implementing a tic-tac-toe game object.
; The procedure has local state representing the positions
; of x's and o's on the board (and any other relevant
; information) and responds to the following commands.

; command           result
; -------           ------
; board             returns the game board as a list of 3
;                   lists, each of which is a list of 3 symbols 
;                   chosen from: x, o, -.
;                   The board
;                         x | o | 
;                         ---------
;                         o | x | o
;                         ---------
;                           | x | o
;                   is represented as
;                   ((x o -) (o x o) (- x o))

; play sym i j      attempts to place symbol sym on the board, where
;                   i is the zero-indexed row (top to bottom)
;                   j is the zero-indexed column (left to right).
;                   If the move is not permitted,
;                   the symbol not-permitted is returned.
;                   Otherwise, the move is made and
;                   if x has won, the symbol x-won is returned,
;                   if o has won, the symbol o-won is returned,
;                   if the board is filled and neither has won,
;                   the symbol cats-game is returned,
;                   otherwise, the board is returned (as for board.)

;                   A move is not permitted 
;                   if it is out of turn, 
;                   if the square is already filled,
;                   or if x or o has already won.  You may assume
;                   that sym is either x or o, and i and j are
;                   between 0 and 2 inclusive.  Note that either
;                   x or o may make the first move.

; Examples:


;; > (g1 'board)
;; ((- - -) (- - -) (- - -))
;; > (g1 'play 'x 1 1)
;; ((- - -) (- x -) (- - -))
;; > (g1 'play 'x 1 2)
;; not-permitted
;; > (g1 'play 'o 1 1)
;; not-permitted
;; > (g1 'play 'o 2 2)
;; ((- - -) (- x -) (- - o))
;; > (g1 'play 'x 1 2)
;; ((- - -) (- x x) (- - o))
;; > (g1 'play 'o 1 0)
;; ((- - -) (o x x) (- - o))
;; > (g1 'play 'x 0 2)
;; ((- - x) (o x x) (- - o))
;; > (g1 'play 'o 0 0)
;; ((o - x) (o x x) (- - o))
;; > (g1 'play 'x 2 0)
;; x-won
;; > (g1 'play 'o 0 1)
;; not-permitted
; ************************************************************************

(define make-ttt-game
  (lambda ()
    (let ((table (make-array 3 3 '-)))
    (lambda (msg . args)
      (let ((tab (get-list table '() 0 0)))
      (case msg
        ((board) tab)
        ((play) (cond
                  ((or (is-out-turn? (car args) (get-num 'x tab 0) (get-num 'o tab 0))
                      (is-filled table (cadr args) (caddr args))
                      (player-won? tab 'x) (player-won? tab 'y)) 'not-permitted)
                  (else (array-set! table (caddr args) (cadr args) (car args))
                        (do-something table (get-list table '() 0 0)))))))))))

(define do-something
  (lambda (table tab)
    (cond
       ((player-won? tab 'x) 'x-won)
       ((player-won? tab 'y) 'y-won)
       (else
        tab))))

(define get-list
  (lambda (tab lst m n)
    (cond
      ((equal? n 3) lst)
      ((< m 3) (get-list tab (append lst (list (array-ref tab m n))) (+ m 1) n))
      (else
       (get-list tab (cons lst (get-list tab '() 0 (+ n 1))) 3 3)))))

(define get-num
  (lambda (sym lst count)
    (cond
      ((null? lst) count)
      ((list? (car lst)) (+ (get-num sym (car lst) count) (get-num sym (cdr lst) count)))
      ((equal? (car lst) sym) (get-num sym (cdr lst) (+ 1 count)))
      (else
       (get-num sym (cdr lst) count)))))

(define is-out-turn?
  (lambda (sym no-x no-y)
    (cond
      ((and (equal? sym 'x) (> no-x no-y)) #t)
      ((and (equal? sym 'y) (< no-x no-y)) #t)
      (else
       #f))))

(define is-filled
  (lambda (table i j)
    (let ((ele (array-ref table j i)))
      (not (equal? ele '-)))))

(define player-won?
  (lambda (lst sym)
    (or (helper-rows lst sym) (helper-columns (car lst) (cadr lst) (caddr lst) sym) (helper-diag lst sym 0))))

(define helper-rows
  (lambda (lst sym)
    (cond
    ((null? lst) #f)
    ((and (equal? (caar lst) sym) (equal? (caar lst) (cadar lst)) (equal? (caar lst) (caddar lst))) #t)
    (else
     (helper-rows (cdr lst) sym)))))

(define helper-columns
  (lambda (lst1 lst2 lst3 sym)
    (cond
      ((null? lst1) #f)
      ((and (equal? (car lst1) sym) (equal? (car lst1) (car lst2)) (equal? (car lst1) (car lst3))) #t)
      (else
       (helper-columns (cdr lst1) (cdr lst2) (cdr lst3) sym)))))

(define helper-diag
  (lambda (lst sym count)
    (cond
      ((equal? count 4) #f)
      ((and (equal? count 0) (equal? (caar lst) sym) (equal? (caar lst) (cadadr lst)) (equal? (caar lst) (caddddr lst))) #t)
      ((and (equal? count 2) (equal? (caddar lst) sym) (equal? (caddar lst) (cadadr lst)) (equal? (caddar lst) (caaddr lst))) #t) 
      (else
       (helper-diag lst sym (+ count 2))))))
