;ARGUMENTS: N: number, TREE: ordered tree
;RETURNS: whether N appears in TREE
;SOLUTION:
;checks to see if TREE is a list
;if TREE is a list, recursively calls function on left and right branches 
;else returns NIL
(defun TREE-CONTAINS (N TREE)
(cond ((listp TREE)
    (cond
        ((TREE-CONTAINS N (car TREE)) t)
        ((= N (cadr TREE)) t)
        ((TREE-CONTAINS N (caddr TREE)) t)
        (t NIL)
    )
)
((= N TREE) t)
(t NIL))
)

;ARGUMENTS: TREE: ordered tree
;RETURNS: node in tree with largest value
;SOLUTION:
;checks if TREE is a list and recursively calls until the rightmost branch
(defun TREE-MAX (TREE)
(cond ((AND (listp TREE) )
        (TREE-MAX (caddr TREE)))
	(t TREE)
)
)

;ARGUMENTS: TREE: ordered tree
;RETURNS: in-ordered list of the numbers in TREE
;SOLUTION:
;checks if TREE is list
;if so, cons the middle and right branches, then appends the left branch
;else returns that number
(defun TREE-ORDER (TREE)
(cond
    ((listp TREE) (append (TREE-ORDER(car TREE)) (cons (cadr TREE) (TREE-ORDER(caddr TREE)))))
    (t (list TREE))
)
)

;ARGUMENTS: L: list, START: non-negative integer, LEN: non-negative integer
;RETURNS: sub-list of L starting at position START and having length LEN
;SOLUTIONS:
;checks to see if the list is an empty list
;if START is > 0, recursively calls function without first value of L and with
;;START-1
;if LEN is > 0, cons the first element of L with a recursive call to the
;;function with a decremented value of LEN
(defun SUB-LIST (L START LEN)
  (cond
   ((equal L NIL) NIL)
     ((> START 0) (SUB-LIST (cdr L) (- START 1) LEN))
     ((> LEN 0) (cons (car L) (SUB-LIST (cdr L) START (- LEN 1))))
     (t NIL)
     )
)

;ARGUMENTS: L: list
;RETURNS: single list of two lists L1 and L2 (L is the result of appending L1 and L2, length of L2 minus length of L1 is 0 or 1)
;SOLUTION:
;if the list has an even number of elements, use the SUB-LIST function to break up L into even lists
;if the list has an odd number of elements, break it up such that the first list has one fewer element than the second
(defun SPLIT-LIST (L)
(cond
    ((evenp (length L)) (list (SUB-LIST L  0 (/ (length L) 2)) (SUB-LIST L (/ (length L) 2) (length L))))
    (t (list (SUB-LIST L  0 (/ (- (length L) 1) 2)) (SUB-LIST L (/ (- (length L) 1) 2) (length L))))
)
)

;ARGUMENTS: TREE: binary tree
;RETURNS: height of TREE
;SOLUTION:
;checks if TREE is just a number and returns 0 if so
;checks to see if the right or left branch has a greater "height" i.e.
;;makes a greater number of recursive calls
(defun BTREE-HEIGHT (TREE)
  (cond 
   ((NOT (listp TREE)) 0)
   ((> (BTREE-HEIGHT(car TREE)) (BTREE-HEIGHT(cadr TREE))) (+ (BTREE-HEIGHT(car TREE)) 1))
   (t (+ (BTREE-HEIGHT(cadr TREE)) 1))
   )
)

;ARGUMENTS: LEAVES: non-empty list of atoms
;RETURNS: binary tree (leaves are elements of LEAVES, number of leaves in right branch minus number of leaves in left branch is 0 or 1
;SOLUTION:
;checks for branch with only single leaf
;if there are greater than 2 leaves, uses SPLIT-LIST function to break the
;;LEAVES into smaller lists and combines them into one list recursively 
(defun LIST2BTREE (LEAVES)
  (cond
   ((= (length LEAVES) 1) (car LEAVES))
   ((> (length LEAVES) 2) (list (LIST2BTREE (car (SPLIT-LIST LEAVES))) (LIST2BTREE (cadr (SPLIT-LIST LEAVES)))))
   (t LEAVES)
   )
)

;ARGUMENTS: TREE: binary tree
;RETURNS: list of atoms (each node has at most two children)
;SOLUTION:
;if TREE is not a list, return the number
;else append the different branches of the tree together recursively
(defun BTREE2LIST (TREE)
  (cond
   ((NOT (listp TREE)) (list TREE))
   (t (append(BTREE2LIST (car TREE)) (BTREE2LIST (cadr TREE))) ) 
   )
)
