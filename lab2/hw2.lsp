;; Problem 1

;; DFS performs a depth-first search of a tree
;; Arguments: Takes a list representation of the tree
;; Returns a single, top-level list of the terminal nodes in the order they would 
;; be visited by a left-to-right DFS

#| 
Test cases for DFS:
	(DFS '((A (B)) C (D))) => (A B C D)
	(DFS '((A (B)) C (D (E)))) => (A B C D E)
|#

(defun DFS (tree)
  (cond
  	
  	; If we hit the end of the tree and there are no nodes, return an empty list
    ((null tree) '())

    ; If the tree is just an atom and not a list, make it a list
    ((atom tree) (list tree))

    ; Else, if none of the above conditions are tree, perform DFS on left and 
    ; right subtrees
    (t (append (DFS (car tree)) (DFS (cdr tree))))
  )
)

;; Problem 2

;; dfs-upto-depth performs DFS up to a specified depth.
;; Arguments: Takes two arguments - 1) a list representation of the tree and 2) an
;; integer representing the depth to perform DFS up to
;; Returns a single, top-level list of the terminal nodes in the order they would 
;; be visited by a DFS search up to the depth

(defun dfs-upto-depth (tree depth)
  (cond

  	; If we hit the end of the tree and there are no nodes, return an empty list
    ((null tree) '())

    ; If the tree is just an atom and not a list, make it a list
    ((atom tree) (list tree))

    ; If we reach the deepest depth we can go up to, return an empty list
    ((<= depth 0) '())

    ; Else, if none of the above conditions are tree, perform DFS on left and 
    ; right subtrees while decreasing depth by 1
    (t (append (dfs-upto-depth (car tree) (- depth 1)) (dfs-upto-depth (cdr tree) depth)))))

;; DFID performs a depth-first interative-deepening search of a tree
;; Arguments: Takes two arguments - 1) a list representation of the tree and 2) an
;; integer representing the max depth of the tree
;; Returns a single, top-level list of the terminal nodes in the order they would 
;; be visited by a left-to-right depth-first interative-deepening search

#|
Test cases for DFID:
	(DFID '((A (B)) C (D)) 0) => NIL
	(DFID '((A (B)) C (D)) 2) => (C A C D)
	(DFID '((A (B)) C (D)) 3) => (C A C D A B C D)
	(DFID '((A (B)) C (D)) 4) => (C A C D A B C D A B C D)
	(DFID '((A (B)) C (D (E))) 3) => (C A C D A B C D E)
|#

(defun DFID(tree depth)
	(cond 
		((null tree) '())

		; If we reach the deepest depth we can go up to, run dfs-upto-depth with 
		; depth = 0
		((<= depth 0) (dfs-upto-depth tree 0))
    	(t (append (DFID tree (- depth 1)) (dfs-upto-depth tree depth)))))

;; Problem 3

;; BFS performs a breadth-first of a tree
;; Arguments: Takes a list representation of the tree
;; Returns a single, top-level list of the terminal nodes in the order they would 
;; be visited by a left-to-right BFS

#| 
Test cases for BFS:
	(BFS '((A (B)) C (D))) => (C A D B)
	(BFS '((A (B)) C (D (E)))) => (C A D B E)
	(BFS '((A) (B C D) E)) => (E A B C D)
|#

(defun BFS (tree)
	(cond 
		((null tree) '())

		; Check if the first item of the list is an atom. If it is, then make it
		; a list and append the BFS search of the rest of the tree
		((atom (car tree)) (append (list (car tree)) (BFS (cdr tree))))

		; Otherwise, it is a list and append first item to the rest of the tree
		; and run BFS on it
    	(t (BFS (append (cdr tree) (car tree))))))

