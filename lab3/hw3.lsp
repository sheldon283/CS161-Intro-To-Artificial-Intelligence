;; Sheldon Dong
;; 004784870
;; CS 161

;; generate_test_data returns a list with the specified number of values and an upper bound on the random number generated
;; Arguments: takes an integer representing the number of values wanted in the list (numberOfValues), and an integer representing
;; the upper bound on what each value in the list can go up to (upper)
;; Returns a list with the specified number of values and numbers in each value spot that has an upperbound of that which is specified
(defun generate_test_data (numberOfValues upper)
        (cond
                ((equal numberOfValues 0) '())
                ((equal numberOfValues 1) (list (random upper)))
                (t (cons (random upper) (generate_test_data (- numberOfValues 1) upper)))))

;; SUM calculates the total sum of a list
;; Arguments: List of integers
;; Returns a number that represents total sum of the integers in that list

(defun SUM (integerList)
  (cond 
    ((null integerList) 0)
    (t (+ (car integerList) (SUM(cdr integerList))))))

;; PartitionHelper performs a recursive calculation for the 2-set partitioning that has the lowest difference between the sums. 
;; A list of the form (difference leftPartition rightPartition) is constructed once the base case of the listLength being equal
;; to 0 is hit.
;; Arguments: List of positive integers (integers), the calculatedSum at that point (sumCalculated), the length of the list
;; (listlength), the totalSum of the entire list at the beginning (totalSum), a list of the leftPartition (leftPartition),
;; and a list of the rightPartition (rightPartition)
;; Returns the list that has the minimum difference, with the left and right partitions

(defun partitionHelper (integers sumCalculated listLength totalSum leftPartition rightPartition)
  (cond 
    ((equal listLength 0) (let ((diff (abs (- (- totalSum sumCalculated) sumCalculated)))) (list diff (list leftPartition (list rightPartition)))))
    (t (let ((leftSide (partitionHelper (cdr integers) (+ sumCalculated (car integers)) (- listLength 1) totalSum (cons (car integers) leftPartition) rightPartition))
            (rightSide (partitionHelper (cdr integers) sumCalculated (- listLength 1) totalSum leftPartition (cons (car integers) rightPartition))))
            (cond 
              ((< (car leftSide) (car rightSide)) leftSide)
              (t rightSide))))))


;; PARTITION optimally partition sets of integers by calling the helper function partitionHelper
;; Arguments: Takes a list of non-negative integers
;; Returns an optimal partition of those integers

(defun PARTITION (integers)
  (partitionHelper integers 0 (length integers) (SUM integers) NIL NIL))