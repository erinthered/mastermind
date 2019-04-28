;;;*********************************************************************************************************
;;;THIS FILE IS THE MASTERMIND PLAYER FOR THE NIL NEWTS
;;;USES A GENETIC ALGORITHM WITH LOCAL SEARCH BASED ON:
;;;Berghman, L., Dries, G., Leus, R. (2009). Efficient Solutions for Mastermind Using Genetic Algorithms.
;;;Proceedings of the 1999 ACM Symposium on Applied Computing, SAC'99, 307-311.
;;;Oijen, V. (2018). Genetic Algorithms Playing Mastermind. Utrecht University Bachelor Thesis, Netherlands.
;;;*********************************************************************************************************

;;;*********************************************************************************************************
;;;Global Variables & Parameters
;;;*********************************************************************************************************

(defvar *population-size* 30) ;start with 30 as a baseline, see pg 21 of Oijen

;list containing all guesses made in sequential order
(defvar *guess-history* (list))

;list containing all responses to guesses made by the player
(defvar *response-history* (list))

(defparameter *fitness-alpha* 1)

(defparameter *fitness-beta* 1)

;;;*********************************************************************************************************
;;;Helper Functions (1) - General
;;;*********************************************************************************************************

;;copy constructor for game class
(defmethod copy-game ((self game))
  (make-instance 'game :board (board self) :colors (colors self) :number-of-colors (number-of-colors self) :answer (answer self) :SCSA (SCSA self) :guesses (guesses self) :game-cutoff (game-cutoff self)))

;;;*********************************************************************************************************
;;;Helper Functions (2a) - GA - Initial Population
;;;*********************************************************************************************************

;;make initial random population for GA
(defun make-initial-population (code-length colors)
  (loop for i from 1 to *population-size*
     collect (insert-colors code-length colors) into result
     finally (return result)))

;;;*********************************************************************************************************
;;;Helper Functions (2b) - GA - Crossover
;;;*********************************************************************************************************

;;main crossover function
;;parents is a list of two guesses
;;return a list of two new child guesses after one or two point crossover
(defun crossover (parents)
  (let ((crossover-type (+ 1 (random 2))))  ;choose 1 or 2, 1/2 probability of choosing one or two point crossover
    (if (= crossover-type 1)
	(one-point-crossover parents)
	(two-point-crossover parents))))

;;one point crossover helper function, splits parent lists at one point and recombines into two child lists
(defun one-point-crossover (parents)
  (let ((child1 (list))
	(child2 (list)))
    (loop with n = (length (first parents))
	  with index = (+ 1 (random (- n 1)))
	  for i from 0 to (- n 1)
	  when (< i index)
	    do (setf child1 (cons (nth i (first parents)) child1))
	    and do (setf child2 (cons (nth i (second parents)) child2))
	  else when (>= i index)
		 do (setf child1 (cons (nth i (second parents)) child1))
		 and do (setf child2 (cons (nth i (first parents)) child2))
	  finally (return (list (reverse child1) (reverse child2))))))

;;two point crossover helper function, splits parent lists at two points and recombines into two child lists
(defun two-point-crossover (parents)
  (let ((child1 (list))
	(child2 (list)))
    (loop with n = (length (first parents))
	  with index1 = (+ 1 (random (- n 2)))
	  with index2 = (+ (+ 1 index1) (random (- (- n 1) index1)))
	  for i from 0 to (- n 1)
	  when (< i index1)
	    do (setf child1 (cons (nth i (first parents)) child1))
	    and do (setf child2 (cons (nth i (second parents)) child2))
	  else when (< i index2)
		 do (setf child1 (cons (nth i (second parents)) child1))
		 and do (setf child2 (cons (nth i (first parents)) child2))
	  else when (>= i index2)
		 do (setf child1 (cons (nth i (first parents)) child1))
		 and do (setf child2 (cons (nth i (second parents)) child2))
       finally (return (list (reverse child1) (reverse child2))))))

;;;*********************************************************************************************************
;;;Helper Functions (2c) - GA - Local Search
;;;*********************************************************************************************************


;; Hill climbing local search
;; Based on local-search algorithm from Oijen
;; and hill-climbing algorithm from Russell, J., Norvig, P. "Artificial Intelligence: A Modern Approach"
(defun local-search (board colors child)
  (loop with current = (copy-list child)
	with neighbor = (copy-list child)
	for peg from 0 to (1- (length child))
	do (setf neighbor (best-successor board colors neighbor peg))
	do (when (equal neighbor current)
	     (return current))
	do (setf current neighbor)
	finally (return current)))

;; finds local optima of all color changes for a single peg
;; returns best of successor states or child if child is already local optima
(defun best-successor (board colors child peg)
  (loop with best-code = (copy-list child)
	with best-fitness = (fitness board best-code)
	with current-code = (copy-list child)
	with current-fitness = (fitness board current-code)
	for color in colors
	do (setf (nth peg current-code) color)
	do (setf current-fitness (fitness board current-code))
	do (when (> current-fitness best-fitness)
	     (setf best-code (copy-list current-code))
	     (setf best-fitness (fitness board best-code)))
	finally (return best-code)))

;; fitness function for local algorithm
;; based on fitness function from Berghman
(defun fitness (board c)
  (let ((a *fitness-alpha*)
        (b *fitness-beta*)
        (game-copy (copy-game *Mastermind*))
        (N (1- (length *guess-history*))))
    (setf (answer game-copy) c)
    (loop for i from 0 to N
       with guess         
       with mock-response
       with response
       do (setf guess (nth i *guess-history*))
       do (setf mock-response (process-guess game-copy guess))
       do (setf response (nth i *response-history*))
       sum (abs (- (first mock-response) (first response))) into sum-x
       sum (abs (- (second mock-response) (second response))) into sum-y
       finally (return (+ (* a sum-x) sum-y (* b board N))))))         

;;;*********************************************************************************************************
;;;Helper Functions (2d) - GA - Family Competition
;;;*********************************************************************************************************
	     
;;codes is a list of generated codes
;;returns a sequence of (similarity code) over each code in codes, where similarity is as defined in Berghman et al
(defun similarity-scores (codes)
  (let ((result (make-sequence 'list (length codes)))
        (game-copy)        
        (N (1- (length codes))))    
    (loop for i from 0 to N
       for c = (nth i codes)         
       do (setf game-copy nil)         
       do (loop for j from 0 to N	     
	   for c* = (nth j codes)	     
	   when (not game-copy) ;entering loop for the first time	     
	   do (setf game-copy (copy-game *Mastermind*))	     
	   and do (setf (answer game-copy) c*)	     
	   when (/= i j)	     
	   do (setf (nth j result)		  
		  (list (+ (nth j result) (apply '+ (process-guess game-copy c))) c)))
       finally (return result))))

;;returns T when code is eligible as defined in Berghman et al. Returns nil otherwise
(defun eligiblep (code)
  (let ((N (1- (length *guess-history*))))
    (loop for i from 0 to N
       with guess ;actual guess made
       with response ;actual response to guess
       with mock-response
       with game-copy = (copy-game *Mastermind*)
       when (null mock-response)
       do (setf (answer game-copy) code) ;set the answer for the copy of *Mastermind*
       do (setf guess (nth i *guess-history*))
       do (setf response (nth i *response-history*))
       do (setf mock-response (process-guess game-copy guess))
       when (not (equal response mock-response))
       do (return nil)
       finally (return T))))

;;make a family from parents using crossover and local search
;;parents is a list of two codes
(defun nuclear-family (board colors parents)
  (let* ((children (crossover parents))
         (modified-children (local-search board colors children))
         (family (append parents modified-children)))
    family))

;;will describe this later
(defun pairwise-indices (n)
  (let* ((result (multiple-value-bind (f r) (floor n 2) (list f r)))
         (N (first result)))
    (loop for i from 0 to (1- N)
       collect (list (* 2 i) (1+ (* 2 i))) into result
       finally (return result))))

;;makes a new generation from old-gen
;;Error when running the following: (make-new-generation 3 '(a b c) '((a b c) (a a a) (b b b))) with population size of 3
;;Evaluation aborted on #<SB-INT:INVALID-ARRAY-INDEX-ERROR expected-type: (INTEGER 0 (3)) datum: NIL>
(defun make-new-generation (board colors old-gen)
  (let* ((parent-indices (pairwise-indices *population-size*)))
    (cond ((oddp *population-size*)
	 (setf parent-indices (append parent-indices (list (1- *population-size*) -1))))) ;-1 is a dummy value
    (loop for (idx1 idx2) in parent-indices
       with parent1       
       with parent2       
       with parents       
       with family       
       with family-seq ;sequence of format (fitness-of-member member)
       with new-gen         
       do (setf parent1 (nth idx1 old-gen))
       when (= idx2 -1)
       do (setf new-gen (append new-gen (list parent1))) ;just tack on the last member
       else
       do (setf family-seq (make-sequence 'list 4)) ;reset family-seq 	       (setf parent2 (nth idx2 old-gen))        
       and do (setf parents (list parent1 parent2))        
       and do (setf family (nuclear-family board colors parents))
       and do (setf new-gen (append new-gen (n-most-fit board 2 family)))
       finally (return new-gen))))

;;returns a list of the n most fit members of gen
;;n must be <= the length of gen
(defun n-most-fit (board n gen)
  (let* ((M (1- (length gen)))
        (gen-seq (make-sequence 'list (length gen))))
    (loop for i from 0 to M
       with member
       do (setf member (nth i gen))         
       do (setf (nth i gen-seq) (list (fitness board member) member)))
    (stable-sort gen-seq #'> :key #'first)
    (loop for i from 0 to (1- n)
       collect (second (nth i gen-seq)) into result
       finally (return result))))         

;;;*********************************************************************************************************
;;; Player
;;;*********************************************************************************************************

;;all helper functions are good to go except make-new-generation. see the comment above the function for the error.
;;haven't tested below :')
(defun nilNewts (board colors SCSA last-response)
  (declare (ignore SCSA))
  (let ((result)
        (similarities)
        (pass) ;boolean
        (next-guess))
    (cond ((null last-response) ;first round
	 (setf *response-history* (list))
	 (setf *guess-history* (list))) ;reset histories
	(T (append *response-history* (list last-response)))) ;update response history	   
    (loop while (not pass)
       with old-gen = (make-initial-population board colors)
       with new-gen
       with new-gen-seq = (make-sequence 'list 0)
       with eligible
       with max-fitness
       with min-fitness
       with new-max-fitness
       with new-min-fitness
       with unchanged-count = 0
       do (setf result (make-new-generation board colors old-gen))
       do (setf new-gen (first result))       
       do (setf new-gen-seq (make-sequence 'list 0)) ;reset new-gen-seq
       do (loop for member in new-gen ;populate new-gen-seq
	   do (append new-gen-seq (list (fitness board member) member))
	   when (eligiblep member)
	   do (cons member eligible)) ;end of populate new-gen-seq
       do (stable-sort new-gen-seq #'> :key #'first) ;sort new-gen-seq by descending fitness
       do (setf new-max-fitness (first (first new-gen-seq)))
       do (setf new-min-fitness (first (first new-gen-seq)))
       when (and (> new-max-fitness max-fitness) (< new-min-fitness min-fitness))
       do (incf unchanged-count)
       else do (setf unchanged-count 0)         
       when (> new-max-fitness max-fitness)
       do (setf max-fitness new-max-fitness)
       when (< new-min-fitness min-fitness)
       do (setf min-fitness new-min-fitness)
       when (= unchanged-count 5)
       do (setf pass T)
       do (setf old-gen new-gen)
       do (setf new-gen nil)) ;end of generation making while-loop
    do (setf similarities (similarity-scores eligible)) ;sort by descending similarity scores
    do (stable-sort similarities #'> :key #'first)
    (setf next-guess (second (first similarities)))
    (append *guess-history* (list next-guess))
    next-guess))
         
	     
