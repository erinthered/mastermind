;;;*********************************************************************************************************
;;;THIS FILE IS THE MASTERMIND PLAYER FOR THE NIL NEWTS
;;;USES A GENETIC ALGORITHM WITH LOCAL SEARCH BASED ON:
;;;Berghman, L., Dries, G., Leus, R. (2009). Efficient Solutions for Mastermind Using Genetic Algorithms.
;;;Proceedings of the 1999 ACM Symposium on Applied Computing, SAC'99, 307-311.
;;;Oijen, V. (2018). Genetic Algorithms Playing Mastermind. Utrecht University Bachelor Thesis, Netherlands.
;;;*********************************************************************************************************

;;;*********************************************************************************************************
;;;Global Variables
;;;*********************************************************************************************************

;list containing all guesses made in sequential order
(defvar *guess-history* (list))

;list containing all responses to guesses made by the player
(defvar *response-history* (list))

;;;*********************************************************************************************************
;;;Helper Functions
;;;*********************************************************************************************************

;;copy constructor for game class
(defmethod copy-game ((self game))
  (make-instance 'game :board (board self) :colors (colors self) :number-of-colors (number-of-colors self) :answer (answer self) :SCSA (SCSA self) :guesses (guesses self) :game-cutoff (game-cutoff self)))

;;make initial population for GA
(defun make-initial-population (code-length colors)
  (loop for i from 1 to *population-size*
     collect (insert-colors code-length colors) into result
     finally (return result)))

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
     with game-copy = (setf game-copy (copy-game *Mastermind*))
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
(defun nuclear-family (parents)
  (let* ((children (crossover parents))
         (modified-children (local-search children))
         (family (append parents modified-children)))
    (return family)))

;;makes a new generation from old-gen
;;adds eligible members of the new-gen to a new list eligible
;;returns a list (new-gen eligible)
(defun make-new-generation (old-gen)
  (loop for i from 0 to (1- *population-size*)
     with parent1       
     with parent2       
     with parents       
     with family       
     with family-seq ;sequence of format (fitness-of-member member)
     with new-gen
     with eligible
     do (setf family-seq (make-sequence 'list 4)) ;reset family-seq       
     do (setf parent1 (nth (* 2 i) old-gen))       
     do (setf parent2 (nth (1+ (* 2 i)) old-gen))       
     do (setf parents (list parent1 parent2))       
     do (setf family (nuclear-family parents))
     do (append new-gen (n-most-fit 2 family))       
     finally (return (list new-gen eligible))))

;;returns a list of the n most fit members of gen
;;n must be <= the length of gen
(defun n-most-fit (n gen)
  (let ((N (1- (length gen)))
        (gen-seq (make-sequence 'list 0)))
    (loop for i from 0 to N
       with member
       do (setf member (nth n gen))         
       do (append gen-seq (list (fitness-function member) member)))
    do (sort gen-seq #'> :key #'first)
    (loop for i from 0 to n
       collect (second (nth i gen-seq)) into result
       finally (return result))))         
      

;; local-search performed on the list of codes 
;; modifies each child one peg at a time until a local optima is reached
;; colors contains all the permissible colors in the game  
;; input-codes contains the population to be checked
;; optimal-codes should be passed as an empty list and will be returned as an optimal list
(defun local-search (colors input-codes)
	(let ((optimal-codes '())
				(code (first input-codes))
				(best-fitness most-negative-fixnum))

	(cond ((endp input-codes) (return-from local-search optimal-codes)))	;; base case
	;; (setf best-fitness 1)
	(loop for pegs from 0 to (- (length code) 1)
		do (loop for color from 0 to (- (length colors) 1)
            ;; do (format t "~%Optimal Code at beginning: ~a ~%" optimal-codes)
            ;; do (format t "Code at beginning: ~a ~%" code)
            do (setf new-code code)
            do (setf (nth pegs new-code) (nth color colors))
            do (setf current-fitness (fitness-function new-code))
            do (cond ((> current-fitness best-fitness)
                (setf optimal-codes (list new-code)) ;; remove append from this line
                ;; (format t "Optimal Code: ~a" optimal-codes)
                (terpri)(terpri)
                (setf best-fitness current-fitness))
                ;; (t (format t "Cond Skipped")))))
    (setf optimal-codes (append optimal-codes (local-search colors (rest input-codes))))
    ;; (format t "optimal-code ~a ~%" optimal-codes)
    (return-from local-search optimal-codes)))	

;; dummy fitness function
;; will implement the fitness function and return the fitness value for list codes
(defun fitness-function (codes)
	(return-from fitness-function 0))


;;;*********************************************************************************************************
;;;Player
;;;*********************************************************************************************************

(defvar *population-size* 30) ;start with 30 as a baseline, see pg 21 of Oijen

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
       do (setf result (make-new-generation old-gen))
       do (setf new-gen (first result))
       do (setf eligible (second result))         
       do (setf new-gen-seq (make-sequence 'list 0)) ;reset new-gen-seq
       do (loop for member in new-gen
	   do (append new-gen-seq (list (fitness-function member) member)
	   when (eligiblep member)
	   do (cons member eligible))) ;end of make new-gen loop
       do (sort new-gen-seq #'> :key #'first) ;sort new-gen-seq by descending fitness
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
    do (sort similarities #'> :key #'first)
    (setf next-guess (second (first similarities)))
    (append *guess-history* (list next-guess))
    next-guess))
         
	     
