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

(defvar *population-size* 50) ;start with 30 as a baseline, see pg 21 of Oijen

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

;;make initial guess for GA
(defun make-initial-guess (board)
  (let ((l1)
	(l2)
	(initial))
    (cond ((oddp board)
	   (setf l1 (/ (1- board) 2))
	   (setf l2 (1+ l1)))
	  (T (setf l1 (/ board 2))
	     (setf l2 l1)))
    (setf initial (make-list l2 :initial-element 'b))
    (loop for i from 1 to l1
	  do (setf initial (cons 'a initial)))
    initial))

;;make initial random population for GA
(defun make-initial-population (board colors)
  (loop for i from 1 to *population-size*
	collect (insert-colors board colors) into result
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
	do (when (< current-fitness best-fitness) ;flipped the inequality
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
  (let ((result)
        (game-copy)        
        (N (1- (length codes))))
    (if (> (length codes) 1)
        (setf result (make-sequence 'list (length codes)))
        (setf result (make-sequence 'list 1 :initial-element (list 0 (first codes)))))
    (loop for i from 0 to N
	  with similarity-score
	  for c = (nth i codes)
	  do (setf similarity-score 0)
	  do (setf game-copy nil)         
	  do (loop for j from 0 to N	     
		   for c* = (nth j codes)	     
		   when (not game-copy) ;entering loop for the first time	     
		     do (setf game-copy (copy-game *Mastermind*))	     
		     and do (setf (answer game-copy) c*)	     
		   when (/= i j)
		     sum (apply '+ (process-guess game-copy c)) into similarity-score)
	  do (setf (nth i result) (list similarity-score  c))
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
         (modified-children (list (local-search board colors (first children))
				  (local-search board colors (second children))))
         (family (append parents modified-children)))
    family))

;;makes a list of 2-tuples of pairwise disjoint indices for parents in order to yield n children
;;for even numbers we split the list (0 ... n-1) exactly
;;for odd numbers we split the list (0 ... n-2) exactly. The last pair is (n-1 -1), where -1 is to be taken as a dummy variable meaning "for the last child, just clone the n-1th parent"
;;ex) n=4; (make-parent-indices n) -> ((0 1) (2 3))
(defun make-parent-indices ()
  (let* ((n *population-size*)
         (result (multiple-value-bind (f r) (floor n 2) (list f r)))
         (M (first result)))
    (loop for i from 0 to (1- M)
	  collect (list (* 2 i) (1+ (* 2 i))) into parent-indices
	  finally (cond ((oddp n)
			 (setf parent-indices (cons (list (1- n) -1) parent-indices))	 
			 (return parent-indices))
			(T (return parent-indices))))))

;;makes a new generation from old-gen
(defun make-new-generation (board colors old-gen)
  (let* ((parent-indices (make-parent-indices)))
    (loop for (idx1 idx2) in parent-indices
	  with parent1       
	  with parent2       
	  with parents       
	  with family
	  with new-gen
          with result
	  do (setf parent1 (nth idx1 old-gen))
	  when (= idx2 -1)
	    do (setf new-gen (cons parent1 new-gen)) ;just tack on the last member
	  else
	    do (setf parent2 (nth idx2 old-gen))         
	    and do (setf parents (list parent1 parent2))
	    and do (setf family (nuclear-family board colors parents))
	    and do (setf result (n-most-fit board 2 family))
	    and do (setf new-gen (cons (first result) new-gen))
	    and do (setf new-gen (cons (second result) new-gen))
	  finally (return new-gen))))

(defun make-fitness-sequence-from-codes (board codes)
  (let ((L (length codes)))
    (loop for i from 0 to (1- L)
	  with copy-seq = (make-sequence 'list L)
	  with code
	  do (setf code (nth i codes))
	  do (setf (nth i copy-seq) (list (fitness board code) code))
	  finally (return copy-seq))))

;;returns a list of the n most fit members of gen
;;n must be <= the length of gen
(defun n-most-fit (board n gen)
  (let* ((gen-seq (make-fitness-sequence-from-codes board gen)))
    (setf gen-seq (stable-sort gen-seq #'< :key #'first))
    (loop for i from 0 to (1- n)
	  collect (second (nth i gen-seq)) into result
	  finally (return result))))         

;;;*********************************************************************************************************
;;; Player
;;;*********************************************************************************************************

(defun GA-Player (board colors)
  (let ((loop-count 0)
        (pass) ;boolean
	(return-list)
	(return-seq))
    (loop while (not pass) ;make next guess using the Berghman GA
	  with old-gen = (make-initial-population board colors)
	  with new-gen
	  with new-gen-seq = (make-sequence 'list *population-size* :initial-element (list))
	  with max-fitness
	  with min-fitness
	  with new-max-fitness
	  with new-min-fitness
	  with unchanged-count = 0	        
	  do (incf loop-count) ;increment loop counter
	  do (setf new-gen (make-new-generation board colors old-gen))       
	  do (setf new-gen-seq (make-sequence 'list *population-size*)) ;reset new-gen-seq
             
	  do (loop for i from 0 to (1- *population-size*) ;populate new-gen-seq
		   with member
		   do (setf member (nth i new-gen))
		   do (setf (nth i new-gen-seq) (list (fitness board member) member))) ;end of populate new-gen-seq
	  do (setf new-gen-seq (stable-sort new-gen-seq #'< :key #'first)) ;sort new-gen-seq by descending fitness
	  do (setf new-min-fitness (first (first new-gen-seq)))
	  do (setf new-max-fitness (first (first (last new-gen-seq))))

	  when (= loop-count 1)
	    do (setf max-fitness new-max-fitness) ;initialize values
	    and do (setf min-fitness new-min-fitness)		   
	  when (and (> loop-count 1)
		    (= new-max-fitness max-fitness) (= new-min-fitness min-fitness)) ;increment unchanged-count
	    do (setf unchanged-count (1+ unchanged-count))
	  else do (setf unchanged-count 0)		  
	  when (and (> loop-count 1) (> new-max-fitness max-fitness)) ;new max
	    do (setf max-fitness new-max-fitness)               
	  when (and (> loop-count 1) (< new-min-fitness min-fitness)) ;new min
	    do (setf min-fitness new-min-fitness)         
	  when (= loop-count 10) ;exit condition
	    do (setf pass T)
               
	  do (setf old-gen new-gen)
	  do (setf return-list new-gen)
	  do (setf new-gen nil))
    (setf return-seq (make-fitness-sequence-from-codes board return-list))
    (setf return-seq (stable-sort return-seq #'< :key #'first))
    (second (first return-seq))))


(defun nilNewts (board colors SCSA last-response)
  (declare (ignore SCSA))
  (let* ((next-guess))
    (cond ((null last-response) ;first round
	   (setf *response-history* (list))
	   (setf *guess-history* (list)) ;reset histories
	   (setf next-guess (make-initial-guess board))) ;make initial guess
	  (T (setf *response-history* (append *response-history* (list (subseq last-response 0 2)))) ;update response history
	     (setf next-guess (GA-Player board colors))))
    (setf *guess-history* (append *guess-history* (list next-guess))) ;update guess history
    ;;(print next-guess)
    next-guess))

(defun find-run-time (n p)
  (let ((start-time (get-internal-run-time)))
    (Mastermind n p 'insert-colors)
    (play-tournament *Mastermind* 'nilNewts 'insert-colors 100)
    (- (get-internal-run-time) start-time)))

