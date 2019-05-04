;;;THIS FILE IS THE MASTERMIND PLAYER FOR THE NIL NEWTS
;;;USES A GENETIC ALGORITHM WITH LOCAL SEARCH BASED ON:
;;;Berghman, L., Dries, G., Leus, R. (2009). Efficient Solutions for Mastermind Using Genetic Algorithms.
;;;Proceedings of the 1999 ACM Symposium on Applied Computing, SAC'99, 307-311.
;;;Oijen, V. (2018). Genetic Algorithms Playing Mastermind. Utrecht University Bachelor Thesis, Netherlands.

(defvar *guesses* (list))

(defvar *total-guesses* 0)

(defvar *responses* (list))

(defvar *population-size* 0)

(defvar *fitness-a* 0)

(defvar *fitness-b* 0)

;;;******************************************************************************
;;; Helper Functions (1) - General
;;;******************************************************************************

;; copy constructor for game class
(defmethod copy-game ((self game))
  (make-instance 'game :board (board self) :colors (colors self) :number-of-colors (number-of-colors self) :answer (answer self) :SCSA (SCSA self) :guesses (guesses self) :game-cutoff (game-cutoff self)))

(defun set-population-size (n)
  (setf *population-size* n))

(defun set-fitness-a (a)
  (setf *fitness-a* a))

(defun set-fitness-b (b)
  (setf *fitness-b* b))

(defun reset-history ()
  (setf *responses* (list))
  (setf *guesses* (list)))

(defun update-responses (full-response)
  (let ((bullscows (list (subseq full-response 0 2))))
    (setf *responses* (append *responses* bullscows))))

(defun update-guesses (guess)
  (setf *guesses* (append *guesses* (list guess))))

;;;******************************************************************************
;;; Helper Functions (2a) - Initialize GA
;;;******************************************************************************

;; make random initial population
(defun make-initial-population (board colors)
  (loop for i from 1 to *population-size*
     collect (insert-colors board colors) into initial
     finally (return initial)))

;; make initial guess
(defun make-initial-guess (board colors)
  (halfsies board colors))

;; starts with an empty code. populates the remaining half with increasing colors until the code is of length board
;; ex) (halfsies 5 '(a b c)) -> (a a a b c)
(defun halfsies (board colors)
  (let ((initial))
    (loop while (< (length initial) board)
       with half
       with remainder
       with n = 0
       with color = (nth n colors)
       do (setf remainder (- board (length initial)))
       do (cond ((evenp remainder) (setf half (/ remainder 2)))
	      (T (setf half (/ (1+ remainder) 2))))
       do (loop for i from 1 to half
	   do (setf initial (cons color initial))
	   finally (cond ((< n (1- (length colors))) (incf n) (setf color (nth n colors))))))
    (reverse initial)))

;;;******************************************************************************
;;; Helper Functions (2b) - GA crossover
;;;******************************************************************************

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

;;;******************************************************************************
;;; Helper Functions (2c) - GA fitness heuristic
;;;******************************************************************************

;; returns the fitness value of a code c
(defun fitness (c)
  (let ((board (length c))
        (game-copy (copy-game *Mastermind*))
        (i (length *guesses*)))
    (setf (answer game-copy) c)
    (loop for guess in *guesses*
       for response in *responses*
       with response-prime
       do (setf response-prime (process-guess game-copy guess))
       sum (abs (- (first response-prime) (first response))) into sum-x
       sum (abs (- (second response-prime) (second response))) into sum-y
       finally (return (+ (* sum-x *fitness-a*) sum-y (* board (1- i) *fitness-b*)))
         )))

;; returns the member of codes with the lowest fitness value
(defun least-fit (codes)
    (loop for c in codes
       with best = (first codes)
       with best-fitness = (fitness best)
       with current-fitness
       do (setf current-fitness (fitness c))
       when (< current-fitness best-fitness)
       do (setf best (copy-list c))
       and do (setf best-fitness current-fitness)
       finally (return best)))

;; returns the memebr of codes with the highest fitness value
(defun most-fit (codes)
  (loop for c in codes
     with best = (first codes)
     with best-fitness = (fitness best)
     with current-fitness
     do (setf current-fitness (fitness c))
     when (> current-fitness best-fitness)
     do (setf best (copy-list c))
     and do (setf best-fitness current-fitness)
     finally (return best)))

;; returns the 2 members of codes with the lowest fitness values
;; ***depends on slick-value = 0
(defun 2-least-fit (codes)
  (let* ((best (first codes))
         (best-fitness (fitness best))
         (next (second codes))
         (next-fitness (fitness next))
         (board (length best))
         (slick-value (* board (1- (length *guesses*)) *fitness-b*))
         (dummy))
    (cond ((> best-fitness next-fitness)
	 (setf dummy best) ;store old value of best
	 (setf best next)
	 (setf next dummy)
	 (setf dummy best-fitness) ;store old value of best-fitness
	 (setf best-fitness next-fitness)
	 (setf next-fitness best-fitness)))
    (loop for c in codes
       with current-fitness
       do (setf current-fitness (fitness c))
       when (< current-fitness best-fitness) ;c is better than best
       do (setf dummy best)
       and do (setf best c) ;update best
       and do (setf next dummy) ;update next to old best
       and do (setf dummy best-fitness)
       and do (setf best-fitness current-fitness) ;update best-fitness
       and do (setf next-fitness dummy) ;update next-fitness to old best-fitness
       when (and (>= current-fitness best-fitness)
	       (not (equal best c))
	       (< current-fitness next-fitness)) ;c worse than best but better than next
       do (setf next c) ;update next
       and do (setf next-fitness current-fitness) ;update next-fitness
       do (cond ((and (= best-fitness slick-value) (= next-fitness slick-value))
	       (return (list best next)))) ;return early if best two are already found
       finally (return (list best next)))))

;; makes a sequences with elements ((fitness c) c) for each c in codes
(defun fitness-sequence-from-list (codes)
  (let ((L (length codes)))
    (loop for i from 0 to (1- L)
       with fitness-seq = (make-sequence 'list L)
       with code
       do (setf code (nth i codes))
       do (setf (nth i fitness-seq) (list (fitness code) code))
       finally (return fitness-seq))))

;; codes is a list of codes. does not have to be distinct
;; general n-least-fit function for testing. More efficient to use least-fit, 2-least-fit, or write a function when n is a known value.
(defun n-least-fit (n codes)
  (let ((fitness-seq (fitness-sequence-from-list codes)))
    (setf fitness-seq (stable-sort fitness-seq #'< :key #'first))
    (loop for i from 0 to (1- n)
       collect (second (nth i fitness-seq)) into result
       finally (return result))))

;;;******************************************************************************
;;; Helper Functions (2d) - GA local search
;;;******************************************************************************

;; Hill climbing local search
(defun local-search (colors child)
  (let ((N (1- (length child)))) ;board-1
    (loop for peg from 0 to N
       with current = (copy-list child)
       with neighbor = (copy-list child)
       do (setf neighbor (best-successor colors neighbor peg))
       when (equal neighbor current)
       do (return current)
       do (setf current (copy-list neighbor))
       finally (return current))))

;; returns the local optima amongst successors of child which differ by a single peg
(defun best-successor (colors child peg)
  (let ((successors))
    (loop for color in colors
       with successor
       do (setf successor (copy-list child))
       do (setf (nth peg successor) color)
       collect successor into temp
       finally (setf successors temp))
    (least-fit successors)))

;;;******************************************************************************
;;; Helper Functions (2e) - GA making a new generation
;;;******************************************************************************

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

;;make a family consisting of two parents and two children
;;parents is a list of two codes
(defun nuclear-family (colors parents)
  (let* ((children (crossover parents))
         (modified-children (list (local-search colors (first children))
				  (local-search colors (second children))))
         (family (append parents modified-children)))
    family))

;;makes a new generation from the previous generation using crossover and local search
(defun make-new-generation (colors prev-gen)
  (let* ((parent-indices (make-parent-indices)))
    (loop for (idx1 idx2) in parent-indices
       with parent1       
       with parent2       
       with parents       
       with family
       with new-gen
       with dummy
       do (setf parent1 (nth idx1 prev-gen))
       when (= idx2 -1) ;see comment above make-parent-indices
       do (setf new-gen (cons (local-search colors parent1) new-gen)) ;last child is direct copy of parent1 with only local-search performed
       else do (setf parent2 (nth idx2 prev-gen))       
       and do (setf parents (list parent1 parent2))
       and do (setf family (nuclear-family colors parents))
       and do (setf dummy (2-least-fit family))
       and do (setf new-gen (cons (first dummy) new-gen))
       and do (setf new-gen (cons (second dummy) new-gen))
       finally (return new-gen))))

;;;******************************************************************************
;;; Helper Functions (2f) - GA family competition
;;;******************************************************************************

;; returns the similarity score of the ith code in codes
(defun similarity-score (i codes)
  (let* ((game-copy (copy-game *Mastermind*))
         (c (nth i codes))
         (N (1- (length codes))))
    (setf (answer game-copy) c)
    (loop for j from 0 to N
       for c* = (nth j codes)
       when (/= i j)
       sum (apply '+ (process-guess game-copy c*)) into similarity
       finally (return similarity))))

;; direction is the symbol '< or '>. '< yields least similar, '> yields most similar
;; returns the code which is most or least similar to the other codes in codes.
(defun similarity-extrema (codes direction)
  (let ((N (1- (length codes))))
    (loop for i from 0 to N
       for current = (nth i codes)
       with best = (first codes) ;initialize to first code
       with best-similarity = (similarity-score i codes)
       with current-similarity
       do (setf current-similarity (similarity-score i codes))
       when (equal direction '>) ;find maxima
       do (cond ((> current-similarity best-similarity)
	       (setf best current)
	       (setf best-similarity current-similarity)))
       else ;find minima
       do (cond ((< current-similarity best-similarity)
	       (setf best current)
	       (setf best-similarity current-similarity)))
       finally (return best))))

;; returns T if c is eligible
(defun eligiblep (c)
  (let ((game-copy (copy-game *Mastermind*)))
    (setf (answer game-copy) c)
    (loop for guess in *guesses*
       for response in *responses*
       with response-prime
       do (setf response-prime (process-guess game-copy guess))
       when (not (equal response response-prime))
       do (return nil)
       finally (return T))))

;;;******************************************************************************
;;; Players to be called by nilNewts
;;;******************************************************************************

(defun GA-Player (board colors SCSA)
  (declare (ignore SCSA))
  (let* ((pass) ;exit condition
         (return-list)
         (loop-count 0))
    (loop while (not pass) ;make next guess using the Berghman GA
       with prev-gen = (make-initial-population board colors)
       with new-gen
       with max-fitness
       with min-fitness
       with current-min-fitness
       with current-max-fitness
       with unchanged-count = 0
       do (incf loop-count)
       do (setf new-gen (make-new-generation colors prev-gen)) ;make the new generation
       when (= loop-count 1) ;initialize values for max and min fitness
       do (setf max-fitness (fitness (most-fit new-gen)))
       and do (setf min-fitness (fitness (least-fit new-gen)))
       when (> loop-count 1)
       do (setf current-max-fitness (fitness (most-fit new-gen))) ;update current max fitness
       and do (setf current-min-fitness (fitness (least-fit new-gen))) ;update current min fitness
       and do (cond ((> current-max-fitness max-fitness)
		 (setf max-fitness current-max-fitness)
		 (setf unchanged-count 0))
		(T (incf unchanged-count)))
       and do (cond ((< current-min-fitness min-fitness)
		 (setf min-fitness current-min-fitness)
		 (setf unchanged-count 0))
		(T (incf unchanged-count)))

       ;; when (= unchanged-count 5)
       ;; do (setf pass T)
       when (= loop-count 10) ;exit condition--needs to be investigated
       do (setf pass T)

       do (setf prev-gen new-gen)
       finally (setf return-list new-gen)) ;keep the last population
    (least-fit return-list)))

;;;******************************************************************************
;;; Metric Functions
;;;******************************************************************************

(defun reset-total-guesses ()
  (setf *total-guesses* 0))

(defun update-total-guesses ()
  (setf *total-guesses* (+ *total-guesses* (length *guesses*))))

(defun statistics (N P SCSA num-games)
  (let ((start-time (get-internal-run-time))
        (end-time)
        (run-time)
        (avg-guesses))
    (reset-total-guesses) ;reset global var
    (Mastermind N P SCSA)
    (play-tournament *Mastermind* 'nilNewts SCSA num-games)
    (setf end-time (get-internal-run-time))
    (setf run-time (/ (float (- end-time start-time)) 10))
    (setf avg-guesses (/ (float *total-guesses*) num-games))
    (format t "~%Total run-time: ~a" run-time)
    (format t "~%Run-time per round: ~a" (/ run-time num-games))
    (format t "~%Average guesses: ~a" avg-guesses)))

;;;******************************************************************************
;;; nilNewts
;;;******************************************************************************

(defun set-values ()
  (set-fitness-a 1)
  (set-fitness-b 2)
  (set-population-size 50))

(defun nilNewts (board colors SCSA last-response)
  (let ((next))
    (cond ((null last-response) ;first round
	 (update-total-guesses)
	 (reset-history)
	 (set-values)
	 (setf next (make-initial-guess board colors)))
	(T (update-responses last-response)
	   (setf next (GA-Player board colors SCSA))))
    (update-guesses next)
    next))
