;;;THIS FILE IS THE MASTERMIND PLAYER FOR THE NIL NEWTS
;;;USES A GENETIC ALGORITHM WITH LOCAL SEARCH BASED ON:
;;;Berghman, L., Dries, G., Leus, R. (2009). Efficient Solutions for Mastermind Using Genetic Algorithms.
;;;Proceedings of the 1999 ACM Symposium on Applied Computing, SAC'99, 307-311.
;;;Oijen, V. (2018). Genetic Algorithms Playing Mastermind. Utrecht University Bachelor Thesis, Netherlands.

(defvar *guesses* (list))
(defvar *responses* (list))
(defvar *population-size* 0)
(defvar *fitness-a* 0)
(defvar *fitness-b* 0)

;;;*********************************************************************************************************
;;;Helper Functions (1a) - General
;;;*********************************************************************************************************

;;copy constructor for game class
(defmethod copy-game ((self game))
  (make-instance 'game :board (board self) :colors (colors self) :number-of-colors (number-of-colors self) :answer (answer self) :SCSA (SCSA self) :guesses (guesses self) :game-cutoff (game-cutoff self)))

(defun set-population-size (n)
  (setf *population-size* n))

(defun set-fitness-a (a)
  (setf *fitness-a* a))

(defun set-fitness-b (b)
  (setf *fitness-b* b))

;; set parameter values for the GA
(defun set-parameters (a b)
  (set-fitness-a a)
  (set-fitness-b b))

(defun update-responses (full-response)
  (let ((bullscows (list (subseq full-response 0 2))))
    (setf *responses* (append *responses* bullscows))))

(defun update-guesses (guess)
  (setf *guesses* (append *guesses* (list guess))))

(defun reset-history ()
  (setf *responses* (list))
  (setf *guesses* (list)))

;;returns T if the code has already been guessed
(defun guessedp (code)
  (cond ((member code *guesses* :test 'equal) T)
        (T nil)))

;;;*********************************************************************************************************
;;;Helper Functions (1b) - SCSAs
;;;*********************************************************************************************************

;; individual SCSA weight functions are real valued functions which return from the range [0,1]. 0 indicates a full match and 1 indicates a full mismatch.
;; the return value from the weight functions are shifted down one to increase preference towards mmatches, as the algorithm seeks to minimize the fitness value.
(defun scsa-weight (scsa code)
  (let ((w (cond ((equal scsa 'two-color) ;two color
	          (two-color-weight code))
		 ((equal scsa 'two-color-alternating) ;two-color-alternating
	          (two-color-alternating-weight code))
		 ((equal scsa 'only-once) ;only-once
	          (only-once-weight code))
		 ((equal scsa 'first-and-last) ;first-and-last
	          (first-and-last-weight code))
		 ((equal scsa 'usually-fewer) ;usually-fewer
	          (usually-fewer-weight code))
		 ((equal scsa 'prefer-fewer) ;prefer-fewer
	          (prefer-fewer-weight code))
		 ((equal scsa 'mystery-1) ;mystery-1
	          (mystery-1-weight code))
	       	 ((equal scsa 'mystery-2) ;mystery-2
	          (mystery-2-weight code))
		 ((equal scsa 'mystery-3) ;mystery-3
	          (mystery-3-weight code))
		 ((equal scsa 'mystery-4) ;mystery-4
	          (mystery-4-weight code))
		 ((equal scsa 'mystery-5) ;mystery-5
	          (two-color-alternating-weight code))
		 (T 0))))
    (1- w)
    ;w
    ))

(defun two-color-weight (code)
  (loop with color1 = (first code)
	with color2
	for i from 1 to (1- (length code))
	do (when (not (equal (nth i code) color1))
	     (if (equal color2 nil)
		 (setf color2 (nth i code))
		 (when (not (equal (nth i code) color2))
		   (return 1))))
	finally (return 0)))
       
(defun two-color-alternating-weight (code)
  (loop with color1 = (first code)
	with color2 = (second code)
	with alternating-list = (list)
	for i from 0 to (1- (length code))
	when (evenp i)
	  do (setf alternating-list (cons color1 alternating-list))
	else
	  do (setf alternating-list (cons color2 alternating-list))
	finally (if (equal code (reverse alternating-list))
		    (return 0)
		    (return 1))))

(defun only-once-weight (code)
  (loop with hash-counter = (make-hash-table)
	for letter in code
	when (not (gethash letter hash-counter))
	  do (setf (gethash letter hash-counter) 0)
	do (setf (gethash letter hash-counter) (1+ (gethash letter hash-counter)))
	when (> (gethash letter hash-counter) 1)
	  do (return 1)
	finally (return 0)))

(defun first-and-last-weight (code)
  (if (equal (first code) (first (last code))) 0 1))

;;choose 2 or 3 colors with p = 0.9
(defun usually-fewer-weight (code)
  (let* ((color-count (color-counter *Mastermind* code))
         (missing (count 0 color-count))
         (present (- (number-of-colors *Mastermind*) missing)))
    (cond ((or (= present 2) (= present 3)) 0.45)
	(T 0.1))))

;; choose 1 color with p = 0.49
;; choose 2 colors with p = 0.25
;; choose 3 colors with p = 0.13
;; choose 4 colors with p = 0.08
;; choose 5 colors with p = 0.03
;; choose 6 or more with p = 0.02
(defun prefer-fewer-weight (code)
  (let* ((color-count (color-counter *Mastermind* code)) ;array
         (present (count-if #'plusp color-count)))
    (cond ((= present 1) 0.49)
	((= present 2) 0.25)
	((= present 3) 0.13)
	((= present 4) 0.08)
	((= present 5) 0.03)
	(T 0.02))))

(defun mystery-1-weight (code)
  (let* ((color-count (color-counter *Mastermind* code)) ;array
         (present (count-if #'plusp color-count)))
    (case present
      (1 (- 1 (/ (float 89) 200)))
      (2 (- 1 (/ (float 91) 200)))
      (3 (- 1 (/ (float 6) 200)))
      (4 (- 1 (/ (float 9) 200)))
      (5 (- 1 (/ (float 5) 200)))
      (T 1)
      )))

(defun mystery-2-weight (code)
  (loop with color1 = (first code)
	with color2 = (second code)
	with color3 = (third code)
	with alternating-list = (list)
	for i from 0 to (1- (length code))
	if (equal 0 (mod i 3))
	  do (setf alternating-list (cons color1 alternating-list))
	else if (equal 1 (mod i 3))
	       do (setf alternating-list (cons color2 alternating-list))
	else
	  do (setf alternating-list (cons color3 alternating-list))
	finally (if (equal code (reverse alternating-list))
		    (return 0)
		    (return 1))))
  

(defun mystery-3-weight (code)
  (let* ((color-count (color-counter *Mastermind* code)) ;array
         (present (count-if #'plusp color-count)))
    (case present
      (3 0)
      (T 1))))

(defun mystery-4-weight (code)
  (let* ((color-count (color-counter *Mastermind* code)) ;array
         (present (count-if #'plusp color-count)))
    (case present
      (2 (- 1 (/ (float 7) 200)))
      (3 (- 1 (/ (float 88) 200)))
      (4 (- 1 (/ (float 105) 200)))
      (T 1)
      )))

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
(defun fitness (c SCSA)
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
       finally (return (+ (* sum-x *fitness-a*)
		      sum-y		      
		      (* board (1- i) *fitness-b*)
		      (scsa-weight c SCSA)
		      ))
         )))

;; returns the member of codes with the lowest fitness value
(defun least-fit (codes SCSA)
  (loop for c in codes
     with best = (first codes)
     with best-fitness = (fitness best SCSA)
     with current-fitness
     do (setf current-fitness (fitness c SCSA))
     when (< current-fitness best-fitness)
     do (setf best (copy-list c))
     and do (setf best-fitness current-fitness)
     finally (return best)))

;; returns the memebr of codes with the highest fitness value
(defun most-fit (codes SCSA)
  (loop for c in codes
     with best = (first codes)
     with best-fitness = (fitness best SCSA)
     with current-fitness
     do (setf current-fitness (fitness c SCSA))
     when (> current-fitness best-fitness)
     do (setf best (copy-list c))
     and do (setf best-fitness current-fitness)
     finally (return best)))

;; returns the 2 members of codes with the lowest fitness values
;; ***depends on slick-value = 0
(defun 2-least-fit (codes SCSA)
  (let* ((best (first codes))
         (best-fitness (fitness best SCSA))
         (next (second codes))
         (next-fitness (fitness next SCSA))
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
       do (setf current-fitness (fitness c SCSA))
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
(defun fitness-sequence-from-list (codes SCSA)
  (let ((L (length codes)))
    (loop for i from 0 to (1- L)
       with fitness-seq = (make-sequence 'list L)
       with code
       do (setf code (nth i codes))
       do (setf (nth i fitness-seq) (list (fitness code SCSA) code))
       finally (return fitness-seq))))

;;;******************************************************************************
;;; Helper Functions (2d) - GA local search
;;;******************************************************************************

;; Hill climbing local search
(defun local-search (colors child SCSA)
  (let ((N (1- (length child)))) ;board-1
    (loop for peg from 0 to N
       with current = (copy-list child)
       with neighbor = (copy-list child)
       do (setf neighbor (best-successor colors neighbor peg SCSA))
       when (equal neighbor current)
       do (return current)
       do (setf current (copy-list neighbor))
       finally (return current))))

;; returns the local optima amongst successors of child which differ by a single peg
(defun best-successor (colors child peg SCSA)
  (let ((successors))
    (loop for color in colors
       with successor
       do (setf successor (copy-list child))
       do (setf (nth peg successor) color)
       collect successor into temp
       finally (setf successors temp))
    (least-fit successors SCSA)))

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
(defun nuclear-family (colors parents SCSA)
  (let* ((children (crossover parents))
         (modified-children (list (local-search colors (first children) SCSA)
			    (local-search colors (second children) SCSA)))
         (family (append parents modified-children)))
    family))

;;makes a new generation from the previous generation using crossover and local search
(defun make-new-generation (colors prev-gen SCSA)
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
       do (setf new-gen (cons (local-search colors parent1 SCSA) new-gen)) ;last child is direct copy of parent1 with only local-search performed
       else do (setf parent2 (nth idx2 prev-gen))       
       and do (setf parents (list parent1 parent2))
       and do (setf family (nuclear-family colors parents SCSA))
       and do (setf dummy (2-least-fit family SCSA))
       and do (setf new-gen (cons (first dummy) new-gen))
       and do (setf new-gen (cons (second dummy) new-gen))
       finally (return new-gen))))

;;;******************************************************************************
;;; Helper Functions (2f) - GA family competition
;;;******************************************************************************

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
;;; Helper Functions (2g) - Decaying population size
;;;******************************************************************************

;; sigmoid decreasing function from max to max-step
(defun f1 (x max step)
  (let* ((g (/ (* -1 step x) (+ 1 (abs x))))
         (result (multiple-value-bind (f r) (floor g) (list f r)))
         (x* (first result)))
    (+ max x*)))

;; shifted sigmoid decreasing function
(defun f2 (x max step)
  (let* ((g (/ (* -1 step (- x 50)) (+ 1 (abs (- x 50)))))
         (result (multiple-value-bind (f r) (floor g) (list f r)))
         (x* (first result)))
    (+ max x*)))

;;;******************************************************************************
;;; Players to be called by nilNewts
;;;******************************************************************************

(defun GA-Player (board colors SCSA)
  (let* ((pass) ;exit condition
         (return-list)
         (loop-count 0)
         (result))
    (loop while (not pass) ;make next guess using the Berghman GA
       with prev-gen = (make-initial-population board colors)
       with new-gen
       with max-fitness
       with min-fitness
       with current-min-fitness
       with current-max-fitness
       with unchanged-count = 0
       do (incf loop-count)
       do (setf new-gen (make-new-generation colors prev-gen SCSA)) ;make the new generation
       when (= loop-count 1) ;initialize values for max and min fitness
       do (setf max-fitness (fitness (most-fit new-gen SCSA) SCSA))
       and do (setf min-fitness (fitness (least-fit new-gen SCSA) SCSA))
       when (> loop-count 1)
       do (setf current-max-fitness (fitness (most-fit new-gen SCSA) SCSA)) ;update current max fitness
       and do (setf current-min-fitness (fitness (least-fit new-gen SCSA) SCSA)) ;update current min fitness
       and do (cond ((> current-max-fitness max-fitness)
		 (setf max-fitness current-max-fitness)
		 (setf unchanged-count 0))
		(T (incf unchanged-count)))
       and do (cond ((< current-min-fitness min-fitness)
		 (setf min-fitness current-min-fitness)
		 (setf unchanged-count 0))
		(T (incf unchanged-count)))

       when (= unchanged-count 2)
       do (setf pass T)
       ;;when (= loop-count 3) ;exit condition--needs to be investigated
       ;;do (setf pass T)
       do (setf prev-gen new-gen)
       finally (setf return-list new-gen)) ;keep the last population
    
    (update-total-generations loop-count) ;for measurement
    (setf return-list (remove-if #'guessedp return-list)) ;no duplicate guesses allowed
    (setf result (member-if #'eligiblep return-list))
    ;;(least-fit return-list SCSA)
    (cond ((not (null result)) (first result))
	(T (least-fit return-list SCSA)))
    ))

;;;******************************************************************************
;;; Measurement Functions
;;;******************************************************************************

(defvar *total-guesses* 0)
(defvar *total-generations* 0)
(defvar *failed-guesses* 0)

(defun reset-total-guesses ()
  (setf *total-guesses* 0))

(defun update-total-guesses ()
  (setf *total-guesses* (+ *total-guesses* (length *guesses*))))

(defun reset-total-generations ()
  (setf *total-generations* 0))

(defun update-total-generations (n)
  (setf *total-generations* (+ n *total-generations*)))

(defun reset-failed-guesses ()
  (setf *failed-guesses* 0))

(defun reset-statistics ()
  (reset-total-guesses)
  (reset-history)
  (reset-total-generations)
  (reset-failed-guesses))

;;insert the following line into play-tournament under the when (null round) condition
;;and do (setf *lost-guesses* (+ *lost-guesses* (length *guesses*)))
(defun statistics (P N player SCSA num-games)
  (let ((start-time)
        (end-time)
        (run-time)
        (avg-guesses)
        (won-guesses)
        (return-list)
        (losses) ;number of losses
        (failures)
        (wins)) ;number of wins
    (reset-statistics)
    (Mastermind P N SCSA)
    (setf start-time (get-internal-run-time)) ;start time
    (setf return-list (play-tournament *Mastermind* player SCSA num-games)) ;play tournament
    (setf end-time (get-internal-run-time)) ;end time
    (setf losses (second return-list))
    (setf failures (third return-list))
    (setf wins (- num-games (+ losses failures)))
    
    (update-total-guesses) ;update total guesses for last game played
    (setf won-guesses (- *total-guesses*
		     (* 100 losses) ;100 guesses results in loss
		     *failed-guesses*))
    
    (setf run-time (- end-time start-time))
    ;; (setf run-time (/ (float run-time) 10))
    
    (setf avg-guesses (/ (float *total-guesses*) num-games))
    (cond ((> wins 0) (setf won-guesses (/ (float won-guesses) wins)))
	(T (setf won-guesses 'NA))) ;no wins
    (format t "~%(wins losses failures): ~a" return-list)
    (format t "~%Total run-time: ~a ms" run-time)
    (format t "~%Average guesses: ~a" avg-guesses)
    (format t "~%Average guesses when won: ~a" won-guesses)
    (terpri)
    (format t "~%Population size: ~a" *population-size*)
    (format t "~%Generations made: ~a" *total-generations*)))

;;;******************************************************************************
;;; nilNewts
;;;******************************************************************************

;; default max is 150 and step is 120
(defun nilNewts (board colors SCSA last-response)
  (let ((next)
        (colors* colors)
        (f* 'f2) ;function name
        (max 70) ;argument for f     MAX 120 step 80
        (step 40))
         ;argument for f
    ;; (print SCSA)
    (cond ((equal SCSA 'ab-color) (setf colors* '(a b))))
    (cond ((null last-response) ;first round, initializing values
	 (update-total-guesses)
	 (reset-history) ;reset global variables
	 (set-parameters 1 1)
	 (set-population-size (funcall f* (length *guesses*) max step)) ;decrease pop size over time - scrapped
	;;  (set-population-size 100) ;CONSTANT POPULATION SIZE
	 (setf next (make-initial-guess board colors*)))
	(T (update-responses last-response)
	   (set-population-size (funcall f* (length *guesses*) max step)) ;decrease pop size over time - scrapped
	   (setf next (GA-Player board colors* SCSA))))
    (update-guesses next)
    ;(print next)
    next))