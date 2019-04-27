;;;*********************************************************************************************************
;;;THIS FILE IS THE MASTERMIND PLAYER FOR THE NIL NEWTS
;;;USES A GENETIC ALGORITHM WITH LOCAL SEARCH BASED ON:
;;;Berghman, L., Dries, G., Leus, R. (2009). Efficient Solutions for Mastermind Using Genetic Algorithms.
;;;Proceedings of the 1999 ACM Symposium on Applied Computing, SAC'99, 307-311.
;;;Oijen, V. (2018). Genetic Algorithms Playing Mastermind. Utrecht University Bachelor Thesis, Netherlands.
;;;*********************************************************************************************************


;;;*********************************************************************************************************
;;;Helper Functions
;;;*********************************************************************************************************

;;copy constructor for game class
(defmethod copy-game ((self game))
	(make-instance 'game :board (board self) :colors (colors self) :number-of-colors (number-of-colors self) :answer (answer self) :SCSA (SCSA self) :guesses (guesses self) :game-cutoff (game-cutoff self)))

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
;;returns a list of similarity scores for each entry in codes, where similarity is as defined in Berghman et al
(defun similarity-scores (codes)
	(let ((result (make-list (length codes) :initial-element 0))
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
											(+ (nth j result) (apply '+ (process-guess game-copy c)))))
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
  (declare (ignore board colors SCSA last-response))) ;to avoid compiler warnings, remove when writing player
