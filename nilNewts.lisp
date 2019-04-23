;;crossover function used by genetic algorithm
;;takes two parent codes and randomly crosses them over and recombines them to produce two children codes
;;based on Berghman, L., Dries, G., Leus, R. (2009). Efficient Solutions for Mastermind Using Genetic Algorithms. Proceedings of the 1999 ACM Symposium on Applied Computing, SAC'99, 307-311.

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

