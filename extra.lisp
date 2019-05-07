
(defun ab-color-weight (code)
  (loop for i from 0 to (1- (length code))
	do (when (and (not (equal (nth i code) 'A))
		      (not (equal (nth i code) 'B)))
	     (return 1))
	finally (return 0)))
	
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


;; codes is a list of codes. does not have to be distinct
;; general n-least-fit function for testing. More efficient to use least-fit, 2-least-fit, or write a function when n is a known value.
(defun n-least-fit (n codes SCSA)
  (let ((fitness-seq (fitness-sequence-from-list codes SCSA)))
    (setf fitness-seq (stable-sort fitness-seq #'< :key #'first))
    (loop for i from 0 to (1- n)
       collect (second (nth i fitness-seq)) into result
       finally (return result))))

;; sigmoid increasing function from min to min+step
(defun f3 (x min step)
  (let* ((g (/ (* step x) (+ 1 (abs x))))
         (result (multiple-value-bind (f r) (floor g) (list f r)))
         (x* (first result)))
    (+ min x*)))
