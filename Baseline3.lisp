(defvar *guess-list* nil)
(defvar *color-counter* 0)

(defun generate-individual-colors (board colors)
		(let ((peg))
		(loop for i from 0 to (1- board)
			do(setf peg (nth *color-counter* colors))
			collect peg)))

(defun generate-individual-guesses (board colors last-response))

(defun baseline-3 (board colors last-response)
	(let ((guess-sequence))
	(cond (	(< *color-counter* (- (length colors) 1))
						(setf guess-sequence (generate-individual-colors board colors))
						(push guess-sequence *guess-list*)
						(setf *color-counter* (+ *color-counter* 1)))
					(T 
						(setf guess-sequence (generate-eligible-guesses board colors last-response))
						(push guess-sequence *guess-list*)))
				guess-sequence))


(defun nilNewts (board colors SCSA last-response)
  (declare (ignore SCSA))
	(let ((guess))
	(baseline-3 board colors last-response)
	(setf guess (first *guess-list*))
	(print guess)))