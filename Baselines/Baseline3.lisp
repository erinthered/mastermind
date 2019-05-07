(defvar *guess-list* nil)
(defvar *response-list* nil)
(defvar *color-counter* 0)
(defvar *eligible* nil)

;; generates a list of individual colors
(defun generate-individual-colors (board colors)
	(let ((peg))
	(loop for i from 0 to (1- board)
		do(setf peg (nth *color-counter* colors))
		collect peg)))

;; populates the list of eligible code and removes duplicates
(defun populate-list (board eligible-code)
	(let ((other-eligible-code (copy-list eligible-code)))	
		(loop for i from 0 to (1- board)
			do (loop for j from 0 to (1- board)
				do (rotatef (nth i other-eligible-code) (nth j other-eligible-code))
				do (push other-eligible-code *eligible*)
				do (setf other-eligible-code (copy-list eligible-code))))
		(setf *eligible* (remove-duplicates *eligible* :test #'equalp :from-end t))))

;; generates the first eligible code and calls helper function to derive all other eligible codes
(defun generate-eligible (board colors)
	(let ((remaining-pegs board)
		(eligible-code nil))
		(loop for color in *response-list*
			do(loop for i from 0 to (1- (nth 1 color))
				do (push (nth 0 color) eligible-code)
				do (setf remaining-pegs (1- remaining-pegs))))	
		(cond ((not (= remaining-pegs 0))
				(loop for i from 0 to (1- remaining-pegs)
					do (push (nth (- (length colors) 1) colors) eligible-code))
				(setf remaining-pegs 0)))
		(populate-list board eligible-code)))

;; maps the response from the game to create a list of responses
(defun map-response-to-colors (colors)
	(let ((color-limit (- (length colors) 2)))
		(setf *color-counter* (+ *color-counter* 1))
		(loop for i from color-limit downto 0
			do(setf (nth (- color-limit i) *response-list*)
					(list (nth i colors) (nth 0 (nth (- color-limit i) *response-list*)))))))

;; main player for baseline-3
;; the first colors-1 guesses will be a list of the same colors
;; then it uses the responses as a knowledge base to generate a list of other eligible codes
(defun baseline-3 (board colors last-response)
	(let ((guess-sequence))
	(cond (	(and (> *color-counter* 0) (< *color-counter* (length colors)))
			(push last-response *response-list*)))
	(cond (	(< *color-counter* (- (length colors) 1))
				(setf guess-sequence (generate-individual-colors board colors))
				(push guess-sequence *guess-list*)
				(setf *color-counter* (+ *color-counter* 1)))
			(T
				(cond (	(= *color-counter* (- (length colors) 1))
						(map-response-to-colors colors)
						(generate-eligible board colors)))
				(setf guess-sequence (first *eligible*))
				(push guess-sequence *guess-list*)
				(setf *eligible* (rest *eligible*))))
			;; print(guess-sequence)
		guess-sequence))


(defun nilNewts (board colors SCSA last-response)
  (declare (ignore SCSA))
	(let ((guess))
	(cond ((null last-response)
			(setf *guess-list* nil)
			(setf *response-list* nil)
			(setf *color-counter* 0)
			(setf *eligible* nil)))
	(baseline-3 board colors last-response)
	(setf guess (first *guess-list*))
	(print guess)))