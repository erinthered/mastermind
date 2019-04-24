
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; GLOBAL VARIABLES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *Rao-KB* (list))

(defvar *being-fixed* nil)

(defvar *being-considered* nil)

(defvar *guess-history* (list))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Helper Functions (i) - Modify  KB
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;for manual resetting
(defun reset-rao ()
	(setf *Rao-KB* (list))
	(setf *being-fixed* nil)
	(setf *being-considered* nil)
	(setf *guess-history* (list)))

;add entry of the form (color 1 2 ... board)
(defun add-kb-entry (board color)
	(let ((kb-entry (list)))
		(loop for i from board downto 1
			 do (setf kb-entry (cons i kb-entry)))
		(setf *Rao-KB* (append *Rao-KB* (list (cons color kb-entry))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Helper Functions (ii) - GetNext
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;returns color tied to position pos
(defun tied (pos)
	(let ((itscolor))
		(loop for entry in *Rao-KB*
			 when (and (= (length entry) 2)
								 (= (second entry) pos))
			 do (setf itscolor (first entry)))
		itscolor))

;returns next possible position for color
(defun nextpos (color)
	(let ((next))
		(loop for entry in *Rao-KB*
			 when (and (eq (first entry) color)
								 (> (length entry) 2)) ;not fixed
			 do (setf next (second entry))
			 until (or next (not entry)))
		next))

;returns second unfixed color
(defun secondunfixed ()
	(let ((first-unfixed)
				(second-unfixed))
		(loop for entry in *Rao-KB*
			 when (and first-unfixed
								 (> (length entry) 2))
			 do (setf second-unfixed (nth 0 entry))
			 when (and (not first-unfixed)
								 (> (length entry) 2))
			 do (setf first-unfixed (nth 0 entry))
			 until (or second-unfixed (not entry)))
		second-unfixed))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Helper Functions (iii) - Update
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;adds gain # of copies of entries with header *being-considered* to KB
(defun addlists (board gain)
	(loop for i from 1 to gain
		 do (add-kb-entry board *being-considered*)))

;fix the current *being-fixed*
(defun fix ()
	(let ((NN (1- (length *Rao-KB*)))
				(entry)
				(done)) ;boolean
		(loop for n from 0 to NN
			 do (setf entry (nth n *Rao-KB*))
			 when (and (eq (first entry) *being-fixed*)
								 (> (length entry) 2)) ;dont fix an already fixed entry
			 do (setf (nth n *Rao-KB*) (subseq entry 0 2))
			 and do (setf done T)
		   until done)
		(cleanup)))

;fix color i in current position of color j
(defun fix-1 (i j)
	(let ((fixpos (nextpos j))
				(NN (1- (length *Rao-KB*)))
				(entry)
				(done)) ;boolean
		(loop for n from 0 to NN
			 do (setf entry (nth n *Rao-KB*))
			 when (and (eq (first entry) i)
								 (> (length entry) 2)) ;find entry for i
			 do (setf (nth n *Rao-KB*) (list i fixpos))
			 and do (setf done T)
			 until done)))

;delete the current position of color i from KB-entry for color j
(defun del (i j)
	(let ((deletepos (nextpos i))
				(NN (1- (length *Rao-KB*)))
				(done) ;boolean
				(entry))
		(loop for n from 0 to NN
			 do (setf entry (nth n *Rao-KB*))
			 when (and (eq (first entry) j)
								 (> (length entry) 2)) ;find entry for j
			 do (setf (nth n *Rao-KB*) (remove deletepos entry))
			 and do (setf done T)
			 until done)))

;sets the value for *being-fixed*
(defun bump ()
	(let ((NN (1- (length *Rao-KB*)))
				(done) ;boolean
				(entry))
		(loop for n from 0 to NN
			 do (setf entry (nth n *Rao-KB*))
			 when (> (length entry) 2)
			 do (setf *being-fixed* (first entry))
			 and do (setf done T)
			 until done)))

;set the value for *being-considered*
(defun nextcolor (board colors)
	(let ((next)
				(b-c-index))
		(if *being-considered*
				(setf b-c-index (1+ (spot *being-considered*))))
		(if (< (length *Rao-KB*) board)
				(if (= (numguesses) 0)
						(setf next 'a) ;initial value
						(if (< b-c-index (length colors)) ;this should be redundant
								(setf next (nth b-c-index colors))
								(setf next *being-considered*)))
				(setf next *being-considered*))
	  (setf *being-considered* next)))

;clean up KB
(defun cleanup ()
	(let ((fixedpos) ;a tied position
				(NN (1- (length *Rao-KB*)))
				(fixedind) ;index of item in KB which is tied
				(entry))
		(loop for n from 0 to NN
			 do (setf entry (nth n *Rao-KB*))
			 when (= (length entry) 2)
			 do (setf fixedpos (second entry))
			 and do (setf fixedind n)
			 and do (loop for m from 0 to NN
								 do (setf entry (nth m *Rao-KB*))
								 when (/= fixedind m)
								 do (setf (nth m *Rao-KB*) (remove fixedpos entry))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Helper Functions (iv) - Rao
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;number of KB entries which have been fixed to a position
(defun numfix ()
	(let ((fix-count 0))
		(loop for entry in *Rao-KB*
			 when (= (length entry) 2)
			 do (setf fix-count (1+ fix-count)))
		fix-count))

;number of guesses made
(defun numguesses ()
	(length *guess-history*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; GetNext
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun GetNext (board)
	(let ((nextguess)
				(itscolor)
				(gi)) ;ith entry of nextguess
		(loop for i from 1 to board
			 do (setf itscolor (tied i))
			 when itscolor
			 do (setf gi itscolor)
			 else when (eq i (nextpos *being-fixed*))
			 do (setf gi *being-fixed*)
			 else when (= (length *Rao-KB*) board)
			 do (setf gi (secondunfixed))
			 else
			 do (setf gi *being-considered*)
			 do (setf nextguess (append nextguess (list gi))))
		nextguess))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Update
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun Update (board colors bulls cows)
	(let ((gain 0))
		(setf gain (- (+ bulls cows) (numfix)))
		(if *being-fixed*	(setf gain (1- gain)))
		(if (< (length *Rao-KB*) board) (addlists board gain))
		(case cows
			(0 (fix)
				 (bump))
			(1 (if *being-fixed*
						 (del *being-fixed* *being-considered*))
				 (del *being-fixed* *being-fixed*))
			(2 (fix-1 *being-considered* *being-fixed*))
			(T (print 'error)))
		(cleanup)
		(nextcolor board colors)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Rao Mastermind Algorithm
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun rao (board colors last-response)
	(let* ((bulls (first last-response))
				 (cows (second last-response))
				 (guess))
		(cond ((null bulls)
					 (reset-rao)
					 (setf bulls 0)
					 (setf cows 0)))
		(Update board colors bulls cows)
		(cleanup) ;need this again for some reason
		(setf guess (GetNext board))
		(cond ((= (numguesses) 0)
					 (setf *guess-history* (list guess)))
					(T (setf *guess-history* (append *guess-history* (list guess)))))
		guess))
				
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Player
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun rao-player (board colors SCSA last-response)
	(declare (ignore SCSA))
	(rao board colors last-response))

;test your own sequence of guesses and responses below
(defun run ()
	(let ((resplist '(nil (0 0) (2 0) (2 0) (2 1) (2 0) (4 0))))
		(reset-rao)
		(Mastermind 5 6 'two-color-alternating)
		(setf (answer *Mastermind*) '(b c b d f))
		(loop for resp in resplist
				 do (print resp)
				 do (print (funcall 'nilNewts 5 nil nil resp)))))
