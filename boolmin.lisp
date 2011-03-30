;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                               ;;;
;;;                  ~*~ boolmin.lisp ~*~                         ;;;
;;;                                                               ;;;
;;;  An implementation of Quine-McCluskey algorithm for Maxima.   ;;;
;;;  (http://en.wikipedia.org/wiki/Quine-McCluskey_algorithm)     ;;;
;;;                                                               ;;;
;;;  Author: Andrej Vodopivec <andrej.vodopivec@gmail.com>        ;;;
;;;  Licence: GPL version 2 or later                              ;;;
;;;                                                               ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar $boolean_minimize_debug nil)
(defvar $boolean_minimize_backtrack t)

;;;
;;; combine-terms will combine terms term1 and term2 if possible
;;; (when term1 and term2 are different in one place and in that
;;;  place we have t and nil; combined term will have 'any in
;;;  that place)
;;;
(defun combine-terms (term1 term2)
  (let ((combined ()) (count 0))
    (do ((t1 term1 (cdr t1))
	 (t2 term2 (cdr t2)))
	((null t1) combined)
      (cond ((not (equal (car t1) (car t2)))
	     (incf count)
	     (setq combined (cons 'any combined)))
	    (t (setq combined (cons (car t1) combined)))))
    (when (/= count 1)
      (return-from combine-terms nil))
    (reverse combined)))

;;;
;;; Check if term1 implies term2
;;;
(defun term1-implies-term2-p (term1 term2)
  (do ((t1 term1 (cdr t1))
       (t2 term2 (cdr t2)))
      ((null t1) t)
    (when (and (not (eq (car t1) 'any))
	       (not (eq (car t1) (car t2))))
      (return-from term1-implies-term2-p nil))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Quin-McClusky algorithm
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;
;;; Optimal solution
;;;
(defvar *covering-rows*)

(defun boolmin (expr)

  (let ((vars (cdr ($listofvars expr)))
	(terms ()) (primitive-terms ()) 
	current number-of-vars
	(active-terms (make-hash-table :test #'equal))
	(table (make-hash-table :test #'equal))
	implication-table)

    (when $boolean_minimize_debug
      (format t "~%----- COMPUTING TERMS IN EXPRESSION -----")
      (format t "~%Variables: ~a" (reverse vars)))

    ;;; compute the list of terms
    (dolist (term (cdr expr))
      (cond ((atom term) (setq term `((mlist simp) ,term)))
	    ((eq (caar term) 'mnot) (setq term `((mlist simp) ,term))))
      (setq current ())
      (dolist (v vars)
	(cond ((member v term)
	       (setq current (cons t current)))
	      ((member `((mnot simp) ,v) term :test #'equal)
	       (setq current (cons nil current)))
	      (t (setq current (cons 'any current)))))
      (setf (gethash current active-terms) t)
      (setq terms (cons current terms)))

    (when $boolean_minimize_debug
      (format t "~%Original terms:")
      (loop for trm in terms do
	   (format t "~%  ~a" trm)))

    ;;; sort the terms according to the number of anys and
    ;;; the number of ts
    (dolist (term terms)
      (push term (gethash (list (count 'any term)
				(count t term))
			  table)))

    (when $boolean_minimize_debug
      (format t "~%Initial table of terms:~%")
      (maphash (lambda (key val) (format t "~%  ~a -> ~a" key val)) table)
      (format t "~%----- COMBINING TERMS -----"))
    
    (setq number-of-vars (length vars))
    ;;; combine two terms if the number of anys is the same and the number
    ;;; of ts differs for 1
    (loop for anys from 0 to (1- number-of-vars) do
	 (loop for ts from 0 to (1- number-of-vars) do
	      ;;; combine terms term1 and term2
	      (dolist (term1 (gethash (list anys ts) table))
		(dolist (term2 (gethash (list anys (1+ ts)) table))
		  (when $boolean_minimize_debug
		    (format t "~%  Checking terms ~a ~a ~%" term1 term2))
	          ;;; check if we can combine these two terms
		  (let ((combined (combine-terms term1 term2)))
		    (when combined
		      ;;; If we can combine term1 and term2 the they are not primitive
		      (remhash term1 active-terms)
		      (remhash term2 active-terms)
		      ;;; If combined is a new term add it to the table (and mark it primitive)
		      (unless (member combined
				      (gethash (list (1+ anys) ts) table)
				      :test #'equal)
			(when $boolean_minimize_debug
			  (format t "~%    Combining terms ~a ~a -> ~a ~%" term1 term2 combined))
			(setf (gethash combined active-terms) t)
			(push combined (gethash (list (1+ anys) ts) table))))) )) ))

    ;;; Get the primitive terms
    (maphash (lambda (key val)
	       (declare (ignore val))
	       (setf primitive-terms (cons key primitive-terms)))
	     active-terms)

    (when $boolean_minimize_debug
      (format t "~%----- COMBINING TERMS FINISHED -----")
      (format t "~%Primitive terms:~%~a~%" primitive-terms)
      (format t "~%----- REDUCING BY SINGLES HACK -----"))
   
    ;;; Singles hack: based on p and (not p or A) = p or A.
    ;;; If we have a single variable in a term, use the hack to
    ;;; reduce other terms.
    (let ((ones) (i 0) (used-ones))      
      (dolist (v primitive-terms)
        (when (= (count 'any v) (1- (length v)))
          (push i ones))
        (incf i))
      (when $boolean_minimize_debug
        (format t "~% Singles rows: ~a" ones))
      (loop while ones do
           (let* ((p (pop ones))
                  (p-row (nth p primitive-terms)))
             (push p used-ones)
             (loop for i from 0 to (1- (length primitive-terms)) do
                  (when (/= i p)
                    (let ((c-row (nth i primitive-terms))
                          (nc-row))
                      (do ((pe p-row (cdr pe))
                           (ce c-row (cdr ce)))
                          ((null pe))
                        (cond
                          ((eq (car ce) 'any)
                           (push 'any nc-row))
                          ((eq (null (car ce)) (car pe))
                           (push 'any nc-row))
                          (t (push (car ce) nc-row))))
                      (when (and (= (count 'any nc-row) (1- (length nc-row)))
                                 (not (member i used-ones)))
                        (pushnew i ones))
                      (setf (nth i primitive-terms) (reverse nc-row))))))))

    (when $boolean_minimize_debug
      (format t "~%Reduced set of primitive terms:~%~a" primitive-terms)
      (format t "~%----- COMPUTING IMPICATION TABLE -----"))

    ;;; Create implication table
    (setf implication-table (make-array (list (length primitive-terms) (length terms))))
    (loop for i from 0 to (1- (length primitive-terms)) do
	 (loop for j from 0 to (1- (length terms)) do
	      (setf (aref implication-table i j)
		    (term1-implies-term2-p (nth i primitive-terms)
					   (nth j terms)))))

    (when $boolean_minimize_debug
      (format t "~%Implication table: (~a x ~a)~%" (length primitive-terms) (length terms))
      (print implication-table))

    ;;; Find minimum set of rows which cover all columns
    (let ((active-rows (make-hash-table))    ;; active rows are rows wich can be selected
	                                     ;; sometimes in the future
	  (selected-rows (make-hash-table))  ;; which rows we have selected
	  (*covering-rows* ()))

      ;;; start with all columns/rows active
      (dotimes (i (length primitive-terms))
	(setf (gethash i active-rows) t))
      
      (when $boolean_minimize_debug
	(format t "~%----- FINDING MINIMUM COVER -----~%~%"))

      (select-min-rows-det implication-table (length primitive-terms) (length terms)
		       active-rows selected-rows 0)

      (when $boolean_minimize_debug
	(format t "~%----- FOUND MINIMUM COVER -----~%~%"))

      (when $boolean_minimize_debug
	(format t "~% Minimum cover: ~a" *covering-rows*))

      ;;; Construct the expression from the table
      (setf vars (reverse vars))
      (let ((expression nil))
	(dolist (i *covering-rows*)
	  (let ((curr t) (trm (nth i primitive-terms)))
	    (do ((var trm (cdr var))
		 (vrs vars (cdr vrs)))
		((null var))
	      (cond ((eq (car var) t)
		     (setf curr ($apply 'mand `((mlist simp) ,curr ,(car vrs)))))
		    ((null (car var))
		     (setf curr ($apply 'mand `((mlist simp) ,curr ((mnot simp) ,(car vrs))))))))
	    (setf expression ($apply 'mor `((mlist simp) ,expression ,curr)))))
	
	expression))))

;;;
;;; Finds all active columns which are covered by only one row, which can be
;;; selected
;;;
(defun find-single-covered-cols (table n m active-rows selected-rows)
  (let ((single-covered ()))

    ;;; check if j-th col is covered by a single row
    (dotimes (j m)
      (let ((count 0) (covering-row) (covered))

	;; check all rows
	(dotimes (i n)
	  (when (and (gethash i active-rows)
		     (aref table i j))
	    (incf count)
	    (setf covering-row i))
	  (when (and (gethash i selected-rows)
		     (aref table i j))
	    (setf covered t)))

	(when (and (= count 1) (null covered))
	  (push (list covering-row j) single-covered))))

    single-covered))

;;;
;;; Computes the minimum number of rows neede to cover all columns
;;;
;;; This is a backtracking algorithm:
;;;  - it first chooses a column which is covered by the minimum number
;;;    of active rows (first-column)
;;;  - it covers first-column starting with rows which cover the most
;;;    of the uncovered columns
;;;
(defun select-min-rows-det (table n m active-rows selected-rows depth)

  ;; check if we can backtrack
  (when (and (null $boolean_minimize_backtrack) *covering-rows*)
    (return-from select-min-rows-det nil))

  (when $boolean_minimize_debug
    (let ((keys))
      (maphash (lambda (key val)
		 (when val
		   (push key keys)))
	       selected-rows)
      (format t "~% DEPTH: ~a" depth)
      (format t "~% Current rows: ~a" keys)))

  (let (needed-rows primitive-cols rows uncovered-cols)
 
    ;; find all rows which need to be added
    (dolist (pair (find-single-covered-cols table n m active-rows selected-rows))
      (push (cadr pair) primitive-cols)
      (unless (member (car pair) needed-rows)
	(push (car pair) needed-rows)
	(setf (gethash (car pair) active-rows) nil)
	(setf (gethash (car pair) selected-rows) t)))

    (when (and $boolean_minimize_debug needed-rows)
      (format t "~% Adding needed rows ~a" needed-rows))
    
    ;; count how many rows we must select, find active rows
    (let ((selected-count 0))
      (dotimes (i n)
	(cond
	  ((gethash i selected-rows)
	   (incf selected-count))
	  ((gethash i active-rows)
	   (push i rows))))
      
      ;; can we improve the covering?
      (when (or (null *covering-rows*)
		(< selected-count (length *covering-rows*)))
	
	;;;;;;;;;;;;;;;;;;;
	;;
	;; STEP 1: find the column which is covered by the minimum number of rows
	;;
	;;;;;;;;
	(let ((first-col -1) (covered-times-min -1))
	  (dotimes (j m)
	    ;; check if j is covered
	    (let ((covered nil) (covered-times 0))
	      (dotimes (i n)
		(when (aref table i j)
		  (if (gethash i selected-rows)
		      (setf covered t)
		      (incf covered-times))))
	      (unless covered
		(push j uncovered-cols)
		(when (or (= first-col -1)
			  (< covered-times covered-times-min))
		  (setf first-col j
			covered-times-min covered-times)))))

	  (if (= first-col -1)
	      ;; we covered the table
	      (let ((covering))
		(dotimes (i n)
		  (when (gethash i selected-rows)
		    (push i covering)))
		(when $boolean_minimize_debug
		  (format t "~% !!! ---> Found covering: ~a~&" covering))
		(when (or (null *covering-rows*)
			  (< (length covering)
			     (length *covering-rows*)))
		  (setf *covering-rows* covering)))

	      ;; cover first-col with some row
	      (let ((deactivated-rows) (cover-cols (make-hash-table)))

		;;;;;;;;;;;;;;;;;;;
		;;
		;; STEP 2: cover first-col (start with rows which cover the most
		;;         of the uncovered cols)
		;;
	        ;;;;;;;;
		(dolist (i rows)
		  (setf (gethash i cover-cols) 0)
		  (dolist (j uncovered-cols)
		    (when (aref table i j)
		      (incf (gethash i cover-cols)))))
		(setf rows (sort rows (lambda (row1 row2) (> (gethash row1 cover-cols)
							     (gethash row2 cover-cols)))))

		(dolist (i rows)
		  (when (and (> (gethash i cover-cols) 0)
			     (gethash i active-rows)
			     (aref table i first-col))
		    ;; add i to selected rows
		    (when $boolean_minimize_debug
		      (format t "~% Adding row ~a at depth ~a" i depth))
		    (setf (gethash i active-rows) nil)
		    (push i deactivated-rows)
		    (setf (gethash i selected-rows) t)
		    (select-min-rows-det table n m active-rows selected-rows (1+ depth))
		    (setf (gethash i selected-rows) nil)))

		(when $boolean_minimize_debug
		  (format t "~%  Finishing depth ~a, re-activating rows ~a" depth deactivated-rows))
		(dolist (i deactivated-rows)
		  (setf (gethash i active-rows) t)))))))

    ;; forget required rows/cols
    (when (and $boolean_minimize_debug needed-rows)
      (format t "~%  Removing needed rows:"))
    (dolist (row needed-rows)
      (when $boolean_minimize_debug
	(format t " ~a" row))
      (setf (gethash row active-rows) t)
      (setf (gethash row selected-rows) nil)) ))
