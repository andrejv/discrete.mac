;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                               ;;;
;;;                   ~*~ group.lisp ~*~                          ;;;
;;;                                                               ;;;
;;;  A simple program for computing the generators of the group   ;;;
;;;  of automorphisms of a simple graph.                          ;;;
;;;                                                               ;;;
;;;  Author: Andrej Vodopivec <andrej.vodopivec@gmail.com>        ;;;
;;;  Licence: GPL version 2 or later                              ;;;
;;;                                                               ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *generators*)
(defvar *distance-partitions*)
(defvar $use_partitions t)

(defun connection-vector (v g fixed)
  (loop for u in fixed collect (null (member v (neighbors u g)))))

(defun find-normalizer (g fixed free)
  ;; Check if some free vertices are really fixed
  (let ((start t) more-fixed)
    (loop while start do
	 (let ((vectors (make-hash-table :test #'equal)))
	   (loop for v in free do
		(push v (gethash (connection-vector v g fixed) vectors)))
	   (maphash (lambda (k v)
		      (declare (ignore k))
		      (when (= 1 (length v))
			(push (car v) more-fixed)))
		    vectors))
	 (if more-fixed
	     (progn
	       (setq free (set-difference free more-fixed))
	       (setq fixed (append fixed more-fixed))
	       (setq more-fixed nil))
	     (setq start nil))))

  ;; For other free vertices, use recursion
  (unless (null free)
    (let* ((x (pop free))
	   (mapping (make-hash-table))
	   out1 out2 (m1 (cons x fixed)) (m2 fixed)
	   xvector)
      ;; Find the normalizer of x
      (find-normalizer g (cons x fixed) free)
      ;; Find orbits of x
      (loop for v in fixed do (setf (gethash v mapping) v))
      (setq xvector (connection-vector x g fixed))
      (setq out1 (set-difference
		  (reduce #'union (loop for v in m1 collect (neighbors v g)))
		  m1))
      (setq out2 (set-difference
		  (reduce #'union (loop for v in m2 collect (neighbors v g)))
		  m2))
      (loop while (> (length free) 0) do
	   (let ((y (pop free)))
	     (when (and (equal xvector (connection-vector y g fixed))
			(if $use_partitions
			    (equal (cdr (gethash x *distance-partitions*))
				   (cdr (gethash y *distance-partitions*)))
			    (= ($vertex_degree x g)
			       ($vertex_degree y g))))
	       (let ((m2 m2) (out2 out2) iso)
		 (setf (gethash x mapping) y)
		 (setq m2 (cons y m2))
		 (setq out2 (set-difference (union out2 (neighbors y g)) m2))
		 (setq iso (extend-isomorphism-graphs mapping m1 m2 out1 out2 g g))
		 (when iso
		   ;; Remove some obvious orbits
		   (loop while (not (= x y)) do
			(setq y (gethash y iso))
			(dolist (gen *generators*)
			  (setq free (remove (cdr (assoc y gen)) free))))
		   ;; Add the new generator
		   (let ((generator))
		     (maphash (lambda (k v) (push `(,k . ,v) generator)) iso)
		     (push generator *generators*))))))))))

(defun $graph_automorphisms_gens (g)
  (let* ((vrt (vertices g))
	 (free (cdr vrt))
	 (x (car vrt))
	 (*distance-partitions* (make-hash-table))
	 *generators*)

    (when $use_partitions
      (dolist (v vrt)
	(setf (gethash v *distance-partitions*) (cdr ($distance_partition v g)))))

    ;; Find the normalizer of x
    (find-normalizer g (list x) free)

    ;; Find orbits of x
    (loop while free do
	 (let ((y (pop free)))
	   (when (if $use_partitions
		     (equal (cdr (gethash x *distance-partitions*))
			    (cdr (gethash y *distance-partitions*)))
		     (= ($vertex_degree x g)
			($vertex_degree y g)))
	     (let ((mapping (make-hash-table))
		   (m1 (list x)) (out1 (neighbors x g))
		   (m2 (list y)) (out2 (neighbors y g)))
	       (setf (gethash x mapping) y)
	       (let ((iso (extend-isomorphism-graphs mapping m1 m2 out1 out2 g g)))
		 (when iso
		   ;; Remove some obvious orbits
		   (loop while (/= x y) do
			(setq y (gethash y iso))
			(dolist (gen *generators*)
			  (setq free (remove (cdr (assoc y gen)) free)))
			(setq free (remove y free)))
		   (let (generator)
		     (maphash (lambda (k v) (push `(,k . ,v) generator)) iso)
		     (push generator *generators*))))))))

    ;; Report generators
    (let ((generators '((mlist simp))))
      (loop for gen in *generators* do
	   (let ((generator (cons '(mlist simp) (make-list (graph-order g) :initial-element 0))))
	     (dolist (p gen)
	       (setf (nth (car p) generator) (cdr p)))
	     (push generator generators)))

      ($setify (reverse generators)))))

(defun $distance_partition (v g)
  (let ((active (list v))
	(visited (make-hash-table))
	(partition))
    (loop while active do
	 (push (length active) partition)
	 (let ((new-active))
	   (dolist (x active)
	     (setf (gethash x visited) t)
	     (dolist (y (neighbors x g))
	       (unless (or (gethash y visited)
			   (member y active)
			   (member y new-active))
		 (push y new-active))))
	   (setq active new-active)))
    (cons '(mlist simp) (reverse partition))))
