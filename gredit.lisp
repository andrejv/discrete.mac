;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                               ;;;
;;;                   ~*~ gredit.lisp ~*~                         ;;;
;;;                                                               ;;;
;;   A graph editor using TK gui (cl-simple-tk)                   ;;;
;;;                                                               ;;;
;;;  Author: Andrej Vodopivec <andrej.vodopivec@gmail.com>        ;;;
;;;  Licence: GPL version 2 or later                              ;;;
;;;                                                               ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defpackage :tk-gredit
  (:use :cl)
  (:export :editor))

(in-package :tk-gredit)

(defvar *mouse-down* nil)
(defvar *selected-vertex* nil)
(defvar *command* nil)
(defvar *canvas* nil)
(defvar *selected-vertex* nil)
(defvar *selected-edge* nil)
(defvar *selected-vertex-var* nil)
(defvar *selected-edge-var* nil)
(defvar *vertices* nil)
(defvar *neighbors*)
(defvar *edges*)

(defun e-tag (u v)
  (concatenate 'string u "-" v))

(defun unselect-vertex ()
  (tk:canvas-itemconfig *canvas* "selected" :outline "blue")
  (tk:canvas-dtag *canvas* "selected")
  (setf (tk:var-value *selected-vertex-var*) "")
  (setf *selected-vertex* nil))

(defun select-vertex (evt)
  (unselect-vertex)
  (let ((id (find-vertex evt)))
    (when (> (length id) 0)
      (setf *selected-vertex* id)
      (setf (tk:var-value *selected-vertex-var*) id)
      (tk:canvas-addtag-withtag *canvas* "selected" id)
      (tk:canvas-itemconfig *canvas* "selected" :outline "yellow"))))

(defun unselect-edge ()
  (tk:canvas-itemconfig *canvas* "selected" :fill "black")
  (tk:canvas-dtag *canvas* "selected")
  (setf (tk:var-value *selected-edge-var*) "")
  (setf *selected-edge* nil))

(defun select-edge (evt)
  (unselect-edge)
  (let ((eid (find-edge evt)))
    (when (> (length eid) 0)
      (setf *selected-edge* eid)
      (setf (tk:var-value *selected-edge-var*) (gethash eid *edges*))
      (tk:canvas-addtag-withtag *canvas* "selected" eid)
      (tk:canvas-itemconfig *canvas* "selected" :fill "yellow"))))

(defun mouse-move (evt)
  (when (and *mouse-down* *selected-vertex*
             (string= (tk:var-value *command*) "move"))
    (destructuring-bind (x y) (tk:event-mouse-position evt)
      (setf (tk:canvas-coords *canvas* *selected-vertex*)
            (list (- x 5) (- y 5)
                  (+ x 5) (+ y 5)))
      (setf (gethash *selected-vertex* *vertices*) (list x y))
      (dolist (u (gethash *selected-vertex* *neighbors*))
        (let ((eid (tk:canvas-find-withtag *canvas* (concatenate 'string *selected-vertex* "-" u))))
          (setf (tk:canvas-coords *canvas* (car eid))
                (append (list x y) (gethash u *vertices*))))))))

(defun mouse-down (evt)
  (setf *mouse-down* t)
  (cond
    (*selected-vertex*
     (add-edge evt))
    (*selected-edge*
     (select-edge evt))
    (t
     (select-vertex evt)
     (unless *selected-vertex*
       (select-edge evt)))))

(defun find-vertex (evt)
  (destructuring-bind (x y) (tk:event-mouse-position evt)
    (let ((id (tk:canvas-find-overlapping *canvas*
                                          (- x 1) (- y 1) (+ x 1) (+ y 1))))
      (dolist (v id)
        (when (gethash v *vertices*)
          (return-from find-vertex v))))))

(defun find-edge (evt)
  (destructuring-bind (x y) (tk:event-mouse-position evt)
    (let ((id (tk:canvas-find-overlapping *canvas*
                                          (- x 2) (- y 2) (+ x 2) (+ y 2))))
      (dolist (e id)
        (unless (gethash e *vertices*)
          (return-from find-edge e))))))

(defun add-edge (evt)
  (when (string= "edges" (tk:var-value *command*))
    (let ((id (find-vertex evt)))
      (unless (or (null id) (string= id *selected-vertex*))
        (let* ((pa (gethash id *vertices*))
               (pb (gethash *selected-vertex* *vertices*))
               (eid (tk:canvas-create-line *canvas*
                                           (list (car pa) (cadr pa) (car pb) (cadr pb))
                                           :tags (list "edge"
                                                       (concatenate 'string *selected-vertex* "-" id)
                                                       (concatenate 'string id "-" *selected-vertex*)))))
          (setf (gethash eid *edges*) (list id *selected-vertex*))
          (push *selected-vertex* (gethash id *neighbors*))
          (push id (gethash *selected-vertex* *neighbors*))))
      (tk:canvas-lower *canvas* "edge" "point")))
  (select-vertex evt))

(defun remove-vertex (id)
  (let ((nid (gethash id *neighbors*)))
    (tk:canvas-delete *canvas* id)
    (remhash id *vertices*)
    (remhash id *neighbors*)
    (dolist (u nid)
      (setf (gethash u *neighbors*)
            (remove id (gethash u *neighbors*)))
      (let ((eid (tk:canvas-find-withtag *canvas* (e-tag id u))))
        (when eid
          (tk:canvas-delete *canvas* (car eid)))))))

(defun remove-edge (eid)
  (destructuring-bind (u v) (gethash eid *edges*)
    (setf (gethash u *neighbors*)
          (remove v (gethash u *neighbors*)))
    (setf (gethash v *neighbors*)
          (remove u (gethash v *neighbors*)))
    (remhash eid *edges*)
    (tk:canvas-delete *canvas* eid)))

(defun mouse-double (evt)
  (cond
    ((string= (tk:var-value *command*) "points")
     (destructuring-bind (x y) (tk:event-mouse-position evt)
       (let ((id (tk:canvas-create-oval *canvas* (list (- x 5) (- y 5)
                                                      (+ x 5) (+ y 5))
                                        :outline "blue" :fill "red"
                                        :tags "point")))
         (setf (gethash id *vertices*) (list x y)))))
    ((string= (tk:var-value *command*) "del-points")
     (let ((id (find-vertex evt)))
       (when (> (length id) 0)
         (remove-vertex id))))
    ((string= (tk:var-value *command*) "del-edges")
     (let ((eid (find-edge evt)))
       (when (> (length eid) 0)
         (remove-edge eid))))))

(defun build-editor (parent)
  (let* ((command (tk:string-variable))
         (f (tk:frame :parent parent))
         (fc (tk:frame :parent f))
         (fcc (tk:label-frame :parent fc :text "Commands"))
         (r1 (tk:radiobutton :parent fcc :text "Add points" :value "points"))
         (r2 (tk:radiobutton :parent fcc :text "Add edges" :value "edges"))
         (r3 (tk:radiobutton :parent fcc :text "Move points" :value "move"))
         (r4 (tk:radiobutton :parent fcc :text "Remove points" :value "del-points"))
         (r5 (tk:radiobutton :parent fcc :text "Remove edges" :value "del-edges"))
         (fcv (tk:label-frame :parent fc :text "Selection:"))
         (e-sel-v (tk:entry :parent fcv))
         (e-sel-e (tk:entry :parent fcv))
         (btn-ok (tk:button :parent fc :text "Done"
                            :command (lambda ()
                                       (tk:window-destroy nil)))))
    (setf *vertices* (make-hash-table :test #'equal))
    (setf *edges* (make-hash-table :test #'equal))
    (setf *command* (tk:string-variable))
    (dolist (r (list r1 r2 r3 r4 r5))
      (tk:window-configure r :variable *command*))
    (setf (tk:var-value *command*) "points")
    (setf *canvas* (tk:canvas :parent f :width 600 :height 600))
    (setf *selected-vertex-var* (tk:string-variable))
    (setf *selected-edge-var* (tk:string-variable))
    (setf *selected-vertex* nil)
    (setf *neighbors* (make-hash-table :test #' equal))
    (tk:window-configure e-sel-v :textvariable *selected-vertex-var*)
    (tk:window-configure e-sel-e :textvariable *selected-edge-var*)
    (setf (tk:var-value command) "points")
    (tk:pack fc :side "right" :anchor "n")
    (tk:pack (list fcc fcv) :anchor "n" :fill "x" :pady 6 :padx 3)
    (tk:pack (list r1 r2 r3 r4 r5 e-sel-v e-sel-e) :padx 5 :pady 2 :anchor "w")
    (tk:pack btn-ok :padx 6 :pady 3 :anchor "center")    
    (tk:pack *canvas* :expand t :fill "both" :side "left")
    (tk:bind-event *canvas* "<Double-Button-1>" #'mouse-double)
    (tk:bind-event *canvas* "<Button-1>" #'mouse-down)
    (tk:bind-event *canvas* "<Motion>" #'mouse-move)
    (tk:bind-event *canvas* "<ButtonRelease-1>" (lambda (evt)
                                                  (declare (ignore evt))
                                                  (setf *mouse-down* nil)))
    f))

(defun editor ()
  (tk:with-tk (:title "Graph Editor")
    (let ((f (build-editor nil)))
      (tk:pack f :expand t :fill "both")))
  (let ((vrt) (edg))
    (maphash (lambda (k v)
               (declare (ignore v))
               (push k vrt))
             *vertices*)
    (maphash (lambda  (k v)
               (loop for l in v do
                    (when (string< k l)
                      (push (list k l) edg))))
             *neighbors*)
    (values vrt edg *vertices*)))
