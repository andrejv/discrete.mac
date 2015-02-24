;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                               ;;;
;;;                  ~*~ mgredit.lisp ~*~                         ;;;
;;;                                                               ;;;
;;   A graph editor using TK gui (cl-simple-tk)                   ;;;
;;;                                                               ;;;
;;;  Author: Andrej Vodopivec <andrej.vodopivec@gmail.com>        ;;;
;;;  Licence: GPL version 2 or later                              ;;;
;;;                                                               ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(in-package :maxima)

(defun $graph_input ()
  (let ((g ($empty_graph 0)) (mpos))
    (multiple-value-bind (vrt edg pos) (tk-gredit:editor)
      (dolist (v vrt)
        (add-vertex (parse-integer v) g)
        (let ((vpos (gethash v pos)))
          (push `((mlist simp)
                  ,(parse-integer v)
                  ((mlist simp) ,(car vpos) ,(- 600 (cadr vpos))))
                mpos)))
      (dolist (e edg)
        (add-edge (mapcar #'parse-integer e) g)))
    ($set_positions (cons '(mlist simp) mpos) g)
    g))
