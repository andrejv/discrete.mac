;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                               ;;;
;;;                ~*~ load-editor.lisp ~*~                       ;;;
;;;                                                               ;;;
;;   A graph editor using TK gui (cl-simple-tk)                   ;;;
;;;                                                               ;;;
;;;  Author: Andrej Vodopivec <andrej.vodopivec@gmail.com>        ;;;
;;;  Licence: GPL version 2 or later                              ;;;
;;;                                                               ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(in-package :maxima)

(asdf:load-system :simple-tk)

($load "gredit.lisp")
($load "mgredit.lisp")
