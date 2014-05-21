;;; atmosphere-calculator.lisp --- yet another Lisp hack.

;; Copyright (C) 2014 Ralph Schleicher

;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions
;; are met:
;;
;;    * Redistributions of source code must retain the above copyright
;;      notice, this list of conditions and the following disclaimer.
;;
;;    * Redistributions in binary form must reproduce the above copyright
;;      notice, this list of conditions and the following disclaimer in
;;      the documentation and/or other materials provided with the
;;      distribution.
;;
;;    * The name of the author may not be used to endorse or promote
;;      products derived from this software without specific prior
;;      written permission.
;;
;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR "AS IS" AND ANY EXPRESS
;; OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;; ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT,
;; INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
;; (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
;; SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
;; HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
;; STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING
;; IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
;; POSSIBILITY OF SUCH DAMAGE.

;;; Code:

(in-package :common-lisp-user)

(defpackage :atmosphere-calculator
  (:use :common-lisp
	:iterate
	:named-readtables
	:gtk :gobject
	:rs-cll)
  (:shadow #:t)
  (:documentation "Calculate ambient conditions and air speeds."))

(in-package :atmosphere-calculator)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (save-special-variables
   (*read-default-float-format* 'double-float)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defreadtable case-sensitive
    (:merge :standard)
    (:case :invert)))

(in-readtable case-sensitive)

;;;; Unit Conversion

;; This table has to cover the elements of the GtkListStore objects
;; defined in file `atmosphere-calculator.ui'.
(defvar utab
  (let* ((in 254/10000)
	 (ft (* 12 in))
	 (yd (* 3 ft))
	 (mi (* 1760 yd))
	 (lb 0.45359237)
	 (lbf (* lb 9.80665)))
    `(;; Altitude.
      ("m"  "m" 1)
      ("km" "m" 1000)
      ("ft" "m" ,ft)
      ("mi" "m" ,mi)
      ;; Temperature.
      ("°C" "°C" 1)
      ("K"  "°C" (,(function degree-celsius-from-kelvin)
		  ,(function kelvin-from-degree-celsius)) 1)
      ("°F" "°C" (,(function degree-celsius-from-degree-fahrenheit)
		  ,(function degree-fahrenheit-from-degree-celsius)) 5/9)
      ("°R" "°C" (,(function degree-celsius-from-degree-rankine)
		  ,(function degree-rankine-from-degree-celsius)) 5/9)
      ;; Velocity.
      ("m/s"  "m/s" 1)
      ("km/h" "m/s" 1000/3600)
      ("kn"   "m/s" 1852/3600)
      ("in/s" "m/s" ,in)
      ("ft/s" "m/s" ,ft)
      ("mi/h" "m/s" ,(/ mi 3600))
      ;; Pressure.
      ("Pa"   "Pa" 1)
      ("hPa"  "Pa" 100)
      ("MPa"  "Pa" 1000000)
      ("bar"  "Pa" 100000)
      ("atm"  "Pa" 101325)
      ("Torr" "Pa" 101325/760)
      ("psi"  "Pa" ,(/ lbf (expt in 2)))
      ;; Ratio.
      ("1" "1" 1)
      ("%" "1" 1/100)
      ))
  "Alist of unit conversions.
First element is the source/target unit.
Second element is the intermediate unit.
Third element is the conversion scale.
Optional fourth element is the conversion
 scale for a relative unit conversion.")

(defvar utab-aliases
  '(("" . "1")
    ("NM" . "M")
    ("kt" . "kn")
    ("fps" . "ft/s")
    ("mph" . "mi/h")
    ("mbar" . "hPa")
    ("degC" . "°C")
    ("degF" . "°F")
    ("degR" . "°R"))
  "Alist of unit aliases.
Key is the alternative unit spelling.
Value is the actual unit spelling.")

(defun canonical-unit-name (unit-name)
  "Return the canonical unit name."
  ;; Trim whitespace characters.
  (let ((s (multiple-value-bind (start end)
	       (bounding-indices-if-not #'whitespace-char-p unit-name)
	     (subseq unit-name start end))))
    ;; Fix multiplication sign.
    (setf s (nsubstitute #\× #\· s)
	  s (nsubstitute #\* #\· s)
	  s (nsubstitute #\. #\· s)
	  s (nsubstitute #\  #\· s))
    ;; Resolve aliases.
    (let ((entry (assoc s utab-aliases :test #'string=)))
      (when entry (setf s (cdr entry))))
    ;; Return value.
    (if (string= unit-name s) unit-name s)))

(defun uconv (value unit target &optional relative)
  "Convert a numerical value from one unit into another.

First argument VALUE is the numerical value (a number).
Second argument UNIT is the physical unit (a string).
Third argument TARGET is the target unit (a string).
If optional argument RELATIVE is true, VALUE/UNIT represents
 an assembled physical quantity.  This is only relevant when
 converting temperatures.

Return value is the numerical value of the physical quantity
given in the target unit."
  (let ((from (canonical-unit-name unit))
	(to (canonical-unit-name target)))
    ;; If both units are equal, return value as is.
    (when (string/= from to)
      (let ((ent1 (assoc from utab :test #'string=))
	    (ent2 (assoc to utab :test #'string=)))
	(cond ((not ent1)
	       (error "Unknown physical unit '~A'." unit))
	      ((not ent2)
	       (error "Unknown physical unit '~A'." target))
	      ;; Intermediate units have to be equal.
	      ((string/= (second ent1) (second ent2))
	       (error "Incompatible physical units '~A' and '~A'." unit target)))
	;; Convert VALUE into intermediate unit.
	(let ((conv (cddr ent1)))
	  (setf conv (if (and relative (rest conv))
			 (second conv)
		       (first conv))
		value (if (numberp conv)
			  (* value conv)
			(funcall (first conv) value))))
	;; Convert VALUE into target unit.
	(let ((conv (cddr ent2)))
	  (setf conv (if (and relative (rest conv))
			 (second conv)
		       (first conv))
		value (if (numberp conv)
			  (/ value conv)
			(funcall (second conv) value))))
	))
    value))

;;;; Physical Quantities

(defclass q-object ()
  ((tag
    :documentation "A short description of the physical quantity."
    :initarg :tag
    :initform (error "Missing tag.")
    :accessor q-tag)
   (value
    :documentation "The numerical value, or nil if void."
    :initarg :value
    :initform nil
    :accessor q-value)
   (unit
    :documentation "The unit of measurement."
    :initarg :unit
    :initform (error "Missing unit.")
    :accessor q-unit)
   (relative
    :documentation "Whether or not the unit is a relative unit."
    :initarg :relative
    :initform nil
    :accessor q-relative)
   (value-widget
    :documentation "The widget to display the numerical value."
    :initarg :value-widget
    :initform (error "Missing value widget.")
    :accessor q-value-widget)
   (value-editable
    :documentation "Whether or not the numerical value is editable."
    :initarg :value-editable
    :initform cl:t
    :accessor q-value-editable)
   (value-dirty
    :documentation "Whether or not the numerical value has been edited."
    :initform nil
    :accessor q-value-dirty)
   (display-value
    :documentation "The currently displayed numerical value, or nil if void."
    :initform nil
    :accessor q-display-value)
   (unit-widget
    :documentation "The widget to display the unit of measurement."
    :initarg :unit-widget
    :initform (error "Missing unit widget.")
    :accessor q-unit-widget)
   (unit-dirty
    :documentation "Whether or not the unit has been edited."
    :initform nil
    :accessor q-unit-dirty)
   (display-unit
    :documentation "The currently displayed unit of measurement."
    :initarg :display-unit
    :initform nil
    :accessor q-display-unit)
   (display-units
    :documentation "Alist of display units.  Key is the drop-down menu index, value is the unit name."
    :initform nil
    :accessor q-display-units))
  (:documentation
   "A quasi meta-widget to display a physical quantity, that is the
combination of a text entry widget for the numerical value and
a drop-down menu widget for the unit of measure."))

(defun push-display-value (q)
  (let* ((value (q-display-value q))
	 (string (if (realp value)
		     (multiple-value-bind (q r)
			 (truncate value)
		       (format nil "~A" (if (zerop r) q (float value 1F0))))
		   "")))
    (setf (gtk-entry-text (q-value-widget q)) string)))

(defun pull-display-value (q)
  (let* ((string (gtk-entry-text (q-value-widget q)))
	 ;; Attempt to read a number from STRING.
	 (value (let ((*read-default-float-format* 'double-float))
		  (multiple-value-bind (start end)
		      (bounding-indices-if-not #'whitespace-char-p string)
		    (let ((obj (read-from-string string nil nil :start start :end end)))
		      (when (realp obj) obj))))))
    (setf (q-display-value q) value)))

(defun push-display-unit (q)
  (let* ((unit (q-display-unit q))
	 (index (car (rassoc unit (q-display-units q) :test #'string=))))
    (setf (gtk-combo-box-active (q-unit-widget q)) index)))

(defun pull-display-unit (q)
  (let* ((index (gtk-combo-box-active (q-unit-widget q)))
	 (unit (cdr (assoc index (q-display-units q)))))
    (setf (q-display-unit q) unit)))

(defun q-push (q &optional (value nil value-supplied-p))
  "Propagate numerical value."
  (when value-supplied-p
    (setf (q-value q) value))
  ;; Convert numerical value into the display unit.
  (when (q-unit-dirty q)
    (pull-display-unit q)
    (setf (q-unit-dirty q) nil))
  (setf (q-display-value q) (when (q-value q)
			      (uconv (q-value q) (q-unit q) (q-display-unit q) (q-relative q))))
  (push-display-value q)
  (setf (q-value-dirty q) nil)
  (values))

(defun q-pull (q)
  "Query numerical value."
  (when (or (when (q-unit-dirty q)
	      (pull-display-unit q)
	      (setf (q-unit-dirty q) nil)
	      cl:t)
	    (when (q-value-dirty q)
	      (pull-display-value q)
	      (setf (q-value-dirty q) nil)
	      cl:t))
    (setf (q-value q) (when (q-display-value q)
			(uconv (q-display-value q) (q-display-unit q) (q-unit q) (q-relative q)))))
  (q-value q))

(defun make-q (app &rest init-arg)
  (let ((q (apply #'make-instance 'q-object init-arg)))
    ;; Gather display units from the drop-down menu widget.
    (setf (q-display-units q)
	  (iter (with tree-model = (gtk-combo-box-get-model (q-unit-widget q)))
		(for tree-iter :first (gtk-tree-model-get-iter-first tree-model)
			       :then (gtk-tree-model-iter-next tree-model tree-iter))
		(while tree-iter)
		(for index :from 0)
		(collect (cons index (first (gtk-tree-model-get tree-model tree-iter 0))))))
    ;; Set display unit.
    (unless (q-display-unit q)
      (setf (q-display-unit q) (q-unit q)))
    (push-display-unit q)
    ;; Set display value.
    (when (q-value q)
      (setf (q-value q) (float (q-value q))))
    (q-push q)
    ;; Install call-backs.
    (when (q-value-editable q)
      (let ((widget (q-value-widget q)))
	(g-signal-connect widget "changed"
			  (lambda (object)
			    (declare (ignore object))
			    (setf (q-value-dirty q) cl:t)))
	(let ((fun (lambda (object &rest arg)
		     (declare (ignore object arg))
		     (when (q-value-dirty q)
		       (update app)))))
	  (g-signal-connect widget "activate" fun)
	  (g-signal-connect widget "focus-out-event" fun))))
    (let ((widget (q-unit-widget q))
	  (fun (lambda (object)
		 (declare (ignore object))
		 (setf (q-unit-dirty q) cl:t)
		 (if (q-value-editable q)
		     (update app)
		   (q-push q)))))
      (g-signal-connect widget "changed" fun))
    q))

(defstruct (atmosphere-calculator
	    (:conc-name nil)
	    (:copier nil)
	    (:predicate nil))
  (builder
   (make-instance 'gtk-builder))
  main-window
  file-quit
  help-about
  ;; Ambient pressure.
  ambient-pressure-selection
  static-pressure-input-flag
  static-pressure-input-value
  static-pressure-input-unit
  static-pressure-input
  geometric-height-input-flag
  geometric-height-input-value
  geometric-height-input-unit
  geometric-height-input
  geopotential-height-input-flag
  geopotential-height-input-value
  geopotential-height-input-unit
  geopotential-height-input
  ;; Ambient temperature.
  ambient-temperature-selection
  static-temperature-input-flag
  static-temperature-input-value
  static-temperature-input-unit
  static-temperature-input
  temperature-offset-input-flag
  temperature-offset-input-value
  temperature-offset-input-unit
  temperature-offset-input
  ;; Air speed.
  air-speed-selection
  true-air-speed-input-flag
  true-air-speed-input-value
  true-air-speed-input-unit
  true-air-speed-input
  equivalent-air-speed-input-flag
  equivalent-air-speed-input-value
  equivalent-air-speed-input-unit
  equivalent-air-speed-input
  calibrated-air-speed-input-flag
  calibrated-air-speed-input-value
  calibrated-air-speed-input-unit
  calibrated-air-speed-input
  mach-number-input-flag
  mach-number-input-value
  mach-number-input-unit
  mach-number-input
  ;; International standard atmosphere.
  static-pressure-result-value
  static-pressure-result-unit
  static-pressure-result
  geometric-height-result-value
  geometric-height-result-unit
  geometric-height-result
  geopotential-height-result-value
  geopotential-height-result-unit
  geopotential-height-result
  static-temperature-result-value
  static-temperature-result-unit
  static-temperature-result
  temperature-offset-result-value
  temperature-offset-result-unit
  temperature-offset-result
  ;; Air properties.
  density-result-value
  density-result-unit
  density-result
  speed-of-sound-result-value
  speed-of-sound-result-unit
  speed-of-sound-result
  ;; Air speeds.
  true-air-speed-result-value
  true-air-speed-result-unit
  true-air-speed-result
  equivalent-air-speed-result-value
  equivalent-air-speed-result-unit
  equivalent-air-speed-result
  calibrated-air-speed-result-value
  calibrated-air-speed-result-unit
  calibrated-air-speed-result
  mach-number-result-value
  mach-number-result-unit
  mach-number-result
  dynamic-pressure-result-value
  dynamic-pressure-result-unit
  dynamic-pressure-result
  ;; About dialog.
  about-dialog
  about-close)

(define-condition invalid-input (simple-error)
  ()
  (:documentation "Condition type for an invalid input value."))

(defun calc (app)
  (let (p h H t T dT r a TAS EAS CAS M q)
    ;; Clear results.
    (q-push (static-pressure-result app) nil)
    (q-push (geometric-height-result app) nil)
    (q-push (geopotential-height-result app) nil)
    (q-push (static-temperature-result app) nil)
    (q-push (temperature-offset-result app) nil)
    (q-push (density-result app) nil)
    (q-push (speed-of-sound-result app) nil)
    (q-push (true-air-speed-result app) nil)
    (q-push (equivalent-air-speed-result app) nil)
    (q-push (calibrated-air-speed-result app) nil)
    (q-push (mach-number-result app) nil)
    (q-push (dynamic-pressure-result app) nil)
    (handler-case
	(progn
	  (ecase (ambient-pressure-selection app)
	    (:static-pressure
	     (when (setf p (q-pull (static-pressure-input app)))
	       (setf H (iso-2533:pressure-altitude p)
		     h (iso-2533:geometric-altitude H))))
	    (:geometric-height
	     (when (setf h (q-pull (geometric-height-input app)))
	       (setf H (iso-2533:geopotential-altitude h)
		     p (iso-2533:atm H))))
	    (:geopotential-height
	     (when (setf H (q-pull (geopotential-height-input app)))
	       (setf h (iso-2533:geometric-altitude H)
		     p (iso-2533:atm H)))))
	  (unless (and p h H)
	    (error 'invalid-input))
	  (ecase (ambient-temperature-selection app)
	    (:static-temperature
	     (when (setf t (q-pull (static-temperature-input app)))
	       (setf T (kelvin-from-degree-celsius t)
		     dT (- T (nth-value 1 (iso-2533:atm H))))))
	    (:temperature-offset
	     (when (setf dT (q-pull (temperature-offset-input app)))
	       (setf T (+ (nth-value 1 (iso-2533:atm H)) dT)
		     t (degree-celsius-from-kelvin T)))))
	  (unless (and t T dT)
	    (error 'invalid-input))
	  (unless (plusp T)
	    (error "Ambient temperature out of range (it can't get that cold)')."))
	  (let (k pn Tn rn an SMOE)
	    ;; Ratio of specific heat capacities of air.
	    (setf k iso-2533:ratio-of-specific-heats)
	    ;; Standard air pressure at sea level in Pa.
	    (setf pn iso-2533:standard-pressure)
	    ;; Standard air temperature at sea level in K.
	    (setf Tn iso-2533:standard-temperature)
	    ;; Standard air density at sea level in kg/m³.
	    (setf rn iso-2533:standard-density)
	    ;; Speed of sound at sea level in m/s.
	    (setf an (iso-2533:speed-of-sound Tn))
	    ;; Air density at ambient conditions in kg/m³.
	    (setf r (iso-2533:density p T))
	    ;; Speed of sound at ambient conditions in m/s.
	    (setf a (iso-2533:speed-of-sound T))
	    ;; Standard means of evaluation.
	    (setf SMOE (/ (sqrt (/ r rn))))
	    (ecase (air-speed-selection app)
	      (:true-air-speed
	       (when (setf TAS (q-pull (true-air-speed-input app)))
		 (setf EAS  (/ TAS SMOE))))
	      (:equivalent-air-speed
	       (when (setf EAS (q-pull (equivalent-air-speed-input app)))
		 (setf TAS  (* EAS SMOE))))
	      (:calibrated-air-speed
	       (setf CAS (q-pull (calibrated-air-speed-input app))))
	      (:mach-number
	       (setf M (q-pull (mach-number-input app)))))
	    (cond ((and TAS EAS)
		   ;; Mach number.
		   (setf M (/ TAS a))
		   ;; Dynamic pressure.
		   (let ((c (/ (- k 1) 2))
			 (m (/ k (- k 1))))
		     (setf q (* p (- (expt (+ 1 (* c (expt M 2))) m) 1))))
		   ;; Calibrated air speed.
		   (let ((c (/ 2 (- k 1)))
			 (m (/ (- k 1) k)))
		     (setf CAS (* an (sqrt (* c (- (expt (+ (/ q pn) 1) m) 1)))))))
		  ((or CAS M)
		   ;; Dynamic pressure.
		   (let ((c (/ (- k 1) 2))
			 (m (/ k (- k 1))))
		     (setf q (if CAS
				 (* pn (- (expt (+ 1 (* c (expt (/ CAS an) 2))) m) 1))
			       (* p (- (expt (+ 1 (* c (expt M 2))) m) 1)))))
		   ;; Mach number.
		   (unless M
		     (let ((c (/ 2 (- k 1)))
			   (m (/ (- k 1) k)))
		       (setf M (sqrt (* c (- (expt (+ (/ q pn) 1) m) 1))))))
		   ;; Calibrated air speed.
		   (unless CAS
		     (let ((c (/ 2 (- k 1)))
			   (m (/ (- k 1) k)))
		       (setf CAS (* an (sqrt (* c (- (expt (+ (/ q pn) 1) m) 1)))))))
		   ;; True air speed.
		   (setf TAS (* M a))
		   ;; Equivalent air speed.
		   (setf EAS (/ TAS SMOE)))
		  (cl:t
		   (error 'invalid-input)))
	    (unless (<= 0 M 1)
	      (error "Air speed out of range.")))
	  (values))
      (:no-error ()
	(q-push (static-pressure-result app) p)
	(q-push (geometric-height-result app) h)
	(q-push (geopotential-height-result app) H)
	(q-push (static-temperature-result app) t)
	(q-push (temperature-offset-result app) dT)
	(q-push (density-result app) r)
	(q-push (speed-of-sound-result app) a)
	(q-push (true-air-speed-result app) TAS)
	(q-push (equivalent-air-speed-result app) EAS)
	(q-push (calibrated-air-speed-result app) CAS)
	(q-push (mach-number-result app) M)
	(q-push (dynamic-pressure-result app) q)
	))))

(defun plot (app)
  (declare (ignore app))
  (values))

(defun update (app)
  (handler-case
      (progn
	(calc app)
	(plot app))
    (error (c)
      (format nil "Error: ~S~%" c))))

(defun atmosphere-calculator ()
  (let (app)
    (within-main-loop
      (setf app (make-atmosphere-calculator))
      ;; Create the objects of the user interface.
      (gtk-builder-add-from-file (builder app) "atmosphere-calculator.glade")
      (gtk-builder-add-from-file (builder app) "atmosphere-calculator-about.glade")
      ;; Gather widgets handles.
      (iter (for slot :in (closer-mop:class-slots (class-of app)))
	    (for slot-name = (closer-mop:slot-definition-name slot))
	    (for name = (string-downcase (symbol-name slot-name)))
	    (for widget = (gtk-builder-get-object (builder app) name))
	    (when widget (setf (slot-value app slot-name) widget)))
      ;; About dialog.
      (g-signal-connect (about-dialog app) "delete-event"
			(lambda (object &rest arg)
			  (declare (ignore object arg))
			  (gtk-widget-hide-on-delete (about-dialog app))))
      (g-signal-connect (help-about app) "activate"
			(lambda (object)
			  (declare (ignore object))
			  (gtk-widget-show-all (about-dialog app))))
      (g-signal-connect (about-close app) "clicked"
			(lambda (object)
			  (declare (ignore object))
			  (gtk-widget-hide (about-dialog app))))
      ;; Install call-backs.
      (let ((quit (lambda (object)
		    (declare (ignore object))
		    (leave-gtk-main))))
	(g-signal-connect (main-window app) "destroy" quit)
	(g-signal-connect (file-quit app) "activate" quit))
      (g-signal-connect (static-pressure-input-flag app) "clicked"
			(lambda (object)
			  (declare (ignore object))
			  (setf (ambient-pressure-selection app) :static-pressure
				(gtk-widget-sensitive (static-pressure-input-value app)) cl:t
				(gtk-widget-sensitive (static-pressure-input-unit app)) cl:t
				(gtk-widget-sensitive (geometric-height-input-value app)) nil
				(gtk-widget-sensitive (geometric-height-input-unit app)) nil
				(gtk-widget-sensitive (geopotential-height-input-value app)) nil
				(gtk-widget-sensitive (geopotential-height-input-unit app)) nil)
			  (update app)))
      (g-signal-connect (geometric-height-input-flag app) "clicked"
			(lambda (object)
			  (declare (ignore object))
			  (setf (ambient-pressure-selection app) :geometric-height
				(gtk-widget-sensitive (static-pressure-input-value app)) nil
				(gtk-widget-sensitive (static-pressure-input-unit app)) nil
				(gtk-widget-sensitive (geometric-height-input-value app)) cl:t
				(gtk-widget-sensitive (geometric-height-input-unit app)) cl:t
				(gtk-widget-sensitive (geopotential-height-input-value app)) nil
				(gtk-widget-sensitive (geopotential-height-input-unit app)) nil)
			  (update app)))
      (g-signal-connect (geopotential-height-input-flag app) "clicked"
			(lambda (object)
			  (declare (ignore object))
			  (setf (ambient-pressure-selection app) :geopotential-height
				(gtk-widget-sensitive (static-pressure-input-value app)) nil
				(gtk-widget-sensitive (static-pressure-input-unit app)) nil
				(gtk-widget-sensitive (geometric-height-input-value app)) nil
				(gtk-widget-sensitive (geometric-height-input-unit app)) nil
				(gtk-widget-sensitive (geopotential-height-input-value app)) cl:t
				(gtk-widget-sensitive (geopotential-height-input-unit app)) cl:t)
			  (update app)))
      (g-signal-connect (static-temperature-input-flag app) "clicked"
			(lambda (object)
			  (declare (ignore object))
			  (setf (ambient-temperature-selection app) :static-temperature
				(gtk-widget-sensitive (static-temperature-input-value app)) cl:t
				(gtk-widget-sensitive (static-temperature-input-unit app)) cl:t
				(gtk-widget-sensitive (temperature-offset-input-value app)) nil
				(gtk-widget-sensitive (temperature-offset-input-unit app)) nil)
			  (update app)))
      (g-signal-connect (temperature-offset-input-flag app) "clicked"
			(lambda (object)
			  (declare (ignore object))
			  (setf (ambient-temperature-selection app) :temperature-offset
				(gtk-widget-sensitive (static-temperature-input-value app)) nil
				(gtk-widget-sensitive (static-temperature-input-unit app)) nil
				(gtk-widget-sensitive (temperature-offset-input-value app)) cl:t
				(gtk-widget-sensitive (temperature-offset-input-unit app)) cl:t)
			  (update app)))
      (g-signal-connect (true-air-speed-input-flag app) "clicked"
			(lambda (object)
			  (declare (ignore object))
			  (setf (air-speed-selection app) :true-air-speed
				(gtk-widget-sensitive (true-air-speed-input-value app)) cl:t
				(gtk-widget-sensitive (true-air-speed-input-unit app)) cl:t
				(gtk-widget-sensitive (equivalent-air-speed-input-value app)) nil
				(gtk-widget-sensitive (equivalent-air-speed-input-unit app)) nil
				(gtk-widget-sensitive (calibrated-air-speed-input-value app)) nil
				(gtk-widget-sensitive (calibrated-air-speed-input-unit app)) nil
				(gtk-widget-sensitive (mach-number-input-value app)) nil
				(gtk-widget-sensitive (mach-number-input-unit app)) nil)
			  (update app)))
      (g-signal-connect (equivalent-air-speed-input-flag app) "clicked"
			(lambda (object)
			  (declare (ignore object))
			  (setf (air-speed-selection app) :equivalent-air-speed
				(gtk-widget-sensitive (true-air-speed-input-value app)) nil
				(gtk-widget-sensitive (true-air-speed-input-unit app)) nil
				(gtk-widget-sensitive (equivalent-air-speed-input-value app)) cl:t
				(gtk-widget-sensitive (equivalent-air-speed-input-unit app)) cl:t
				(gtk-widget-sensitive (calibrated-air-speed-input-value app)) nil
				(gtk-widget-sensitive (calibrated-air-speed-input-unit app)) nil
				(gtk-widget-sensitive (mach-number-input-value app)) nil
				(gtk-widget-sensitive (mach-number-input-unit app)) nil)
			  (update app)))
      (g-signal-connect (calibrated-air-speed-input-flag app) "clicked"
			(lambda (object)
			  (declare (ignore object))
			  (setf (air-speed-selection app) :calibrated-air-speed
				(gtk-widget-sensitive (true-air-speed-input-value app)) nil
				(gtk-widget-sensitive (true-air-speed-input-unit app)) nil
				(gtk-widget-sensitive (equivalent-air-speed-input-value app)) nil
				(gtk-widget-sensitive (equivalent-air-speed-input-unit app)) nil
				(gtk-widget-sensitive (calibrated-air-speed-input-value app)) cl:t
				(gtk-widget-sensitive (calibrated-air-speed-input-unit app)) cl:t
				(gtk-widget-sensitive (mach-number-input-value app)) nil
				(gtk-widget-sensitive (mach-number-input-unit app)) nil)
			  (update app)))
      (g-signal-connect (mach-number-input-flag app) "clicked"
			(lambda (object)
			  (declare (ignore object))
			  (setf (air-speed-selection app) :mach-number
				(gtk-widget-sensitive (true-air-speed-input-value app)) nil
				(gtk-widget-sensitive (true-air-speed-input-unit app)) nil
				(gtk-widget-sensitive (equivalent-air-speed-input-value app)) nil
				(gtk-widget-sensitive (equivalent-air-speed-input-unit app)) nil
				(gtk-widget-sensitive (calibrated-air-speed-input-value app)) nil
				(gtk-widget-sensitive (calibrated-air-speed-input-unit app)) nil
				(gtk-widget-sensitive (mach-number-input-value app)) cl:t
				(gtk-widget-sensitive (mach-number-input-unit app)) cl:t)
			  (update app)))
      (setf (static-pressure-input app)
	    (make-q app
		    :tag "static pressure"
		    :value 101325 :unit "Pa" :display-unit "hPa"
		    :value-editable cl:t
		    :value-widget (static-pressure-input-value app)
		    :unit-widget (static-pressure-input-unit app))
	    (geometric-height-input app)
	    (make-q app
		    :tag "geometric height"
		    :value 0 :unit "m"
		    :value-editable cl:t
		    :value-widget (geometric-height-input-value app)
		    :unit-widget (geometric-height-input-unit app))
	    (geopotential-height-input app)
	    (make-q app
		    :tag "geopotential height"
		    :value 0 :unit "m"
		    :value-editable cl:t
		    :value-widget (geopotential-height-input-value app)
		    :unit-widget (geopotential-height-input-unit app))
	    (static-temperature-input app)
	    (make-q app
		    :tag "static temperature"
		    :value 15 :unit "°C"
		    :value-editable cl:t
		    :value-widget (static-temperature-input-value app)
		    :unit-widget (static-temperature-input-unit app))
	    (temperature-offset-input app)
	    (make-q app
		    :tag "temperature offset"
		    :value 0 :unit "K" :relative cl:t
		    :value-editable cl:t
		    :value-widget (temperature-offset-input-value app)
		    :unit-widget (temperature-offset-input-unit app))
	    (true-air-speed-input app)
	    (make-q app
		    :tag "true air speed"
		    :value 0 :unit "m/s" :display-unit "kn"
		    :value-editable cl:t
		    :value-widget (true-air-speed-input-value app)
		    :unit-widget (true-air-speed-input-unit app))
	    (equivalent-air-speed-input app)
	    (make-q app
		    :tag "equivalent air "
		    :value 0 :unit "m/s" :display-unit "kn"
		    :value-editable cl:t
		    :value-widget (equivalent-air-speed-input-value app)
		    :unit-widget (equivalent-air-speed-input-unit app))
	    (calibrated-air-speed-input app)
	    (make-q app
		    :tag "calibrated air speed"
		    :value 0 :unit "m/s" :display-unit "kn"
		    :value-editable cl:t
		    :value-widget (calibrated-air-speed-input-value app)
		    :unit-widget (calibrated-air-speed-input-unit app))
	    (mach-number-input app)
	    (make-q app
		    :tag "Mach number"
		    :value 0 :unit "1"
		    :value-editable cl:t
		    :value-widget (mach-number-input-value app)
		    :unit-widget (mach-number-input-unit app))
	    ;; Result values.
	    (static-pressure-result app)
	    (make-q app
		    :tag "static pressure"
		    :unit "Pa" :display-unit "hPa"
		    :value-editable nil
		    :value-widget (static-pressure-result-value app)
		    :unit-widget (static-pressure-result-unit app))
	    (geometric-height-result app)
	    (make-q app
		    :tag "geometric height"
		    :unit "m"
		    :value-editable nil
		    :value-widget (geometric-height-result-value app)
		    :unit-widget (geometric-height-result-unit app))
	    (geopotential-height-result app)
	    (make-q app
		    :tag "geopotential height"
		    :unit "m"
		    :value-editable nil
		    :value-widget (geopotential-height-result-value app)
		    :unit-widget (geopotential-height-result-unit app))
	    (static-temperature-result app)
	    (make-q app
		    :tag "static temperature"
		    :unit "°C"
		    :value-editable nil
		    :value-widget (static-temperature-result-value app)
		    :unit-widget (static-temperature-result-unit app))
	    (temperature-offset-result app)
	    (make-q app
		    :tag "temperature offset"
		    :unit "K" :relative cl:t
		    :value-editable nil
		    :value-widget (temperature-offset-result-value app)
		    :unit-widget (temperature-offset-result-unit app))
	    (density-result app)
	    (make-q app
		    :tag "density"
		    :unit "kg/m³"
		    :value-editable nil
		    :value-widget (density-result-value app)
		    :unit-widget (density-result-unit app))
	    (speed-of-sound-result app)
	    (make-q app
		    :tag "speed of sound"
		    :unit "m/s"
		    :value-editable nil
		    :value-widget (speed-of-sound-result-value app)
		    :unit-widget (speed-of-sound-result-unit app))
	    (true-air-speed-result app)
	    (make-q app
		    :tag "true air speed"
		    :unit "m/s"
		    :value-editable nil
		    :value-widget (true-air-speed-result-value app)
		    :unit-widget (true-air-speed-result-unit app))
	    (equivalent-air-speed-result app)
	    (make-q app
		    :tag "equivalent air speed"
		    :unit "m/s"
		    :value-editable nil
		    :value-widget (equivalent-air-speed-result-value app)
		    :unit-widget (equivalent-air-speed-result-unit app))
	    (calibrated-air-speed-result app)
	    (make-q app
		    :tag "calibrated air speed"
		    :unit "m/s"
		    :value-editable nil
		    :value-widget (calibrated-air-speed-result-value app)
		    :unit-widget (calibrated-air-speed-result-unit app))
	    (mach-number-result app)
	    (make-q app
		    :tag "Mach number"
		    :unit "1"
		    :value-editable nil
		    :value-widget (mach-number-result-value app)
		    :unit-widget (mach-number-result-unit app))
	    (dynamic-pressure-result app)
	    (make-q app
		    :tag "dynamic pressure"
		    :unit "Pa" :display-unit "hPa"
		    :value-editable nil
		    :value-widget (dynamic-pressure-result-value app)
		    :unit-widget (dynamic-pressure-result-unit app)))
      ;; Fire the radio-button call-backs.
      (gtk-button-clicked (geometric-height-input-flag app))
      (gtk-button-clicked (temperature-offset-input-flag app))
      (gtk-button-clicked (calibrated-air-speed-input-flag app))
      ;; Map main window.
      (gtk-widget-show-all (main-window app)))
    (join-gtk-main)))

(in-readtable :standard)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (restore-special-variables))

;;;; Standalone application.

(defconst +PROGRAM+ "atmosphere-calculator"
  "Official name of the program.")

(defconst +VERSION+ "1.0"
  "Version number of the program.")

(defconst +ADDRESS+ (format nil "<~A@~A>" "rs" "ralph-schleicher.de")
  "Mail address or URL for reporting bugs.")

(defparameter *show-version* nil
  "Non-null means to print the version number.")

(defparameter *show-help* nil
  "Non-null means to print the help text.")

(defun show-version (&optional (stream *standard-output*))
  "Display version number information."
  (format stream "~
~A ~A

Copyright (C) 2014 Ralph Schleicher

This program is free software and distributed under the modified
BSD License.  There is NO warranty; not even for MERCHANTABILITY
or FITNESS FOR A PARTICULAR PURPOSE.~%"
	  +PROGRAM+ +VERSION+))

(defun show-help (&optional (stream *standard-output*))
  "Display help text."
  (format stream "~
Usage: ~A [OPTION...]

Calculate ambient conditions and air speeds.

  --version             Display version number information.
  --help                Display this help text.

Report bugs to ~A~%"
	  (program-invocation-short-name) +ADDRESS+))

(export 'main)
(defun main (&rest arguments)
  "Program entry point."
  (declare (ignore arguments))
  ;; This is a standalone application and
  ;; not an interactive Lisp process.
  (standalone-program)
  ;; Get options and arguments.
  (let ((opt (make-getopt `(("version"
			     :action *show-version*)
			    ("help"
			     :action *show-help*))
			  :help "--help")))
    (when (getopt opt)
      (show-help-hint-and-die opt))
    ;; Check for help.
    (when (or *show-version* *show-help*)
      (fresh-line)
      (when *show-version*
	(show-version))
      (when *show-help*
	(when *show-version*
	  (terpri)
	  (terpri))
	(show-help))
      (exit-success))
    ;; Check remaining arguments.
    (when (remaining-arguments opt)
      (say "Too many command line arguments")
      (show-help-hint-and-die opt)))
  ;; Run the actual program.
  (atmosphere-calculator)
  (exit-success))

;;; atmosphere-calculator.lisp ends here
