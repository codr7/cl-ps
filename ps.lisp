(defpackage ps
  (:use cl)
  (:export))

(in-package ps)

(defvar *actions* nil)
(defvar *state* nil)
(defvar *ops* nil)

(defstruct op
  (action nil)
  (pre nil)
  (add nil)
  (rem nil))

(defun solve (goals)
  (let (*actions*)
    (if (every #'achieve goals)
	(values t (nreverse *actions*))
	(values nil (nreverse *actions*)))))

(defun achieve (goal)
  (or (member goal *state*)
      (some #'apply-op
	    (find-all goal *ops* :test #'appropriate?))))

(defun appropriate? (goal op)
  (member goal (op-add op)))

(defun apply-op (op)
  (when (every #'achieve (op-pre op))
    (push (op-action op) *actions*)
    (setf *state* (set-difference *state* (op-rem op)))
    (setf *state* (union *state* (op-add op)))
    t))

(defmacro define-solver (name (&rest args) &body body)
  (declare (ignore args))
  (flet ((parse-op (f)
	   `(make-op :action ',(pop f)
		     :pre ',(pop f)
		     :add ',(pop f)
		     :rem ',(pop f))))
    `(defun ,name (*state* goals)
       (let ((*ops* (list ,@(mapcar #'parse-op body))))
	 (solve goals)))))

(define-solver school-test-solver ()
  (drive-son-to-school (son-at-home car-works)
   (son-at-school)
   (son-at-home))
  (shop-installs-battery (car-needs-battery shop-knows-problem shop-has-money)
   (car-works))
  (tell-shop-problem (in-communication-with-shop)
   (shop-knows-problem))
  (telephone-shop (know-phone-number)
   (in-communication-with-shop))
  (look-up-number (have-phone-book)
   (know-phone-number))
  (give-shop-money (have-money)
   (shop-has-money)
   (have-money)))

(define-solver yak-test-solver ()
  (wax-car (car-needs-waxing have-hose)
	   (car-waxed)
	   (car-needs-waxing))
  (get-new-hose (have-ez-pass)
		(have-hose))
  (borrow-ez-pass (return-mooshi-pillow)
		  (have-ez-pass))
  (fix-mooshi-pillow (have-yak)
		     (return-mooshi-pillow))
  (visit-zoo ()
	     (have-yak)))

(defun test-school ()
  (multiple-value-bind (ok? actions)
      (school-test-solver '(son-at-home car-needs-battery have-money have-phone-book)
			  '(son-at-school))
    (assert ok?)
    (assert (equal actions
		   '(LOOK-UP-NUMBER
		     TELEPHONE-SHOP
		     TELL-SHOP-PROBLEM
		     GIVE-SHOP-MONEY
		     SHOP-INSTALLS-BATTERY
		     DRIVE-SON-TO-SCHOOL)))))

(defun test-yak ()
  (multiple-value-bind (ok? actions)
      (yak-test-solver '(car-needs-waxing) '(car-waxed))
    (assert ok?)
    (assert (equal actions
		   '(VISIT-ZOO
		     FIX-MOOSHI-PILLOW
		     BORROW-EZ-PASS
		     GET-NEW-HOSE
		     WAX-CAR)))))

(defun test ()
  (test-school)
  (test-yak))
