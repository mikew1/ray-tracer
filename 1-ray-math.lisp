;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Math utils for ray tracer ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun sq (x) (* x x))

(defun mag (x y z)                                      ; [1]
  (sqrt (+ (sq x) (sq y) (sq z))))

(defun unit-vector (x y z)                              ; [2]
  (let ((d (mag x y z)))
    (values (/ x d) (/ y d) (/ z d))))

(defstruct (point (:conc-name nil))                     ; [3]
  x y z)

(defun distance (p1 p2)                                 ; [4]
  (mag (- (x p1) (x p2))
       (- (y p1) (y p2))
       (- (z p1) (z p2))))

(defun minroot (a b c)                                  ; [5]
  (if (zerop a)
      (/ (- c) b)
      (let ((disc (- (sq b) (* 4 a c))))
        (unless (minusp disc)
          (let ((discrt (sqrt disc)))
            (min (/ (+ (- b) discrt) (* 2 a))
                 (/ (- (- b) discrt) (* 2 a))))))))

;; [1] Magnitude of a vector is the square root of the sum of its squared components.
;; [2] To get the unit vector one divides each component by the magnitide.
;;     This returns three separate values, not a compound datastructure of any kind.
;;     usage e.g. (multiple-value-call #'mag (unit-vector 23 12 47)) => 1.0
;; [3] :conc-name of nil gives access functions of merely (x p) (y p) (z p),
;;     not point-x, point-y & point-z, i.e. 'point-' being the default.
;;     super neat, almost dangerously so, but we have that flexibility.
;; [4] To get the (Euclidean) distance we subtract one point from another
;;     component-wise, then take the magnitude.
;;     (setf p1 (make-point :x 1 :y 2 :z 3))
;;     (setf p2 (make-point :x -1 :y 7 :z 5))
;;     (distance p1 p2) => 5.7445626
;;     (distance p2 p1) => 5.7445626
;; [5] Find the roots then return the min. Familiar eqn (-b +/- sqrt(b^2 - 4ac)) / 2a
;;     Here we're only interested in the min of the two.
;;     Why will be explained later when we explain the tracer.
;;     The zerop case is for when not a quadratic, i.e. y = bx+c.