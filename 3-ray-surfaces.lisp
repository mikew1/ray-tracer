;;;;;;;;;;;;;;;;;;;;
;;  Ray Surfaces  ;;
;;;;;;;;;;;;;;;;;;;;

(defstruct (sphere (:include surface))                      ; [1]
  radius center)

(defun defsphere (x y z r c)                                ; [2]
  (let ((s (make-sphere
             :radius r
             :center (make-point :x x :y y :z z)
             :color c)))
    (push s *world*)
    s))

(defun intersect (s pt xr yr zr)                            ; [3]
  (funcall (typecase s (sphere #'sphere-intersect))
           ;(typecase s (cube #'cube-intersect))
           s pt xr yr zr))

(defun sphere-intersect (s pt xr yr zr)                     ; [4]
  (let* ((c (sphere-center s))
         (n (minroot (+ (sq xr) (sq yr) (sq zr))
                     (* 2 (+ (* (- (x pt) (x c)) xr)
                             (* (- (y pt) (y c)) yr)
                             (* (- (z pt) (z c)) zr)))
                     (+ (sq (- (x pt) (x c)))
                        (sq (- (y pt) (y c)))
                        (sq (- (z pt) (z c)))
                        (- (sq (sphere-radius s)))))))
    (if n
        (make-point :x (+ (x pt) (* n xr))
                    :y (+ (y pt) (* n yr))
                    :z (+ (z pt) (* n zr))))))

(defun normal (s pt)                                        ; [5]
  (funcall (typecase s (sphere #'sphere-normal))
           s pt))

(defun sphere-normal (s pt)                                 ; [6]
  (let ((c (sphere-center s)))
    (unit-vector (- (x c) (x pt))
                 (- (y c) (y pt))
                 (- (z c) (z pt)))))

;; [1] Include struct surface, which just adds the property color.
;; [2] Add a sphere to the world.
;; [3] Find the intersect to whatever the surface is.
;;     So long as we have only spheres, can just read this fn as sphere-intersect.
;;     It checks type & calls corresponding function.
;; [4] See book.
;; [5] Find the normal to whatever the surface is.
;; [6] For sphere, this is unit vector from point to center.