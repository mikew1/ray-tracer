;;;;;;;;;;;;;;;;;;
;;  Ray Tracer  ;;
;;;;;;;;;;;;;;;;;;

(defstruct surface  color)                                               ; [1]

(defparameter *world* nil)                                               ; [2]
(defconstant eye (make-point :x 0 :y 0 :z 200))                          ; [3] <- to move eye,
                                                                         ;         change this.
(defun tracer (pathname &optional (res 1))                               ; [4]
  (with-open-file (p pathname :direction :output)
    (format p "P2 ~A ~A 255" (* res 100) (* res 100))                    ; [5]
    (let ((inc (/ res)))                                                 ; [6]
      (do ((y -50 (+ y inc)))                                            ; [7]
          ((< (- 50 y) inc))                                             ; [8]
        (do ((x -50 (+ x inc)))                                          ; [9]
            ((< (- 50 x) inc))
          (print (color-at x y) p))))))  ; <- all we need to do is print the pixel color.

(defun color-at (x y)                                                    ; [10]
  (multiple-value-bind (xr yr zr)
                       (unit-vector (- x (x eye))  ; <- dir. from eye to pixel. we'll trace
                                    (- y (y eye))  ;    back from eye to source, through pixel.
                                    (- 0 (z eye)))
                       (round (* (sendray eye xr yr zr) 255))))          ; [11]

(defun sendray (pt xr yr zr)                                             ; [12]
  (multiple-value-bind (s hit-pt) (first-hit pt xr yr zr)
    (if s                                                                ; [13]
        (multiple-value-bind (xs ys zs) (unit-vector 0 0 -200)           ; [14] <- to place light source
            (* (lambert s hit-pt xr yr zr) (surface-color s)))           ; [15]      elsewhere than at eye
        0)))                                                             ; [16]        (but see note 14).

(defun first-hit (pt xr yr zr)                                           ; [17]
  (let (surface hit-pt dist)  ; <- collect these vals for closest point  ; [18]
    (dolist (s *world*)                                                  ; [19]
      (let ((h (intersect s pt xr yr zr)))                               ; [20]
        (when h               ; <- current s was hit by the ray
          (let ((d (distance h pt)))                ; <- distance from hit-pt to eye
            (when (or (null dist) (< d dist))       ; <- if closest so far
              (setf surface s hit-pt h dist d)))))) ; <- update all hit pt vars
    (values surface hit-pt)))                                            ; [21]

(defun lambert (s int xs ys zs)                                          ; [22]
  (multiple-value-bind (xn yn zn) (normal s int)
    (max 0 (+ (* xs xn) (* ys yn) (* zs zn)))))

;; A Ray Tracer is the rendering algorithm deluxe; yields the best images, but takes the most time.
;; Benefits: easy to get real optical effects, defined using geometric objects, easy to implement.

;; This Ray Tracer
;; - black and white images
;; - single light source at same position as eye
;; - hence look like flash photographs
;; Hence, Optional Improvements
;; - move the light source    [x]
;; - add another light source [ ] (harder)


;; [1]  We'll program the tracer to this interface, then define specific surfaces later.
;;      All we need to establish that interface is surface-color, which here is a gray level.
;; [2]  We'll put surfaces in here and iterate through it to trace.
;; [3]  Eye is a point set back from the image plane; image plane is coincident with the x y plane.
;;      This means eye will only see objects which have negative z coordinates.
;;      Q. Why is an image plane alone not enough? Why do we need an eye if the eye is just a point?
;;      A. Find a window - on the glass surface is the image we'll construct. Walk toward then away
;;         away, and notice how the image changes. With eye closer to the image plane, more of the
;;         world is seen, which also means objects on the fixed size image (the glass) become smaller.
;;         If you move back, less of world is seen, & those objects which are seen become larger.
;;         Thus, a vantage point is necessary to allow us compute what to display. No vantage point, no image.
;;         An image is here constructed by considering all the rays of light passing from the world to
;;         the position of the eye, but is captured at the point those rays hit the image plane.
;;         Your eye itself is similar but has some differences. There, the vantage point, your retina, is fixed.
;;      Q. Must the eye always lie on the central axis?
;;      A. No. The plane is transparent. Movement of the eye in x y directions simply means you see
;;         different regions of the world, whereas z direction affects only image scale.
;;         Again, you can do this with a window, and think about the image on the glass pane.
;;      Note, the axes in the diagram in the book are wrong, need to reverse the directions,
;;      i.e. use negative values to get the directions indicated in the book on each axis.
;; [4]  Go pixel by pixel along the image plane, tracing the light back into the simulated world.
;;      Here we simply get the color of the pixel with color-at, and write that to a file.
;;      Hence, the entire rest of the program is in terms of 'getting the color of a pixel'.
;;      Calls color-at, which is all we need to find our way into the rest of the program.
;; [5]  Param res is image plane size in hundreds of pixels square.
;;      The filetype is a simple ascii format called pgm.
;;      The header is as per format here, P2 type, breadth, height & max int size per pixel.
;;      Hence, 100x100 will be 10k ints of max 256 (white).
;; [6]  (/ res) is same as (/ 1 res)
;; [7]  var init update; y from -50, incd by 1/res
;; [8]  test; at standard res, test passes until y reaches 50; then it fails, ending loop.
;; [9]  Same for each x within y; tracing an xy plane.

;; [10] To get a color at a point on the plane, we define a unit vector in the direction
;;      of that point on the plane from the eye, then send a ray in that direction.
;;      sendray will ultimately return a color. in ray tracing we trace back from the eye.
;;      multiple-value-bind binds the values returned from its first arg, for use later,
;;      like let; allows three values to be passed w/o any premature container.
;; [11] sendray returns a value in (0,1), here we round it to 8 bit, 0-255.
;; [12] Get the first hit surface or none, and the intersect point, and if a surface was hit,
;;      compute the amount of light to return & return it, else, return 0, for no light.
;;      Could call this from pixel position instead of eye position; should work same. try it.
;; [13] Surface was hit on way back to light source.
;; [14] If want to move light source to position other than at the eye, compute a unit vector
;;      as desired here and pass it to lambert. Otherwise, for light at the eye, pass xr yr zr.
;;      Vector must be from outside the plane, i.e. -ve z coord, or no reflection is possible
;;      (and here we only calculate reflection, not lightsource itself or transparency).
;;      *BUT*, while this roughly works, shouldn't lambert direction also depend on pixel?
;;      PROGRESS STEP: pencil & paper diagrams of what's happening.
;; [15] Amount of light is lambert coefficient times surface's intrinsic color; reflectivity.
;; [16] Background color, convention black.
;; [17] Get the surface if any our ray tracing back to the source hits first, along with point if so.
;;      Just like sendray, this takes point and direction. No transparency, hence want only first hit.
;;      This is a brute method of course.
;; [18] Initialise all three to nil. Dist to hold d during update, & be overwritten if smaller d found.
;; [19] To find first-hit, we use a slow brute method for now.
;;      Consider all surfaces in world.
;; [20] Intersect finds the solutions to the eqn. of that surface that also satisfy line of the ray.
;;      If ray goes in and out other side, there will be two roots. In that case, the min root is
;;      the nearest. Hence, intersect uses minroot. If another surface in world also intersects,
;;      it will only win if it's the nearer.
;; [21] Return closest hit point of closest surface, or nil nil if no hits.
;; [22] To get intensity of the ray, we use lamberts law. That means dot product
;;      of unit normal vector and the unit vector in direction of light source.
;;      Originaly program uses light source at eye, in which case use the ray itself.
;;      We also added an option to use a different ray, in [14] above.
;; [23] Wonder if this file might be neater with docstrings than comments, in time.