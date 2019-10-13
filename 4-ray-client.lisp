;;;;;;;;;;;;;;;;;;
;;  Ray Client  ;;
;;;;;;;;;;;;;;;;;;

(defun ray-test (&optional (res 1))                             ; [1]
  (setf *world* nil)
  (defsphere 0 -300 -1200 200 .8)
  (defsphere -80 -150 -1200 200 .7)
  (defsphere 70 -100 -1200 200 .9)
  (do ((x -2 (+ x 1)))                                          ; [2]
      ((> x 2))
    (do ((z 2 (+ z 1)))
        ((> z 7))
      (defsphere (* x 200) 300 (* z -400) 40 .75)))             ; [3]
  (tracer (make-pathname :name "spheres-shifted.pgm") res))


;; [1] res is hundreds of pixels square, e.g. 1 = 100x100, 2 = 200x200
;; [2] This sets up a grid of small spheres
;;     Recall syntax of do is var init update, test result.
;;     Hence, first do runs from -2 to 2, second from 2 to 7.
;; [3] All small spheres in this grid are of same size.
;;     They all have the same y coord, so are spread on the xz plane.

; Notes
; - lightsource in orig. code is always at point of eye
;   we modded that though to allow it to be moved.
; - "Not even optimised as a lisp program, let alone as a ray tracer"

; - FIXME: "merely adding type & inline declarations, s13.3, will make this program
;   more than twice as fast"

; - Ray tracing itself is amenable to parallelisation, tracing each ray is
;   computationally distinct.
; - Focus to start is on simplicity of program and understanding the geometry.

; Discussion
; this program has enabled us to understand more about optics
; because to get a believable image, we need to get the physics working right
; that's a pretty amazing thing to have at your fingertips.
; e.g. we need to simulate reflection correctly, a convincing image
; means we may have modelled it correctly. also, visual result lets us see,
; to some extent, that our algorithm is correct.

; "To solve a problem, describe it, specify it in algorithmic terms, implement it,
; test it, debug and analyze it. Expect this to be an iterative process. [p. 110]"
; "AI programming is largely exploratory programming; the aim is often to discover
; more about the problem area." < Norvig.

; Observations
; Welcome to world of lisp
; Programs are compact; hard part is the math;
; but given that the math is hard, lisp expresses it pretty nicely.
; program is not big, fewer LOC than in other langs.