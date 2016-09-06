(ns training.turtles.example
  (:require [training.turtles [core :refer :all]
                              [util :refer :all]]))

(init)

;;Creative Commons Attribution 4.0 International (CC BY 4.0)
;;Adopted from ClojureBridge version, inlined for ease of
;;distribution.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; On NightCode, click Run with REPL followed by Reload for the
;; first time. If REPL has already started, click Reload to run
;; this code.
;;
;; Once a turtle (small triangle) appears in the center of window,
;; type code in REPL.
;;
;; Alternatively, you may type code in this editor. To evaluate
;; whole file, click Reload.
;;
;; [Exercise]
;; write some functions under these comment lines
;; and evalute those
;;
;; for example
;; (forward 30)
;; (right 90)
;; (forward 30)
;; (right 90)
;;
;; see how the turtle walks
;;

;;Make the turtle walk in random directions
(defn random-walk []
  )

;;Make the turtle draw a square.
(defn square []
   )
;;Make the turtle draw a triangle.
(defn triangle []
  )
;;Make the turtle draw a circle.
(defn circle []
  )
;;Make the turtle draw a sine wave
(defn wave []
   )

;;Turtles Redux
;;=============
;;There are a few commands our turtle now knows.
;;It can look-at an [x y] coordinate.
;;It can trace-path a sequence of [x y] coordinates.

;;Do these new commands, combined with a newfound knowledge
;;of clojure's sequence operations and math functions help?


;;If so, try the following:

;;Generate a sequence of points that outlines a triangle
(defn tri [] )

;;Define a function that scales any sequence of points by
;;a scale-x and a scale-y factor.
(defn scale [scale-x scale-y pts]
  )

;;define a function that translates any sequence of points
;;by a tx and a ty amount
(defn translate [tx ty pts]
  )

;;Make the turtle draw any triangle, centered at
;;the point [-100 -100], with an identical triangle
;;outside of it.  Try to use the preceding functions
;;to transform the points that describe the triangle.

;;Since our turtle knows how to follow points,
;;trying reading points from the
;;file "points.txt" and trace the result.
