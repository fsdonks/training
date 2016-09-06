
;;Enhanced utilities to lend a more functional
;;style to directing turtles.  The focus is on
;;creating sequence of instructions for
;;a "turtle interpreter" to evaluate, thus
;;causing side-effects of movement.  Lots of
;;2d math utilities have been added to aid in
;;outlining paths in 2d coordinates under the
;;familiar "turtle" vocabulary.
(ns training.turtles.util
  (:use training.turtles.core))
;;utils

;;we can tighten our bounds by adding more decimals.
(def ^:constant +epsilon+ 0.0001)
(def +north+ [0 1])

(defn f= [x y]
  (< (Math/abs (- x y)) +epsilon+))

(defn fzero? [x]
  (< (Math/abs  x) +epsilon+))

(defn clamp [x] (if (fzero? x) 0.0 x))
                        
(defn restart []  (do (clean) (home)))

(defn random-turn []
  (if (< (rand) 0.5)
    (left (rand-int  365))
    (right (rand-int  365))))

(defn random-step []
  (if (< (rand) 0.5)
    (forward  (rand-int 30))
    (backward (rand-int 100))))

;;accessor to help us get the model of our turtle.
(defn get-state
  ([nm] (let [data (get @turtles nm)]
          (assoc data :name nm)))         
  ([] (get-state :trinity)))

(defn current-coords [] ((juxt :x :y) (get-state)))

(defn face [dir]
  (let [cd (:angle (get-state))
        ld (-  cd dir)]
    (left ld)))

(defn degrees [rads] (* rads (/ 180.0  Math/PI )))
(defn rads    [degs] (* degs (/ Math/PI 180.0)))

(defn slope->direction [dx dy]  
  (cond (and (not (fzero? dx)) (not (fzero? dy)))
          (degrees (Math/atan (double (/ dx dy))))
          (not (fzero? dx))
          (if (pos? dx) 0
               180)
          :else
          (if (pos? dy) 90
              270)))

;;Simple affine transformation for an x,y coord.
;;This makes our computations a bit easier...
;;We already have the vector.
(defn rotate-xy
  ([theta x y]
   (let [cx (Math/cos theta)
         sx (Math/sin theta)]
     [(clamp (+ (* cx  x)   (* (- sx) y)))
      (clamp (+ (* sx  x)  (* cx y )))]))
  ([theta v] (rotate-xy theta (first v) (second v))))

(defn unrotate-xy [theta x y]
  (rotate-xy (- theta) x y))
(defn untranslate-xy [dx dy x y]
  [(- x dx) (- y dy)])

;;2d dot-product.
(defn dot [u v]
  (let [[x1 y1] u
        [x2 y2] v]
    (+ (* x1 x2) (* y1 y2))))
(defn v- [[x1 y1] [x2 y2]]
  [(- x2 x1) (- y2 y1)])
(defn v+ [[x1 y1] [x2 y2]]
  [(+ x2 x1) (+ y2 y1)])
;;pythagorean theorem / length of a vector.
(defn v-norm [v]  (Math/sqrt (dot v v)))
;;use the identity (u dot v) / (norm u * norm v) = cos theta_uv
;;thus, theta_uv = cos-1 ((u dot v)  / (norm u  * norm v))
;;note: if two vectors are parellel, then the angle between them
;;is 0.
(defn  vector-radians [u v]
  (let [norms (*   (v-norm u)
                   (v-norm v))]
    (if (not (zero? norms))    
      (Math/acos (/  (dot u v)
                     norms))
      0.0)))

;;yield the norm of the psuedovector resulting from a 2d cross product. 
(defn pseudo-cross [[u1 u2] [v1 v2]]
    (- (* u1 v2) (* u2 v1)))

;;is vector u oriented left of vector v
(defn left? [u v]
  (pos? (pseudo-cross u v)))
;;is vector u oriented right of vector v
(defn right? [u v]
  (neg? (pseudo-cross u v)))
(defn parallel? [u v]
  (zero? (pseudo-cross u v)))

(defn vector-degrees [u v] (degrees (vector-radians u v)))
(defn signed-vector-degrees [u v]
  (let [d (degrees (vector-radians u v))
        x1 (first u)
        x2 (first v)
        pc (pseudo-cross u v)]
    (cond (pos? pc) d
          (neg? pc) (- d)
          :else 0.0)))
        


;;transforms a vector into a new vector
;;relative to the current turtle...
;;Normally, we'd use matrices for these.  We can use
;;our vector operations to get the job done though.

;;aka global transform

(defn normal-vec [theta]
  [(clamp (Math/cos theta))
   (clamp (Math/sin theta))])

;;so, moving to a point is a matter of figuring out the
;;local degree change, as well as a distance.
;;Since distance doesn't change in affine transforms,
;;only our angles do.
;;parallel lines remain parallel.
;;Compute the angular difference between the vector xy,
;;in the global coordinate system, relative to the turtle's
;;coordinate system.  Return a heading in degrees and a
;;distance.
(defn compute-move
  ([theta basis v]
   (let [dir       (v- basis v)
         distance  (v-norm dir)
         direction (signed-vector-degrees (normal-vec theta) dir)
         ]
     {:direction   direction
      :distance   distance}))
  ([v]
   (let [{:keys [x y angle]} (get-state)]
     (compute-move (rads angle) [x y] v))))
  
;;after we compute moves, all we need to do is interpret pos/neg moves into
;;commands...
(defn move->command [{:keys [direction distance]}]
  (let [dir    (if (pos? direction) :move-left :move-right)
        angle   (case dir
                  :move-left direction
                  (- direction))]
  {:move-forward   distance
   dir angle}))

(defn random-move [dist]
  (let [hw (/ dist 2.0)]
    [(- (* (rand) dist) hw)
     (- (* (rand) dist) hw)]))

;;commands are now data...
;;we can script the behavior of our turtles.
(defn simple-random-walk [& {:keys [dist] :or {dist 10}}]
  (->> (fn [] (random-move dist))
       (repeatedly)
       (map compute-move)))

;;we'll encode moves as analogues to their
;;imperative functions.
(defn do-command [{:keys [name move-left move-right move-forward move-backward]
                   :or {name :trinity}}]
  (do ;;execute our turns first, so we face the right direction.
      (when move-left     (left move-left))
      (when move-right    (right move-right))
      ;;take steps to accomplish the move.
      (when move-forward  (forward move-forward))
      (when move-backward (backward move-backward))))

(defn do-move [xy]
  (do-command 
   (move->command 
    (compute-move xy))))

(defn interpolate
  ([from to stepsize dist]  
    (if (> stepsize dist) ;;we can arrive at to.
      [to to (- stepsize dist) 0] ;;return the new point, plus residual steps.
      (let [nxt (v+ from [stepsize stepsize])]
        [nxt to stepsize (- dist stepsize)])))
  ([from to stepsize]
   (interpolate from to (v-norm (v- from to)))))

(defn interpolations [from to stepsize]
  (let [dir   (v- from to)
        dist  (v-norm dir)
        steps (/ dist stepsize)
        [dx dy] dir
        dx (/ dx dist)
        dy (/ dy dist)
        step  [(* dx stepsize) (* dy stepsize)]
        ]
    (if (zero? (rem steps 1.0))
      (take (inc steps )
            (iterate (fn [current]
                       (v+ current step)) from))
      ;;uneven steps.
      ;;where does that leave us?
      (let [basic-steps (quot steps 1)
            partial-step   (rem steps 1) ;;the last bit.
            residual      (* (- 1.0 partial-step) stepsize)
            ]
        (concat 
         (take steps (iterate (fn [current]
                                (v+ current step)) from))
         [{:residual residual
           :step      to}])))))

(defn shift [x y]
  (do (swap! turtles
             update :trinity
             #(merge % {:x x :y y}))))

(defn look-at [coordinate]
  (-> (compute-move coordinate)
      (assoc :distance 0)
      (move->command )
      (do-command )))
     
(defn interpolate-path [points stepsize]
  (->> points
       (partition 2 1)
       (mapcat (fn [[from to]]
                 (interpolations from to stepsize)))
       (map (fn [x] (if (map? x) (:step x) x)))))
               
(defn trace-path [points & {:keys [step-size delay] :or {step-size 2 delay 16}}]
  (let [init (atom nil)
        _    (clean)]
    (doseq [p  points]
      (do  (when (not @init)
             (reset! init p)
             (shift (first p) (second p)))
           (do-move p)
           (when delay (Thread/sleep delay ))))))

(defn ->triangle [size]
  [[0 0]
   [size 0]
   [(/ size 2.0) size]
   [0  0]])

(defn ->translate [x y xs]
  (map #(v+ [x y] %) xs))

(defn ->follow [points & {:keys [delay] :or {delay 16}}]
  (doseq [x points]
    (do (when delay (Thread/sleep delay))
        (shift (first x) (second x)))
  ))

(defn cos-samples []
  (->> (range 0.0 1.0 0.01)
       (map-indexed (fn [x y]
                      [(* x 200)
                       (* y 10)]))
       ))
                       
