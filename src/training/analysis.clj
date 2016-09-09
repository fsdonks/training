(ns training.analysis
  (:require [training.proc [core :as proc]
                           [util :as util]
             [stacked :as stacked]]
            [spork.util [table :as tbl]
                        [clipboard :as clipboard]
                        [excel :as xl]]            
            [incanter.core :refer :all]
            [incanter.io :refer :all]
            [incanter.charts :refer :all]
            [incanter.stats]))


(defn ys [xys] (map second xys))
(defn xs [xys] (map first xys))

(defn group-deployments [ds src phase]
  (let [srcs (if (vector? src) 
                           (set (second src))
                           #{src})
        pred (fn [r] (and (srcs (:DemandType r))
                          (zero? (:FollowOnCount r))
                          (not= (:Demand r) "NotUtilized")
                          (if phase
                            (= (:Period r) phase)
                            true)))]
    ($group-by [:Component] ($where pred ds))))
;;Note: we need to standardize the plotting colors for these guys,
;;maybe define a custom theme for the series?
;;plotting
(defn view-deployments [ds src]  
  (let [ds     (util/as-dataset ds)
        title  (if (vector? src) (first src) src)
        groups (group-deployments ds src)
        [series d] (first groups)
        plt (scatter-plot :DeployInterval :DwellYearsBeforeDeploy :series-label (:Component series) :legend true :data d 
                          :title (str title " Dwell Before Deployment")
                          :x-label "Time (days)"
                          :y-label "Dwell (years)")]
    (doseq [[series d] (rest groups)]
      (add-points plt :DeployInterval :DwellYearsBeforeDeploy :series-label (:Component series) :legend true :data d))
    (view plt)))


