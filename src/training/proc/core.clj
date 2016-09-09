(ns training.proc.core
  (:require ; [spork.util.excel [core :as xl]]
           ; [spork.util.table :as tbl]
           ; [spork.util.io    :as io]
           ; [spork.util.reducers]
           ; [spork.util.parsing :as parse]
           ; [spork.cljgui.components.swing :as swing]
            [clojure.core.reducers :as r]
            [training.proc.util :as util]
            [incanter.core :refer :all]
            [incanter.io :refer :all]
            [incanter.charts :refer :all]
            [incanter.stats]
          ;  [proc.patches :as patches]
          ;  [proc.schemas :as schemas]
            [proc.stacked :as stacked]
          ;  [spork.util.temporal :as temp]
          ;  [spork.sketch :as sketch]
            ))

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

(defn set-xticks [plot tick] 
  (.setTickUnit (.getDomainAxis (.getPlot plot)) 
                (org.jfree.chart.axis.NumberTickUnit. tick)))

(defn ys [xys] (map second xys))
(defn xs [xys] (map first xys))

(defn all-bounds [chrt]
  (let [plt    (.getXYPlot chrt)
        bound  (.getDatasetCount plt)
        xmin (atom 0)
        xmax (atom 0)]
  (doseq [ds (map #(.getDataset plt %) (range bound))]
    (reduce (fn [_ [nm s]]
              (do (swap! xmin min (.getMinX s))
                  (swap! xmax max (.getMaxX s))))
            nil
             (stacked/series-seq ds)))
  [@xmin @xmax]))

;;If we have an xyplot, it's stored in multiple datasets vs. 
;;a single datatable.
(defn add-trend-lines! 
  ([chrt aggfn]  
     (let [bnds (all-bounds chrt) ;(stacked/get-bounds chrt)
           plt  (.getXYPlot chrt)
           bound    (.getDatasetCount plt)]
       (doseq [ds (map #(.getDataset plt %) (range bound))
               [idx [nm ^XYSeries ser]] (map-indexed vector  (stacked/series-seq ds))]
          (let [xys  (stacked/xycoords ser)
                y    (aggfn xys)
                xmax (reduce max (xs xys))]
            (do (add-lines chrt bnds [y y] :series-label (str nm "Avg"))
                ;not currently working....
                ;(set-stroke chrt :data ds :series (inc idx) :width 4 :dash 10) 
            ))))
     chrt)
  ([chrt] (add-trend-lines! chrt (comp incanter.stats/mean ys))))

(def default-deploy-colors 
  {"AC"    (stacked/faded :blue 50)
   "NG"    (stacked/faded :red 50)
   "RC"    (stacked/faded :green 50)
   "ACAvg" :blue 
   "NGAvg" :red
   "RCAvg" :green
   "USARAvg" :green
   "USAR"  (stacked/faded :green 50)
   "Ghost" (stacked/faded :grey 50)})

(defn deployment-plot [ds src phase & {:keys [colors tickwidth] :or {colors  default-deploy-colors tickwidth 365} :as opts}]
  (let [ds     (util/as-dataset ds)
        title  (if (vector? src) (first src) src)
        groups (group-deployments ds src phase)
        [series d] (first groups)
        plt        (scatter-plot :DeployInterval :DwellYearsBeforeDeploy :series-label (:Component series) :legend true :data d 
                                 :title (str title " " phase " Dwell Before Deployment")
                                 :x-label "Time (days)"
                                 :y-label "Dwell (years)")]
    (doseq [[series d] (rest groups)]
      (add-points plt :DeployInterval :DwellYearsBeforeDeploy :series-label (:Component series) :legend true :data d))
    (set-xticks plt tickwidth)  
    (add-trend-lines! plt)
    (stacked/set-colors  plt colors)
    plt))
