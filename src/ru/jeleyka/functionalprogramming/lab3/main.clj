(ns ru.jeleyka.functionalprogramming.lab3.main
  (:require [clojure.string :as str]
            [clojure.tools.cli :refer [parse-opts]]
            [ru.jeleyka.functionalprogramming.lab3.interpolation :refer [interpolate lagrange-interpolate linear-interpolate]]))

(def algorithms
  {"linear"   {:interpolate linear-interpolate :required-points 2}
   "lagrange" {:interpolate lagrange-interpolate :required-points 4}})

(def cli-options
  [["-a" "--algorithm NAME" "Algorithm"
    :id :algorithms
    :multi true
    :default []
    :update-fn #(conj %1 (str/lower-case %2))
    :validate [#(contains? algorithms %) (str "Possible algorithms: " (str/join ", " (keys algorithms)) ")")]]
   ["-s" "--step NUMBER" "Step"
    :id :step
    :default 1.0
    :parse-fn #(Double/parseDouble %)]
   ["-h" "--help"]])


(defn validate-args [args]
  (let [{:keys [options _ errors summary]} (parse-opts args cli-options)]
    (cond
      (:help options)
      {:exit-message ((str/join \newline ["Options:" summary]))}

      errors
      {:exit-message (str "The following errors occurred while parsing your command:\n" (str/join \newline errors))}

      (empty? (:algorithms options))
      {:exit-message "At least one algorithm name is required."}

      :else
      {:options options})))

(defn- find-max-window-size [algorithm-names]
  (apply max (map #(get-in algorithms [% :required-points]) algorithm-names)))

(defn- print-values [key result]
  (println (str/join "\t" (map #(str/replace (key %) "," ".") result))))

(defn -main [& args]
  (let [{:keys [options exit-message]} (validate-args args)]
    (if exit-message
      (println exit-message)
      (let [max-window-size (find-max-window-size (:algorithms options))]
        (loop [coords []]
          (let [input (read-line)
                parsed (re-matches #"([+-]?\d*\.?\d+)\s+([+-]?\d*\.?\d+)" input)]
            (if parsed
              (let [x (Double/parseDouble (nth parsed 1))
                    y (Double/parseDouble (nth parsed 2))
                    new-coords (conj coords [x y])]
                (doseq [algorithm-name (:algorithms options)]
                  (let [required-points (get-in algorithms [algorithm-name :required-points])]
                    (when (>= (count new-coords) required-points)
                      (let [result (interpolate new-coords (:step options) required-points (get-in algorithms [algorithm-name :interpolate]))]
                        (println (str algorithm-name " interpolation result:"))
                        (print-values first result)
                        (print-values second result)))))
                (if (> (count new-coords) max-window-size)
                  (recur (subvec new-coords 1))
                  (recur new-coords)))
              (println "Stopping."))))))))