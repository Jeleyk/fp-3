(ns ru.jeleyka.functionalprogramming.lab3.interpolation)

(defn linear-interpolate [points x]
  (let [[p0 p1] (take-last 2 points)
        t (/ (- x (first p0)) (- (first p1) (first p0)))]
    (+ (second p0) (* t (- (second p1) (second p0))))))

(defn- lagrange-coefficient [points i x]
  (reduce
    (fn [acc j]
      (if (= i j)
        acc
        (* acc
           (- x (first (nth points j)))
           (/ 1 (- (first (nth points i)) (first (nth points j)))))))
    1
    (range (count points))))

(defn lagrange-interpolate [points x]
  (reduce
    (fn [result i]
      (+ result (* (second (nth points i)) (lagrange-coefficient points i x))))
    0
    (range (count points))))

(defn- steps [x-min x-max step]
  (let [sequence (take-while #(<= % x-max) (iterate #(+ % step) x-min))
        last-value (+ (last sequence) step)]
    (if (= (last sequence) x-max)
      (vec sequence)
      (conj (vec sequence) last-value))))

(defn interpolate [points step window-size interpolate]
  (let [window (take-last window-size points)
        x-min (first (first window))
        x-max (first (last window))]
    (mapv (fn [x] [x (-> (interpolate window x))]) (steps x-min x-max step))))
