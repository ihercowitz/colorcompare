(ns colorcompare.core
  (:require [org.httpkit.server   :refer :all]
            [compojure.core       :refer :all]
            [compojure.route      :as route]
            [cheshire.core        :refer [generate-string]]
            [clojure.java.io      :as io]
            [ring.middleware.params         :refer [wrap-params]]
            [ring.middleware.keyword-params :refer [wrap-keyword-params]]
            [ring.middleware.json :refer [wrap-json-response
                                          wrap-json-params]]))

(defn exp2 [v]
  (* v v))

(defn dbl-hex [v]
  (apply str (repeat 2 v)))

(defn hex-to-int [hex]
  (Integer/parseInt hex 16))

(defn hex-to-rgb [hex]
  (let [v (clojure.string/replace hex #"#" "")
        c (if (= (count v) 6)
            [(subs v 0 2) (subs v 2 4) (subs v 4 6)]
            [(dbl-hex (subs v 0 1)) (dbl-hex (subs v 1 2)) (dbl-hex (subs v 2 3))])]
    (map #(hex-to-int %) c)))

(defn rgb-color-distance [c1 c2]
  (->> (map #(exp2 (- %2 %1)) c1 c2)
       (apply +)
       Math/sqrt
       Math/round))


(defn fixture []
  (let [gcolor (fn [] (repeatedly 3 #(rand-int 255)))
        colors (repeatedly 100 #(gcolor))
        next-white (->> colors
                        (map-indexed (fn [idx item] [idx (rgb-color-distance [255 255 255] item)]))
                        (sort #(compare (last %1) (last %2)))
                        (map #(assoc {} :color (nth colors (first %)))))]
    next-white))



(defroutes rotas
  (GET "/" request (-> "public/index.html" io/resource io/file slurp))
  (GET "/colors" [] (generate-string (fixture)))
  (route/resources "/"))


(defn -main [& args]
  (println "Running on 8080")
  (run-server (-> rotas 
                  wrap-params
                  wrap-keyword-params
                  wrap-json-params
                  wrap-json-response) {:port (Integer. (or (System/getenv "PORT") "8080"))}))
