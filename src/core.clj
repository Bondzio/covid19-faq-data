(ns core
  (:require [pohjavirta.server :as server]
            [jsonista.core :as j]
            [reitit.ring :as ring]
            [java-time :as t]
            [chime.core :as chime]
            [chime.core-async :refer [chime-ch]]
            [clojure.core.async :as async]
            [clojure.edn :as edn]))

(def dev? true)

(def questions-ids
  (->> (j/read-value
        (slurp "docs/faq-questions.json"))
       (map #(select-keys % ["i"]))
       (map vals)
       flatten))

(def stats
  (atom (or (edn/read-string
             (try (slurp "stats.edn") (catch Exception _ nil)))
            (->> (map (fn [a] {a {:hits 0}})
                      questions-ids)
                 (into {})))))

(add-watch
 stats :backup
 (fn [_ a _ _]
   (when (zero? (mod (apply + (map :hits (vals @a))) 10))
     (spit "stats.edn" (pr-str @a)))))

(def valid-tokens (atom {}))

(defn purge-tokens []
  (let [two-hours-ago (t/minus (t/instant) (t/hours 2))]
    (reset! valid-tokens
            (filter #(not= two-hours-ago
                           (t/max two-hours-ago
                                  (t/instant (val (first %)))))
                    @valid-tokens))))

(defn get-token [_]
  (let [token (str (java.util.UUID/randomUUID))
        date  (str (t/instant))]
    (do (swap! valid-tokens conj {token date})
        {:status 200
         :body   (j/write-value-as-string
                  {:token token :date date})})))

(defn get-stats [_]
  {:status 200
   :body   (j/write-value-as-string
            (map (fn [[k v]]
                   (into {} (concat {:i k} v)))
                 @stats))})

;; Reject answers when visit is < 5 secondes or > 1 hour
(defn valid-date? [date-token date-answer]
  (let [dt     (t/instant date-token)
        da     (t/instant date-answer)
        dt+10  (t/plus dt (t/seconds 5))
        dt+600 (t/plus dt (t/seconds 3600))]
    (and (= da (t/max dt+10 da))
         (= dt+600 (t/max dt+600 da)))))

(defn hit [{{:keys [id token date]} :body-params}]
  (if-let [date-token (get @valid-tokens token)]
    (if (valid-date? date-token date)
      (do (swap! stats #(update-in % [id :hits] inc))
          {:status 200})
      {:status 400})
    {:status 400}))

(def app
  (ring/ring-handler
   (ring/router
    [["/token" {:get get-token}]
     ["/stats" {:get get-stats}]
     ["/hit" {:post hit}]])))

(defn start-tokens-purge-loop []
  (let [chimes (chime-ch (chime/periodic-seq
                          (t/instant) (t/seconds 5)))]
    (async/<!! (async/go-loop []
                 (when (async/<! chimes)
                   (purge-tokens)
                   (recur))))))

;; Testing
(comment
  (app {:request-method :post
        :uri            "/hit"
        :body-params    {:id    "uneid"
                         :token "751d2e65-c1e3-42d0-b38a-92320d623bf4"
                         :date  "2020-05-01T09:35:08.600263Z"}})
  (app {:request-method :get
        :uri            "/token"})
  (app {:request-method :get
        :uri            "/stats"})
  )

(defn -main []
  (if-not dev? (start-tokens-purge-loop))
  (-> app (server/create {:port 3000}) server/start))

