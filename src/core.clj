(ns core
  (:require [pohjavirta.server :as server]
            [jsonista.core :as j]
            [reitit.ring :as ring]
            [java-time :as t]
            [chime.core :as chime]
            [data :as data]
            [chime.core-async :refer [chime-ch]]
            [clojure.core.async :as async]
            [clojure.edn :as edn]
            [clojure.walk :as walk]
            [reitit.ring.middleware.parameters :as parameters]
            [ring.middleware.cors :refer [wrap-cors]]))

(defonce dev? true)
(defonce port 3000)
(defonce front-url "http://localhost:9500")

(def questions-ids
  (->> (j/read-value
        (slurp (str data/upload-dir "faq-questions.json")))
       (map #(select-keys % ["i"]))
       (map vals)
       flatten))

(def stats
  (atom
   (or (edn/read-string
        (try (slurp "stats.edn") (catch Exception _ nil)))
       (->> (map (fn [a] {a {:hits 0 :note {:count 0 :mean 0}}})
                 questions-ids)
            (into {})))))

(def store-stats-chan (async/chan))

(async/go
  (loop [stats-str (async/<! store-stats-chan)]
    (when (string? stats-str)
      (spit "stats.edn" stats-str))
    (recur (async/<! store-stats-chan))))

(add-watch
 stats :backup
 (fn [_ a _ _]
   (when (zero? (mod (apply + (map :hits (vals @a))) 10))
     (when dev? (println "Saving to stats.edn..."))
     (async/thread
       (async/>!! store-stats-chan (pr-str @a))))))

(def valid-tokens (atom {}))

(defn purge-tokens []
  (let [two-hours-ago (t/minus (t/instant) (t/hours 2))]
    (if (seq @valid-tokens)
      (do (reset! valid-tokens
                  (into {}
                        (filter
                         #(not= two-hours-ago
                                (t/max two-hours-ago
                                       (t/instant (last %))))
                         @valid-tokens)))
          (println "Tokens purged"))
      (println "Pas de token"))))

(defn wrap-headers [m]
  (assoc m :headers {"Content-Type" "application/json"}))

(defn get-token [_]
  (let [token (str (java.util.UUID/randomUUID))
        date  (str (t/instant))]
    (swap! valid-tokens conj {token date})
    (wrap-headers
     {:status 200
      :body   (j/write-value-as-string {:token token})})))

(defn get-stats [_]
  (wrap-headers
   {:status 200
    :body   (j/write-value-as-string @stats)}))

;; Reject answers when visit is < 5 secondes or > 1 hour
(defn valid-date? [date-token]
  (let [dt     (t/instant date-token)
        da     (t/instant)
        dt+10  (t/plus dt (t/seconds 5))
        dt+600 (t/plus dt (t/seconds 3600))]
    (and (= da (t/max dt+10 da))
         (= dt+600 (t/max dt+600 da)))))

(defn prn-resp [status msg]
  (when dev?
    (wrap-headers
     {:status status
      :body   (j/write-value-as-string
               {:response msg})})))

(defn hit [{params :query-params}]
  (let [params
        (walk/keywordize-keys params)
        {:keys [id token]} params]
    (if-let [date-token (get @valid-tokens token)]
      (if (valid-date? date-token)
        (do (swap! stats update-in [id :hits] inc)
            (prn-resp 200 "OK"))
        (prn-resp 400 "Invalid token"))
      (prn-resp 400 "Token not found"))))

(defn mean [old-mean old-cnt note]
  (float (/ (+ (* old-mean old-cnt) note)
            (inc old-cnt))))

(defn note [{params :query-params}]
  (let [params
        (walk/keywordize-keys params)
        {:keys [id token note]} params]
    (if-let [date-token (get @valid-tokens token)]
      (if (valid-date? date-token)
        (let [cnt  (get-in @stats [id :note :count])
              note (edn/read-string note)]
          (try (swap! stats update-in [id :note :count] inc)
               (swap! stats update-in [id :note :mean]
                      #(mean % cnt note))
               (prn-resp 200 "OK")
               (catch Exception _ nil)))
        (prn-resp 400 "Invalid token"))
      (prn-resp 400 "Token not found"))))

(def app
  (ring/ring-handler
   (ring/router
    [["/token" {:get get-token}]
     ["/stats" {:get get-stats}]
     ["/note" {:get note}]
     ["/hit" {:get hit}]])
   (ring/create-default-handler)
   {:middleware
    [parameters/parameters-middleware
     #(wrap-cors
       %
       :access-control-allow-origin [(re-pattern front-url)]
       :access-control-allow-headers ["Content-Type"]
       :access-control-allow-methods [:get :post])]}))

(defn start-tokens-purge-loop []
  (let [chimes (chime-ch (chime/periodic-seq
                          (t/instant) (t/hours 2)))]
    (async/go-loop []
      (when (async/<! chimes)
        (purge-tokens)
        (recur)))))

(defn -main [& [json]]
  (if json ;; Any value is OK
    (do
      (data/move-old-answers)
      (data/generate-json))
    (do
      (when dev? (start-tokens-purge-loop))
      (-> app (server/create {:port port}) server/start)
      (println "API started on localhost:3000"))))
