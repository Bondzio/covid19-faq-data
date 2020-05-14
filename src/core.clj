(ns core
  (:require [org.httpkit.server :as server]
            [jsonista.core :as j]
            [reitit.ring :as ring]
            [java-time :as t]
            [chime.core :as chime]
            [data :as data]
            [chime.core-async :refer [chime-ch]]
            [clojure.core.async :as async]
            [clojure.edn :as edn]
            [clojure.walk :as walk]
            [reitit.ring.middleware.muuntaja :as muuntaja]
            [ring.middleware.params :as params]
            [muuntaja.core :as m]
            [reitit.ring.middleware.parameters :as parameters]
            [ring.middleware.cors :refer [wrap-cors]]
            [mount.core :as mount]))

(defonce dev? false)
(defonce port 3000)

(defonce front-url
  (if dev?
    #"http://localhost:9500"
    #"https://www.covid19-faq.fr"))

(defonce spit-every-x-hits (if dev? 5 100))

(def questions-ids
  (->> (j/read-value
        (slurp (str data/upload-dir "faq-questions.json")))
       (map #(select-keys % ["i"]))
       (map vals)
       flatten))

(def stats
  (let [faq-ids
        (->> (map (fn [a] {a {:h 0 :n {:c 0 :m 0}}}) questions-ids)
             (into {}))
        stats-edn
        (edn/read-string
         (try (slurp "stats.edn")
              (catch Exception _ nil)))]
    (atom (merge (select-keys stats-edn (keys faq-ids)) faq-ids))))

(def store-stats-chan (async/chan))

(async/go
  (loop [stats-str (async/<! store-stats-chan)]
    (when (string? stats-str)
      (spit "stats.edn" stats-str))
    (recur (async/<! store-stats-chan))))

(add-watch
 stats :backup
 (fn [_ a _ _]
   (when (zero? (mod (apply + (map :h (vals @a)))
                     spit-every-x-hits))
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
      (println "No token to purge yet"))))

(defn get-token [_]
  (let [token (str (java.util.UUID/randomUUID))
        date  (str (t/instant))]
    (swap! valid-tokens conj {token date})
    {:status 200
     :body   {:token token}}))

(defn get-stats [_]
  {:status 200
   :body   @stats})

(defn valid-date? [date-token]
  (let [dt     (t/instant date-token)
        da     (t/instant)
        dt+min (t/plus dt (t/seconds 2))
        dt+max (t/plus dt (t/seconds 3600))]
    (and (= da (t/max dt+min da))
         (= dt+max (t/max dt+max da)))))

(defn prn-resp [status msg]
  {:status status
   :body   {:response msg}})

(defn hit [{params :query-params}]
  (let [params
        (walk/keywordize-keys params)
        {:keys [id token]} params]
    (if-let [date-token (get @valid-tokens token)]
      (if (valid-date? date-token)
        (do (swap! stats update-in [id :h] inc)
            (prn-resp 200 "OK"))
        (prn-resp 400 "Invalid token"))
      (prn-resp 400 "Token not found"))))

(defn mean [old-mean old-cnt note]
  (float (/ (+ (* old-mean old-cnt) note)
            (inc old-cnt))))

(defn note [{{:keys [id token note]} :body-params}]
  (if-let [date-token (get @valid-tokens token)]
    (if (valid-date? date-token)
      (let [cnt  (get-in @stats [id :n :c])
            note (edn/read-string note)]
        (swap! stats update-in [id :n :c] inc)
        (swap! stats update-in [id :n :m] #(mean % cnt note))
        (prn-resp 200 "OK"))
      (prn-resp 400 "Invalid token"))
    (prn-resp 400 "Token not found")))

(def handler
  (ring/ring-handler
   (ring/router
    [["/token" {:get get-token}]
     ["/stats" {:get get-stats}]
     ["/note" {:post note}]
     ["/hit" {:get hit}]]
    {:data {:muuntaja   m/instance
      	    :middleware [params/wrap-params
                         muuntaja/format-middleware]}})
   (ring/create-default-handler)
   {:middleware
    [parameters/parameters-middleware
     #(wrap-cors
       %
       :access-control-allow-origin [front-url]
       :access-control-allow-methods [:get :post])]}))

(defn start-tokens-purge-loop []
  (let [chimes (chime-ch (chime/periodic-seq
                          (t/instant) (t/hours 2)))]
    (async/go-loop []
      (when (async/<! chimes)
        (purge-tokens)
        (recur)))))

(mount/defstate s
  :start (do (println "API started on localhost:3000")
             (server/run-server handler {:port 3000}))
  :stop (when s (s :timeout 100)))

(defn -main [& [json]]
  (if json ;; Any value is OK
    (do (data/move-old-answers)
        (data/generate-json))
    (do (when dev? (start-tokens-purge-loop))
        (mount/start))))

(comment
  (mount/start)
  (mount/stop)
  )
