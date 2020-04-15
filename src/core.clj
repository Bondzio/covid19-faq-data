(ns core
  (:require [cheshire.core :as json]
            ;; [clj-http.lite.client :as http]
            [clj-http.client :as http]
            [clojure.string :as s]
            [hickory.core :as h]
            [hickory.convert :as hc]
            [hickory.select :as hs]
            [hiccup.core :as hi]
            [java-time :as t])
  (:gen-class))

(def http-get-params {:cookie-policy :standard :insecure? true})

(def date (str (t/local-date)))

(defn scrap [url]
  (try (:body (http/get url http-get-params))
       (catch Exception _ (println "Can't get HTML source"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Parse URSSAF FAQ

(def urssaf-url "https://www.urssaf.fr/portail/home/actualites/foire-aux-questions.html")

(defn scrap-urssaf [url]
  (let [parsed
        (-> (scrap url)
            h/parse
            h/as-hickory
            (as-> s (hs/select (hs/or (hs/class "faqQuestion")
                                      (hs/class "faqAnswer")) s)))
        parsed (map hc/hickory-to-hiccup parsed)]
    (map (fn [qr] {:question (hi/html (last (first qr)))
                   :answer   (hi/html (second qr))
                   :source   url
                   :updated  date})
         (partition 2 parsed))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Parse Pôle Emploi

(def pole-emploi-url-1 "https://www.pole-emploi.fr/actualites/information-covid-19.html")
(def pole-emploi-url-2 "https://www.pole-emploi.fr/actualites/covid-19-activite-partielle-et-a.html")
(def pole-emploi-url-3 "https://www.pole-emploi.fr/actualites/allongement-exceptionnel-de-lind.html")

(defn scrap-pole-emploi [url]
  (let [parsed
        (-> (scrap url)
            h/parse
            h/as-hickory
            (as-> s (hs/select (hs/class "block-article-link") s)))
        parsed (map (fn [e] (filter #(not (string? %)) (:content e))) parsed)]
    (remove nil?
            (map (fn [e]
                   (when-let [question (not-empty (first (:content (first e))))]
                     {:question question
                      :answer   (hi/html (hc/hickory-to-hiccup (second e)))
                      :source   url
                      :updated  date}))
                 parsed))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Parse gouvernement.fr

(def gouvernement-url "https://www.gouvernement.fr/info-coronavirus")

(defn scrap-gouvernement [url]
  (let [parsed
        (-> (scrap url)
            h/parse
            h/as-hickory
            (as-> s (hs/select (hs/or (hs/class "item-question")
                                      (hs/class "item-reponse")) s)))]
    (remove nil?
            (map (fn [e]
                   (when-let [question (not-empty (first (:content (first e))))]
                     {:question question
                      :answer   (hi/html (hc/hickory-to-hiccup (second e)))
                      :source   url
                      :updated  date}))
                 (partition 2 parsed)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Parse education.gouv.fr

(def education-url "https://www.education.gouv.fr/coronavirus-covid-19-informations-et-recommandations-pour-les-etablissements-scolaires-et-les-274253")

(defn scrap-education [url]
  (let [parsed
        (-> (scrap url)
            h/parse
            h/as-hickory
            (as-> s (hs/select (hs/or (hs/and (hs/tag "h3") (hs/class "title"))
                                      (hs/follow (hs/tag "h3") (hs/tag "p")))
                               s)))]
    (remove nil?
            (map (fn [e]
                   (when-let [question (not-empty (first (:content (first e))))]
                     {:question question
                      :answer   (hi/html (hc/hickory-to-hiccup (second e)))
                      :source   url
                      :updated  date}))
                 (partition 2 parsed)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Put it all together

(defn -main []
  (let [urssaf        (scrap-urssaf urssaf-url)
        pole-emploi-1 (scrap-pole-emploi pole-emploi-url-1)
        pole-emploi-2 (scrap-pole-emploi pole-emploi-url-2)
        pole-emploi-3 (scrap-pole-emploi pole-emploi-url-3)
        gouvernement  (scrap-gouvernement gouvernement-url)
        education     (scrap-education education-url)]
    (spit "public/faq.json"
          (json/generate-string
           (concat urssaf
                   pole-emploi-1
                   pole-emploi-2
                   pole-emploi-3
                   gouvernement
                   education)
           true))))
