(ns core
  (:require [cheshire.core :as json]
            ;; [clj-http.lite.client :as http]
            [clj-http.client :as http]
            [clojure.string :as s]
            [hickory.core :as h]
            [hickory.convert :as hc]
            [hickory.select :as hs]
            [hiccup.core :as hi]
            [java-time :as t]
            [clojure.zip :as z]
            [babashka.curl :as curl])
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
    (map (fn [qr] {:q (hi/html (last (first qr)))
                   :r (hi/html (second qr))
                   :s "URSSAF"
                   :u url
                   :m date})
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
                     {:q question
                      :r (hi/html (hc/hickory-to-hiccup (second e)))
                      :s "Pôle emploi"
                      :u url
                      :m date}))
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
                     {:q (s/trim question)
                      :r (s/join "<br/>" (map #(hi/html (hc/hickory-to-hiccup %)) (rest e)))
                      :s "Gouvernement"
                      :u url
                      :m date}))
                 (map flatten (partition 2 (partition-by :attrs parsed)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Parse education.gouv.fr

(def education-url "https://www.education.gouv.fr/coronavirus-covid-19-informations-et-recommandations-pour-les-etablissements-scolaires-et-les-274253")

;; FIXME: use hs/find-in-text?
(defn is-a-question? [e]
  (when-let [s (not-empty (hi/html (hc/hickory-to-hiccup e)))]
    (or (nth (re-matches #"^(<[^>]+>)?(.*\?\s*)(<[^>]+>)?$" s) 2)
        (re-matches #"^.*Annonce.*$" s))))

(defn scrap-education [url]
  (let [hs-question-selector (hs/and (hs/tag "h3") (hs/class "title"))
        parsed
        (-> (scrap url)
            h/parse
            h/as-hickory
            (as-> s (hs/select
                     (hs/or hs-question-selector
                            (hs/follow hs-question-selector (hs/tag "p")))
                     s)))]
    (remove nil?
            (map (fn [e]
                   (when-let [question (not-empty (hi/html (hc/hickory-to-hiccup (first e))))]
                     {:q (nth (re-matches #"^(<[^>]+>)?(.*\?\s*)(<[^>]+>)?$" question) 2)
                      :r (s/join "<br/>" (map #(hi/html (hc/hickory-to-hiccup %)) (rest e)))
                      :s "Ministère de l'Éducation nationale"
                      :u url
                      :m date}))
                 (map flatten
                      (partition
                       2 (partition-by is-a-question? parsed)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Parse https://travail-emploi.gouv.fr

(def travailemploi-url "https://travail-emploi.gouv.fr/actualites/l-actualite-du-ministere/article/coronavirus-questions-reponses-pour-les-entreprises-et-les-salaries")

(defn travailemploi-entity [e url date]
  {:q (try (nth (re-matches #"^(<[^>]+>)?(.*\?\s*)(<[^>]+>)?$"
                            (hi/html (hc/hickory-to-hiccup (first e)))) 2)
           (catch Exception _ "ERREUR"))
   :r (try (hi/html
            (hc/hickory-to-hiccup
             (z/node (z/right (z/right (z/up e))))))
           (catch Exception _ "ERREUR"))
   :s "Ministère du Travail"
   :u url
   :m date})

(defn scrap-travailemploi [url]
  (let [parsed
        (-> (scrap url)
            h/parse
            h/as-hickory
            (as-> s (hs/select-locs
                     (hs/and (hs/tag "strong")
                             (hs/find-in-text #"^.*\?\s*$"))
                     s)))]
    (map #(travailemploi-entity % url date) parsed)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Parse https://www.associations.gouv.fr

(def associations-url "https://www.associations.gouv.fr/associations-et-crise-du-covid-19-la-foire-aux-questions.html")

(defn associations-entity [e url date]
  {:q (try (nth (re-matches #"^(<[^>]+>)?(.*\?\s*)(<[^>]+>)?$"
                            (hi/html (hc/hickory-to-hiccup (first e)))) 2)
           (catch Exception _ "ERREUR"))
   :r (try (hi/html
            (hc/hickory-to-hiccup
             (z/node (z/right (z/right (z/up e))))))
           (catch Exception _ "ERREUR"))
   :s "MENJ - Associations"
   :u url
   :m date})

(defn scrap-associations [url]
  (let [parsed
        (-> (scrap url)
            h/parse
            h/as-hickory
            (as-> s (hs/select-locs
                     (hs/and (hs/tag "strong")
                             (hs/find-in-text #"^.*\?\s*$"))
                     s)))]
    (map #(associations-entity % url date) parsed)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Parse https://handicap.gouv.fr

(def handicap-url "https://handicap.gouv.fr/grands-dossiers/coronavirus/article/foire-aux-questions")

(defn handicap-entity [e url date]
  {:q (try (nth (re-matches #"^(<[^>]+>)?(.*\?\s*)(<[^>]+>)?$"
                            (hi/html (hc/hickory-to-hiccup (first e)))) 2)
           (catch Exception _ "ERREUR"))
   :r (try (hi/html
            (hc/hickory-to-hiccup
             (z/node (z/right (z/right (z/up e))))))
           (catch Exception _ "ERREUR"))
   :s "Secrétariat d'État / Handicap"
   :u url
   :m date})

(defn scrap-handicap [url]
  (let [parsed
        (-> (scrap url)
            h/parse
            h/as-hickory
            (as-> s (hs/select-locs
                     (hs/and (hs/tag "strong")
                             (hs/find-in-text #"^.*\?\s*$"))
                     s)))]
    (map #(handicap-entity % url date) parsed)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Parse https://www.etudiant.gouv.fr

(def etudiant-url "https://www.etudiant.gouv.fr/pid33626-cid150278/covid-19-%7C-faq-crous-etudes-concours-services.html")

(defn scrap-etudiant [url]
  (let [hs-question-selector (hs/and (hs/tag "h4") (hs/find-in-text #"^.*\?\s*$"))
        parsed
        (-> (curl/get url) ;; Cf. 419 status from http/get
            h/parse
            h/as-hickory
            (as-> s (hs/select
                     (hs/or hs-question-selector
                            (hs/follow hs-question-selector (hs/tag "p")))
                     s)))]
    (remove nil?
            (map (fn [e]
                   (when-let [question (not-empty (hi/html (hc/hickory-to-hiccup (first e))))]
                     {:q (nth (re-matches #"^(<[^>]+>)?(.*\?\s*)(<[^>]+>)?$" question) 2)
                      :r (s/join "<br/>" (map #(hi/html (hc/hickory-to-hiccup %)) (rest e)))
                      :s "MESRI / Les Crous"
                      :u url
                      :m date}))
                 (map flatten
                      (partition
                       2 (partition-by is-a-question? parsed)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Put it all together

(defn -main []
  (let [
        urssaf        (scrap-urssaf urssaf-url)
        pole-emploi-1 (scrap-pole-emploi pole-emploi-url-1)
        pole-emploi-2 (scrap-pole-emploi pole-emploi-url-2)
        pole-emploi-3 (scrap-pole-emploi pole-emploi-url-3)
        gouvernement  (scrap-gouvernement gouvernement-url)
        education     (scrap-education education-url)
        travailemploi (scrap-travailemploi travailemploi-url)
        associations  (scrap-associations associations-url)
        handicap      (scrap-handicap handicap-url)
        etudiant      (scrap-etudiant etudiant-url)
        ]
    (spit "public/faq.json"
          (json/generate-string
           (map-indexed (fn [idx itm] (merge itm {:i idx}))
                        (concat
                         urssaf
                         pole-emploi-1
                         pole-emploi-2
                         pole-emploi-3
                         gouvernement
                         education
                         travailemploi
                         associations
                         handicap
                         etudiant
                         ))
           true))))

;; (-main)
