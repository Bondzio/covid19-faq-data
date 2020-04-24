(ns core
  (:require [cheshire.core :as json]
            [clojure.string :as s]
            [hickory.core :as h]
            [hickory.convert :as hc]
            [hickory.select :as hs]
            [hiccup.core :as hi]
            [java-time :as t]
            [clojure.zip :as z]
            [babashka.curl :as curl])
  (:gen-class))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Variables and utility functions

(def date (str (t/local-date-time)))

(defn scrap-to-hickory [url]
  (try (-> (curl/get url {:raw-args ["-k"]})
           h/parse
           h/as-hickory)
       (catch Exception _
         (println "Can't get URL:" url))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Parse FAQs URSSAF

(def urssaf-url "https://www.urssaf.fr/portail/home/actualites/foire-aux-questions.html")

(defn urssaf-entity [e url]
  {:q (hi/html (last (first e)))
   :r (hi/html (second e))
   :s "URSSAF"
   :u url
   :m date})

(defn scrap-urssaf [url]
  (->> (scrap-to-hickory url)
       (hs/select
        (hs/or (hs/class "faqQuestion")
               (hs/class "faqAnswer")))
       (map hc/hickory-to-hiccup)
       (partition 2)
       (map #(urssaf-entity % url))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Parse FAQs from Pôle Emploi

(def pole-emploi-url-1 "https://www.pole-emploi.fr/actualites/information-covid-19.html")
(def pole-emploi-url-2 "https://www.pole-emploi.fr/actualites/covid-19-activite-partielle-et-a.html")
(def pole-emploi-url-3 "https://www.pole-emploi.fr/actualites/allongement-exceptionnel-de-lind.html")

(defn pole-emploi-entity [e url]
  (when-let [question (not-empty (first (:content (first e))))]
    {:q question
     :r (hi/html (hc/hickory-to-hiccup (second e)))
     :s "Pôle emploi"
     :u url
     :m date}))

(defn scrap-pole-emploi [url]
  (->> (scrap-to-hickory url)
       (hs/select (hs/class "block-article-link"))
       (map (fn [e] (filter #(not (string? %)) (:content e))))
       (map #(pole-emploi-entity % url))
       (remove nil?)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Parse FAQs from gouvernement.fr

(def gouvernement-url "https://www.gouvernement.fr/info-coronavirus")

(defn gouvernement-entity [e url]
  (when-let [question (not-empty (first (:content (first e))))]
    {:q (s/trim question)
     :r (s/join "<br/>" (map #(hi/html (hc/hickory-to-hiccup %)) (rest e)))
     :s "Gouvernement"
     :u url
     :m date}))

(defn scrap-gouvernement [url]
  (->> (scrap-to-hickory url)
       (hs/select (hs/or (hs/class "item-question")
                         (hs/class "item-reponse")))
       (partition-by :attrs)
       (partition 2)
       (map flatten)
       (map #(gouvernement-entity % url))
       (remove nil?)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Parse FAQs from education.gouv.fr

(def education-url "https://www.education.gouv.fr/coronavirus-covid-19-informations-et-recommandations-pour-les-etablissements-scolaires-et-les-274253")

(defn education-entity [e url]
  (when-let [q0 (not-empty (hi/html (hc/hickory-to-hiccup (first e))))]
    (when-let [q (nth (re-matches #"^(<[^>]+>)?(.*\?\s*)(<[^>]+>)?$" q0) 2)]
      {:q q
       :r (s/join "<br/>" (map #(hi/html (hc/hickory-to-hiccup %)) (rest e)))
       :s "Ministère de l'Éducation nationale"
       :u url
       :m date})))

(defn scrap-education [url]
  (->> (scrap-to-hickory url)
       (hs/select
        (hs/or (hs/and (hs/tag "h3") (hs/class "title"))
               (hs/tag "p")))
       (partition-by #(= "title" (:class (:attrs %))))
       (drop-while #(nil? (:attrs (first %))))
       (partition 2)
       (map flatten)
       (map #(education-entity % url))
       (remove nil?)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Parse FAQs from https://www.associations.gouv.fr

(def associations-url "https://www.associations.gouv.fr/associations-et-crise-du-covid-19-la-foire-aux-questions.html")

(defn associations-entity [e url]
  (when-let [q0 (not-empty (hi/html (hc/hickory-to-hiccup (first e))))]
    (when-let [q (nth (re-matches #"^(<[^>]+>)?(.*\?\s*)(<[^>]+>)?$" q0) 2)]
      {:q q
       :r (s/join "<br/>" (map #(hi/html (hc/hickory-to-hiccup %)) (rest e)))
       :s "MENJ - Associations"
       :u url
       :m date})))

(defn scrap-associations [url]
  (->> (scrap-to-hickory url)
       (hs/select
        (hs/or (hs/and (hs/tag "strong")
                       (hs/find-in-text #"^.*\?\s*$"))
               (hs/and (hs/tag "p")
                       (hs/not (hs/find-in-text #"^.*\?\s*$")))))
       (drop-while #(not (string? (first (:content %)))))
       (partition-by #(= (:tag %) :strong))
       (partition 2)
       (map flatten)
       (map #(associations-entity % url))
       (remove nil?)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Parse FAQs from https://travail-emploi.gouv.fr

(def travailemploi-url-prefix
  "https://travail-emploi.gouv.fr/le-ministere-en-action/coronavirus-covid-19/questions-reponses-par-theme/article/")

(def travailemploi-urls
  ["mesures-de-prevention-dans-l-entreprise-contre-le-covid-19-masques"
   "mesures-de-prevention-sante-hors-covid-19"
   "garde-d-enfants-et-personnes-vulnerables"
   "indemnisation-chomage"
   "formation-professionnelle-stagiaires-et-organismes-de-formation"
   "apprentissage-apprentis-et-organismes-de-formation-cfa"
   "activite-partielle-chomage-partiel"
   "adaptation-de-l-activite-conges-mise-a-disposition-de-main-d-oeuvre"
   "primes-exceptionnelles-et-epargne-salariale"
   "dialogue-social"
   "embauche-demission-sanctions-licenciement"
   "services-de-sante-au-travail"])

(defn travailemploi-entity [e url]
  (when-let [q0 (not-empty (hi/html (hc/hickory-to-hiccup (first e))))]
    (when-let [q (nth (re-matches #"^(<[^>]+>)?(.*\?\s*)(<[^>]+>)?$" q0) 2)]
      {:q q
       :r (s/join "<br/>" (map #(hi/html (hc/hickory-to-hiccup %)) (rest e)))
       :s "Ministère du Travail"
       :u url
       :m date})))

(defn scrap-travailemploi-url [url]
  (let [url (str travailemploi-url-prefix url)]
    (->> (scrap-to-hickory url)
         (hs/select
          (hs/or (hs/and (hs/tag "strong")
                         (hs/find-in-text #"^.*\?\s*$"))
                 (hs/and (hs/tag "p")
                         (hs/not (hs/find-in-text #"^.*\?\s*$")))))
         (drop-while #(not (= (:tag %) :strong)))
         (partition-by #(= (:tag %) :strong))
         (partition 2)
         (map flatten)
         (map #(travailemploi-entity % url))
         (remove nil?))))

(defn scrap-travailemploi []
  (flatten (map scrap-travailemploi-url travailemploi-urls)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Parse FAQs from https://handicap.gouv.fr

(def handicap-url "https://handicap.gouv.fr/grands-dossiers/coronavirus/article/foire-aux-questions")

(defn handicap-entity [e url]
  (when-let [q0 (not-empty (hi/html (hc/hickory-to-hiccup (first e))))]
    (when-let [q (nth (re-matches #"^(<[^>]+>)?(.*\?\s*)(<[^>]+>)?$" q0) 2)]
      {:q q
       :r (s/join "<br/>" (map #(hi/html (hc/hickory-to-hiccup %)) (rest e)))
       :s "Secrétariat d'État au handicap"
       :u url
       :m date})))

(defn scrap-handicap [url]
  (->> (scrap-to-hickory url)
       (hs/select
        (hs/or (hs/and (hs/tag "strong")
                       (hs/find-in-text #"^.*\?\s*$"))
               (hs/and (hs/tag "p")
                       (hs/not (hs/find-in-text #"^.*\?\s*$")))))
       (drop-while #(not (string? (first (:content %)))))
       (partition-by #(= (:tag %) :strong))
       (partition 2)
       (map flatten)
       (map #(handicap-entity % url))
       (remove nil?)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Parse FAQs from https://www.etudiant.gouv.fr

(def etudiant-url
  "https://www.etudiant.gouv.fr/pid33626-cid150278/covid-19-%7C-faq-crous-etudes-concours-services.html")

(defn is-a-question? [e]
  (when-let [s (not-empty (hi/html (hc/hickory-to-hiccup e)))]
    (nth (re-matches #"^(<[^>]+>)?(.*\?\s*)(<[^>]+>)?$" s) 2)))

(defn etudiant-entity [e url]
  (when-let [q0 (not-empty (hi/html (hc/hickory-to-hiccup (first e))))]
    (when-let [q (nth (re-matches #"^(<[^>]+>)?(.*\?\s*)(<[^>]+>)?$" q0) 2)]
      {:q q
       :r (s/join "<br/>" (map #(hi/html (hc/hickory-to-hiccup %)) (rest e)))
       :s "MESRI / Les Crous"
       :u url
       :m date})))

(defn scrap-etudiant [url]
  (->> (scrap-to-hickory url)
       (hs/select
        (hs/or (hs/and (hs/tag "h4") (hs/find-in-text #"^.*\?\s*$"))
               (hs/and (hs/tag "p")
                       (hs/not (hs/find-in-text #"^.*\?\s*$")))))
       (drop-while (fn [{:keys [content]}]
                     (not (and (string? (first content))
                               (re-matches #"^(<[^>]+>)?(.*\?\s*)(<[^>]+>)?$"
                                           (first content))))))
       (partition-by is-a-question?)
       (partition 2)
       (map flatten)
       (map #(etudiant-entity % url))
       (remove nil?)))

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
        travailemploi (scrap-travailemploi)
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
