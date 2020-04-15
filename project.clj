(defproject choices "0.1.0"
  :description "Fetch COVID-19 FAQs data"
  :url ""
  :license {:name "Eclipse Public License - v 2.0"
            :url  "http://www.eclipse.org/legal/epl-v20.html"}
  :dependencies [[org.clojure/clojure "1.10.1"]
                 [clj-http "3.10.0"]
                 ;; [org.martinklepsch/clj-http-lite "0.4.3"]
                 [hickory "0.7.1"]
                 [cheshire "5.10.0"]
                 [hiccup "1.0.5"]
                 [clojure.java-time "0.3.2"]]
  :clean-targets ^{:protect false} ["target" "public"]
  :aliases {"run" ["trampoline" "run" "-m" "core"]}
  )
