(defproject choices "0.1.0"
  :description "Fetch COVID-19 FAQs data"
  :url ""
  :license {:name "Eclipse Public License - v 2.0"
            :url  "http://www.eclipse.org/legal/epl-v20.html"}
  :dependencies [[org.clojure/clojure "1.10.1"]
                 [org.clojure/core.async "1.1.587"]
                 [jarohen/chime "0.3.2"]
                 [hickory "0.7.1"]
                 [cheshire "5.10.0"]
                 [hiccup "1.0.5"]
                 [clojure.java-time "0.3.2"]
                 [borkdude/babashka "0.0.75"]
                 [metosin/pohjavirta "0.0.1-alpha7"]
                 [metosin/jsonista "0.2.5"]
                 [metosin/reitit      "0.4.2"]
                 [metosin/reitit-ring "0.4.2"]
                 [metosin/reitit-middleware "0.4.2"]
                 [ring-cors "0.1.13"]]
  :clean-targets ^{:protect false} ["target" "public"]
  :aliases {"run" ["trampoline" "run" "-m" "core"]}
  )
