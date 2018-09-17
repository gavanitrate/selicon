(ns selicon.macro
  (:require [clojure.string :as string]
            [clojure.java.io :as io]

            [hickory.select :as s]
            [hickory.render :as hr]
            [hickory.core :as h])
  (:import (java.io File)))

(def ss-file-sep "--")

(defn svg-file? [f]
  (let [is-file? (and (.exists f) (.isFile f))
        filename (.getName f)
        svg?     (string/ends-with? filename ".svg")]
    (and is-file? svg?)))

(defn svg-in-dirs [dirs]
  (->> dirs
       (reduce
         (fn [acc dpath]
           (let [f (io/file dpath)]
             (if (.isDirectory f) (conj acc (file-seq f)) acc)))
         [])
       (apply concat)
       (reduce
         (fn [acc f]
           (let [filename (.getName f)
                 svg-name (subs filename 0 (- (count filename) 4))]
             (if (svg-file? f) (assoc acc svg-name (slurp f)) acc)))
         {})))

(defn name-spritesheet-icon
  "creates a name identifying a resource from a spritesheet.
  name has be a valid clojure symbol name; this may be in conflict with some string ids.
  e.g. \"500px\" is a valid string but not a valid symbol name (can't begin with a numerical char).

  in cases of:
  - first char is numerical:
    replace first char with word representing the digit
    e.g. 500px -> Five00px
  "
  [ss-name icon-name]
  (let [icon-name' (try
                     (-> icon-name first str
                         Integer/parseInt
                         (case
                           0 "Zero"
                           1 "One"
                           2 "Two"
                           3 "Three"
                           4 "Four"
                           5 "Five"
                           6 "Six"
                           7 "Seven"
                           8 "Eight"
                           9 "Nine")
                         (str (subs icon-name 1)))
                     (catch Exception e icon-name))]
    (str icon-name' ss-file-sep ss-name)))

(defn svg-in-spritesheets [spritesheets]
  (->> spritesheets
       (reduce
         (fn [acc spath]
           (let [f (io/file spath)]
             (if (svg-file? f)
               (let [ffilename    (.getName f)
                     filename     (subs ffilename 0 (- (count ffilename) 4))
                     f-as-hickory (-> f slurp h/parse h/as-hickory)]
                 (assoc acc filename f-as-hickory))
               acc)))
         {})
       (map
         (fn [[filename ss-hickory]]
           (->>
             ss-hickory
             (s/select (s/tag :symbol))
             (map (fn [s]
                    {(name-spritesheet-icon filename (-> s :attrs :id))
                     (->> (assoc s :tag :svg)
                          hr/hickory-to-html)})))))
       (apply concat)
       (into {})))

(defn svg-defn-form [svg-name raw-svg]
  (let [fn-name (symbol svg-name)
        svg-map {:dangerouslySetInnerHTML {:__html raw-svg}}]
    `(defn ~fn-name
       ([] (~fn-name {}))
       ([~'m]
         [~(keyword (string/join "." ["i" "tq-icon" svg-name]))
          (merge ~'m ~svg-map)]))))


(defmacro deficons
  "takes in a map which specifies the location of spritesheets or directories
  containing svg files. the raw svg for each resource is returned wrapped in a
  function named accordingly to the resource. resources should have a viewbox
  property for scaling, and should avoid using internal fills etc. to allow
  CSS styling.

  spritesheet:
  expected to be an .SVG file containing a collection of 'symbol' elements.
  each symbol should have a viewbox property to scale properly.
  symbol element is parsed and converted to an 'svg' element.
  functions for spritesheet resources are named as such:
    {{iconName--spritesheetName}}
    e.g.
    (user--solid)
    (sync-alt--solid)
    (stripe--brands)

  dir:
  expected to be a directory containing .SVG files.
  each file must be a top level 'svg' element with a viewbox property.
  functions for dir resources are named as such:
    {{filename}} (no extension)
    e.g.
    (user)
  "
  [{:keys [dirs spritesheets]}]
  `(do
     ~@(map
         (fn [[svg-name svg-content]]
           (svg-defn-form svg-name svg-content))
         (svg-in-dirs dirs))
     ~@(map
         (fn [[svg-name svg-hiccup]]
           (svg-defn-form svg-name svg-hiccup))
         (svg-in-spritesheets spritesheets))))
