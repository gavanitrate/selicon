(ns selicon.icon
  (:require [clojure.string :as string])
  (:refer-clojure
    :exclude
    [map key sort filter divide clone print comment list])
  (:require-macros [selicon.macro :refer [deficons]]))

(deficons {:dirs         ["resources/icons/optimised"]
           :spritesheets ["resources/icons/spritesheets/regular.svg"
                          "resources/icons/spritesheets/solid.svg"
                          "resources/icons/spritesheets/brands.svg"]})

(defn style
  "easy way to style an icon.
  allows you to specify any selection of style elements,
  without having to manually build class attribute."
  [& {:keys [size scale
             color bs-color
             spin pulse
             extra-classes
             attrs]}]
  (let [size-class  (cond
                      (string? size) size
                      (keyword? size) (name size)
                      (some? scale) (str "scale-" scale)
                      :else nil)
        color-class (cond
                      (some? color) nil
                      (string? bs-color) (str "text-" bs-color)
                      (keyword? bs-color) (str "text-" (name bs-color))
                      :else nil)
        animation-class
                    (cond
                      (true? pulse) "pulse"
                      (true? spin) "spin"
                      :else nil)
        classes     (->> [extra-classes color-class size-class animation-class]
                         (clojure.core/filter #(not (nil? %)))
                         (string/join " "))]
    (merge
      attrs
      {:class classes}
      (when color {:style {:color color}}))))
