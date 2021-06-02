(ns org.rssys.gantt.spec
  (:require
    [clojure.spec.gen.alpha :as gen]
    [clojure.string :as string])
  (:import
    (java.time
      LocalDate)))


;;
;; basic types
;;

;; not empty string
(def ne-string [:and [:string] [:fn {:error/message "Should be a not blank string"} (complement #(string/blank? %))]])


(def string-date
  [:and
   {:description "Date in string representation."
    :gen/gen     (gen/fmap
                   (fn [[^long y ^long m ^long d]]
                     (str (LocalDate/of y m d)))
                   (gen/tuple (gen/choose 2000 2020) (gen/choose 1 12) (gen/choose 1 28)))}
   :string
   [:fn {:error/message "Should be date as a string in format yyyy-MM-dd"}
    (fn [^String s]
      (instance? LocalDate (LocalDate/parse s)))]])


(def scale
  [:and
   {:description "SVG/PNG image size."
    :gen/gen     (gen/return "1200*900")}
   ne-string])


(def title
  [:and
   {:description "Project title."
    :gen/gen     (gen/fmap (fn [v] (str "Title-" v)) (gen/choose 1 100))}
   ne-string])


(def footer
  [:and
   {:description "Project footer."
    :gen/gen     (gen/fmap (fn [v] (str "Footer-" v)) (gen/choose 1 100))}
   ne-string])


(def header
  [:and
   {:description "Project header."
    :gen/gen     (gen/fmap (fn [v] (str "Header-" v)) (gen/choose 1 100))}
   ne-string])


(def hide-footbox
  [:and
   {:description "Enable/disable footbox."}
   :boolean])


(def project-scale [:enum {:description "Project scale"} :daily :monthly :weekly :quarterly :yearly])


(def color
  [:and
   {:description "PlantUML colors https://plantuml.com/en/color"
    :gen/gen     (gen/elements ["GreenYellow/Red" "#FF0000/FFFF00" "fuchsia"])}
   ne-string])


(def tasks-colors
  [:map
   {:description "Tasks colors"}
   [:color/in-progress {:optional true} color]
   [:color/completed {:optional true} color]])


(def days-colors
  [:vector
   {:description "List of colored days" :gen/min 1, :gen/max 5}
   [:and
    [:map
     {:description "Day colors"}
     [:color color]
     [:days-list {:optional true} [:vector {:description "List of days" :gen/min 1, :gen/max 3} string-date]]
     [:days-range {:optional true} [:map
                                    {:description "Range of days"}
                                    [:days-name {:optional true} ne-string]
                                    [:from string-date]
                                    [:to string-date]]]]
    [:fn {:error/message ":days-list or :days-range must present, but only one of them."}
     (fn [{:keys [days-list days-range]}]
       (or
         (and days-list (nil? days-range))
         (and days-range (nil? days-list))))]]])


(def week-days
  [:enum
   {:description "Week days"}
   :sunday :monday :tuesday :wednesday :thursday :friday :saturday])


(def closed-days
  [:set {:description "Closed days for tasks"}
   week-days])


(def holidays [:vector {:description "Holidays" :gen/min 1, :gen/max 3} string-date])


;;
;; Task types
;;


(def task-name
  [:and {:description "Task name"
         :gen/gen     (gen/fmap (fn [v] (str "task-" v)) (gen/choose 1 100))}
   ne-string])


(def task-alias
  [:and {:description "Task alias"
         :gen/gen     (gen/fmap (fn [v] (keyword (str "task-" v))) (gen/choose 1 10))}
   :keyword])


(def task-days-lasts
  [:and {:description "How many days task lasts"
         :gen/gen     (gen/choose 1 100)}
   [:>= 0]])


(def task-percent-complete
  [:and {:description "Task percent complete"
         :gen/gen     (gen/choose 1 100)}
   :int [:>= 0] [:<= 100]])


(def task-starts-at
  [:and {:description "Task start date"}
   string-date])


(def task-starts-after
  [:and {:description "Task alias which triggers start of the current task"}
   task-alias])


(def task-starts-before-end
  [:and {:description "Task alias and number of days before task (alias) ends which triggers start of the current task"
         :gen/gen     (gen/tuple (gen/fmap (fn [v] (keyword (str "task-" v))) (gen/choose 1 10)) (gen/choose 1 7))}
   [:cat task-alias [:> 0]]])


(def task-starts-after-end
  [:and {:description "Task alias and number of days after task (alias) ends which triggers start of the current task"
         :gen/gen     (gen/tuple (gen/fmap (fn [v] (keyword (str "task-" v))) (gen/choose 1 10)) (gen/choose 1 7))}
   [:cat task-alias [:> 0]]])


(def task-ends-at
  [:and {:description "Task end date"}
   string-date])


(def task-ends-at-start
  [:and {:description "Task ends when another task starts"}
   task-alias])


(def task-ends-at-end
  [:and {:description "Task ends when another task ends"}
   task-alias])


(def task
  [:and [:map
         [:task task-name]
         [:alias task-alias]
         [:percent-complete task-percent-complete]
         [:color {:optional true} color]
         [:pause-on-days {:optional true} [:vector {:gen/min 1, :gen/max 3} string-date]]
         [:starts-at {:optional true} task-starts-at]
         [:starts-before-end {:optional true} task-starts-before-end]
         [:starts-after-end {:optional true} task-starts-after-end]
         [:ends-at-start {:optional true} task-ends-at-start]
         [:ends-at-end {:optional true} task-ends-at-end]
         [:days-lasts {:optional true} task-days-lasts]
         [:ends-at {:optional true} task-ends-at]
         [:starts-after {:optional true} task-starts-after]]
   [:fn {:error/message ":days-lasts or :ends-at must present, but only one of them."}
    (fn [{:keys [days-lasts ends-at]}]
      (or
        (and days-lasts (nil? ends-at))
        (and ends-at (nil? days-lasts))))]

   [:fn {:error/message "If :starts-at present then :starts-before-end should not present."}
    (fn [{:keys [starts-at starts-before-end]}]
      (not (and starts-at starts-before-end)))]

   [:fn {:error/message "If :starts-at present then :starts-after-end should not present."}
    (fn [{:keys [starts-at starts-after-end]}]
      (not (and starts-at starts-after-end)))]

   [:fn {:error/message "If :starts-after present then :starts-before-end should not present."}
    (fn [{:keys [starts-after starts-before-end]}]
      (not (and starts-after starts-before-end)))]

   [:fn {:error/message "If :starts-after present then :starts-after-end should not present."}
    (fn [{:keys [starts-after starts-after-end]}]
      (not (and starts-after starts-after-end)))]

   [:fn {:error/message "If :starts-before-end present then :starts-after-end should not present."}
    (fn [{:keys [starts-before-end starts-after-end]}]
      (not (and starts-before-end starts-after-end)))]

   [:fn {:error/message "If :ends-at-start present then no one if these: #{:starts-at :starts-after :starts-before-end :starts-after-end :ends-at :ends-at-end}  should not present."}
    (fn [task]
      (if (:ends-at-start task)
        (not (some #{:starts-at :starts-after :starts-before-end :starts-after-end :ends-at :ends-at-end}
               (keys task)))
        true))]

   [:fn {:error/message "If :ends-at-end present then no one if these: #{:starts-at :starts-after :starts-before-end :starts-after-end :ends-at :ends-at-start}  should not present."}
    (fn [task]
      (if (:ends-at-end task)
        (not (some #{:starts-at :starts-after :starts-before-end :starts-after-end :ends-at :ends-at-start}
               (keys task)))
        true))]

   [:fn {:error/message "If :ends-at present then :starts-after should not present."}
    (fn [{:keys [starts-after ends-at]}]
      (not (and ends-at starts-after)))]])



(def separator
  [:map {:optional true}
   [:separator
    {:gen/gen (gen/fmap (fn [x] (str "Stage-" x)) (gen/choose 1 10))}
    ne-string]])


;;
;; Milestone types
;;


(def milestone-name
  [:and {:description "Milestone name"
         :gen/gen     (gen/fmap (fn [v] (str "milestone-" v)) (gen/choose 1 100))}
   ne-string])


(def milestone-happens-after
  [:and {:description "Task alias list which should be done to achieve milestone"}
   [:vector {:gen/min 1, :gen/max 3} task-alias]])


(def milestone-happens-at
  [:and {:description "Milestone date"}
   string-date])


(def milestone
  [:and [:map
         [:milestone milestone-name]
         [:happens-after {:optional true} milestone-happens-after]
         [:happens-at {:optional true} milestone-happens-at]]
   [:fn {:error/message ":days-lasts or :ends-at must present, but only one of them "}
    (fn [{happens-after :happens-after
          happens-at    :happens-at}]
      (or
        (and happens-after (nil? happens-at))
        (and happens-at (nil? happens-after))))]])


;;
;; PlantUML Gantt structure
;;

(def project-content [:vector {:gen/min 1, :gen/max 5} [:or task separator milestone]])


(def gantt-structure
  [:and
   [:map
    [:scale {:optional true} scale]
    [:project-starts {:optional true} string-date]
    [:project-title {:optional true} title]
    [:project-header {:optional true} header]
    [:project-footer {:optional true} footer]
    [:project-scale {:optional true} project-scale]
    [:hide-footbox? {:optional true} hide-footbox]
    [:tasks-colors {:optional true} tasks-colors]
    [:days-colors {:optional true} days-colors]
    [:closed-days {:optional true} closed-days]
    [:holidays {:optional true} holidays]
    [:project-content project-content]
    [:milestones {:optional true} [:vector {:gen/min 1, :gen/max 3} milestone]]]])


(comment
  (require '[malli.generator :as mg])
  (mg/generate gantt-structure)
  (mg/generate project-content)
  (me/humanize (m/explain task {:name "task3" :alias :t3 :ends-at "2021-05-18" :percent-complete 0 :starts-after :t1}))
  (def gantt-struc {:project-starts "2004-12-15",
                    :project-scale  :yearly,
                    :tasks          [{:name             "task-48",
                                      :alias            :task-6,
                                      :percent-complete 7,
                                      :color            "#FF0000/FFFF00",
                                      :starts-at        "2007-05-28",
                                      :ends-at          "2014-08-11"}
                                     {:separator "Stage-1"}
                                     {:name "task-56", :alias :t23, :percent-complete 72, :starts-at "2018-03-26", :days-lasts 40}],
                    :milestones     [{:name "milestone-71", :happens-at "2007-04-01"}
                                     {:name "milestone-13", :happens-at "2005-07-12"}
                                     {:name "milestone-90", :happens-after [:task-9 :task-6]}],
                    :closed-days    #{:tuesday :wednesday :sunday :friday :monday},
                    :holidays       ["2011-06-20"]})
  (m/validate gantt-structure gantt-struc)
  (me/humanize (m/explain gantt-structure gantt-struc))

  )

