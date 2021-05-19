(ns org.rssys.gantt.engine
  (:require
    [babashka.fs :as fs]
    [clojure.edn :as edn]
    [clojure.java.io :as io]
    [clojure.string :as string]
    [malli.core :as m]
    [org.rssys.gantt.spec :as spec])
  (:import
    (net.sourceforge.plantuml
      FileFormat
      FileFormatOption
      SourceStringReader)))


(defn project-starts-at
  [content]
  (when (:project-starts content)
    (format "\nProject starts %s\n" (:project-starts content))))


(defn project-title
  [content]
  (when (:project-title content)
    (format "\nTitle %s\n" (:project-title content))))


(defn project-header
  [content]
  (when (:project-header content)
    (format "\nHeader %s\n" (:project-header content))))


(defn project-footer
  [content]
  (when (:project-footer content)
    (format "\nFooter %s\n" (:project-footer content))))


(defn hide-footbox
  [content]
  (when (:hide-footbox? content)
    (format "\nHide footbox\n")))


(defn project-scale
  [content]
  (when (:project-scale content)
    (format "\nprojectscale %s\n" (name (:project-scale content)))))


(defn weekend-days
  [content]
  (when (:closed-days content)
    (for [day (:closed-days content)]
      (format "\n%s are closed" (string/capitalize (name day))))))


(defn holidays
  [content]
  (when (:holidays content)
    (for [day (:holidays content)]
      (format "\n%s is closed" day))))


(defmulti process-task "process task"
  (fn [{:keys [separator name alias days-lasts starts-at
               starts-after ends-at starts-before-end starts-after-end
               ends-at-start ends-at-end] :as t}]
    (cond
      separator :separator
      (and days-lasts starts-at) [:days-lasts :starts-at]
      (and days-lasts ends-at-start) [:days-lasts :ends-at-start]
      (and days-lasts ends-at-end) [:days-lasts :ends-at-end]
      (and days-lasts starts-after) [:days-lasts :starts-after]
      (and days-lasts starts-before-end) [:days-lasts :starts-before-end]
      (and days-lasts starts-after-end) [:days-lasts :starts-after-end]
      (and starts-at ends-at) [:starts-at :ends-at]
      days-lasts [:days-lasts]
      :else (throw (ex-info "Unknown task format" t)))))


(defmethod process-task :separator
  [task]
  (format "\n\n-- %s --\n" (:separator task)))


(defmethod process-task [:days-lasts]
  [task]
  (format "\n[%s] as [%s] lasts %s days"
    (:name task)
    (name (:alias task))
    (:days-lasts task)))


(defmethod process-task [:days-lasts :starts-at]
  [task]
  (format "\n[%s] as [%s] lasts %s days and starts %s"
    (:name task)
    (name (:alias task))
    (:days-lasts task)
    (:starts-at task)))


(defmethod process-task [:days-lasts :ends-at-start]
  [task]
  (format "\n[%s] as [%s] lasts %s days and ends at [%s]'s start"
    (:name task)
    (name (:alias task))
    (:days-lasts task)
    (name (:ends-at-start task))))


(defmethod process-task [:days-lasts :ends-at-end]
  [task]
  (format "\n[%s] as [%s] lasts %s days and ends at [%s]'s end"
    (:name task)
    (name (:alias task))
    (:days-lasts task)
    (name (:ends-at-end task))))


(defmethod process-task [:days-lasts :starts-after]
  [task]
  (format "\n[%s] as [%s] lasts %s days and starts at [%s]'s end"
    (:name task)
    (name (:alias task))
    (:days-lasts task)
    (name (:starts-after task))))


(defmethod process-task [:days-lasts :starts-before-end]
  [task]
  (format "\n[%s] as [%s] lasts %s days and starts %s days before [%s]'s end"
    (:name task)
    (name (:alias task))
    (:days-lasts task)
    (second (:starts-before-end task))
    (name (first (:starts-before-end task)))))


(defmethod process-task [:days-lasts :starts-after-end]
  [task]
  (format "\n[%s] as [%s] lasts %s days and starts %s days after [%s]'s end"
    (:name task)
    (name (:alias task))
    (:days-lasts task)
    (second (:starts-after-end task))
    (name (first (:starts-after-end task)))))


(defmethod process-task [:starts-at :ends-at]
  [task]
  (format "\n[%s] as [%s] starts %s and ends %s "
    (:name task)
    (name (:alias task))
    (:starts-at task)
    (:ends-at task)))


(defn tasks
  [content]
  (when (:tasks content)
    (for [task (:tasks content)]
      (process-task task))))


(defn task-complete
  [content]
  (when (:tasks content)
    (for [task (:tasks content)]
      (when-not (:separator task)
        (format "[%s] is %s%% completed\n"
          (name (:alias task))
          (:percent-complete task))))))


(defn task-colored
  [content]
  (when (:tasks content)
    (for [task (:tasks content)]
      (when-not (:separator task)
        (when-let [percent (:percent-complete task)]
          (cond
            (:color task) (format "[%s] is colored in %s\n" (name (:alias task)) (:color task))

            (and (= percent 100) (-> content :task-colors :color/completed))
            (format "[%s] is colored in %s\n" (name (:alias task)) (-> content :task-colors :color/completed))

            (and (> percent 0) (-> content :task-colors :color/in-progress))
            (format "[%s] is colored in %s\n" (name (:alias task)) (-> content :task-colors :color/in-progress))))))))


(defn milestones
  [content]
  (when (:milestones content)
    (mapcat (fn [milestone]
              (if (:happens-at milestone)
                [(format "\n[%s] happens %s" (:name milestone) (:happens-at milestone))]
                (for [task-alias (:happens-after milestone)]
                  (format "\n[%s] happens after [%s]'s end" (:name milestone) (name task-alias)))))
      (:milestones content))))


(defn make-gantt-content
  "Create Gantt content as text from EDN structure.
  Returns EDN structure."
  [gantt-struc]
  (-> []
    (conj (project-starts-at gantt-struc))
    (conj (project-title gantt-struc))
    (conj (project-header gantt-struc))
    (conj (project-footer gantt-struc))
    (conj (hide-footbox gantt-struc))
    (conj (project-scale gantt-struc))
    (into (for [weekend (weekend-days gantt-struc)] weekend))
    (into (for [holiday (holidays gantt-struc)] holiday))
    (conj \newline)
    (into (for [task (tasks gantt-struc)] task))
    (conj "\n\n")
    (into (remove nil? (for [task (task-complete gantt-struc)] task)))
    (conj \newline)
    (into (remove nil? (for [task (task-colored gantt-struc)] task)))
    (into (for [milestone (milestones gantt-struc)] milestone))
    (conj \newline)))


(defn gantt-content->asciidoc-content
  "Wraps Gantt content with Asciidoc header for PlantUML frame.
  Returns Asciidoc text content as EDN structure."
  [gantt-content & {:keys [img-format] :or {img-format :png}}]
  (let [out-content (-> []
                      (conj (if (= img-format :png) "[plantuml, format=png]" "[plantuml, format=svg]"))
                      (conj "\n----\n")
                      (conj "@startgantt\n")
                      (into gantt-content)
                      (conj "\n@endgantt")
                      (conj "\n----\n"))]
    out-content))


(defn gantt-content->puml-content
  "Wraps Gantt content with PUML file header.
  Returns PUML text content as EDN structure."
  [gantt-content]
  (let [out-content (-> []
                      (conj "@startgantt\n")
                      (into gantt-content)
                      (conj "\n@endgantt"))]
    out-content))


(defn write-content->file
  "Writes PUML or PlantUML text content to a text file."
  [out-content filename]
  (try (io/delete-file filename) (catch Exception _))
  (doseq [i out-content]
    (spit filename i :append true)))


(defn read-gantt-struct
  "Read EDN-file and validate fo spec."
  [edn-file]
  (let [gantt-struc (edn/read-string (slurp edn-file))]
    (if (m/validate spec/gantt-structure gantt-struc)
      gantt-struc
      (let [error (first (:errors (m/explain spec/gantt-structure gantt-struc)))
            msg   (format "File %s has invalid structure: %s" edn-file (pr-str error))]
        (println msg)
        (throw (ex-info (format "File %s has invalid structure" edn-file)
                 {:data error}))))))


(defn generate-gantt-picture
  "Generate Gantt diagram from PUML file.
  Supported output formats: `png` (default) and `svg`.
  Returns map with output filename."
  [puml-filename & {:keys [img-format output-folder]
                    :or   {img-format    :png
                           output-folder (str "." fs/file-separator)}}]
  (let [puml-string   ^String (slurp puml-filename)
        puml-reader   (SourceStringReader. puml-string)
        filename-only (fs/file-name puml-filename)
        out-extension (case img-format :png ".png" :svg ".svg" ".png")
        out-filename  (str (fs/absolutize (str output-folder fs/file-separator filename-only out-extension)))
        baos          (io/output-stream out-filename)
        _             (.outputImage puml-reader baos (FileFormatOption. (case img-format :png FileFormat/PNG :svg FileFormat/SVG)))]
    (.close baos)
    {:output-filename out-filename}))


(comment
  (def gantt-struc (read-gantt-struct "test/data/ex01-fixed-dates-calendar.edn"))
  (def content (make-gantt-content gantt-struc))
  (write-content->file (gantt-content->asciidoc-content content) "ex01.adoc")

  (def gantt-struc (read-gantt-struct "test/data/ex02-durations-only.edn"))
  (def content (make-gantt-content gantt-struc))
  (write-content->file (gantt-content->asciidoc-content content) "ex02.adoc")

  (def gantt-struc (read-gantt-struct "test/data/ex03-reverse-order-planning.edn"))
  (def content (make-gantt-content gantt-struc))
  (write-content->file (gantt-content->asciidoc-content content) "ex03.adoc")

  (def gantt-struc (read-gantt-struct "test/data/ex04-header-title-footer.edn"))
  (def content (make-gantt-content gantt-struc))
  (write-content->file (gantt-content->asciidoc-content content) "ex04.adoc")
  (write-content->file (gantt-content->puml-content content) "ex04-header-title-footer.puml")

  (generate-gantt-picture "test/data/results/ex04-header-title-footer.puml" :img-format :svg)
  (generate-gantt-picture "test/data/results/ex04-header-title-footer.puml" :img-format :png :output-folder "./test")

  )



