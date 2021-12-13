(ns org.rssys.gantt.files
  (:require
    [babashka.fs :as fs]
    [hawk.core :as hawk]
    [org.rssys.gantt.engine :as engine])
  (:import
    (java.time
      ZonedDateTime)
    (java.time.format
      DateTimeFormatter)))


(defn- generate-gantt-from-edn
  "Process EDN file to PUML & PNG/SVG files and save them to output folder"
  [^String edn-filename ^String output-folder ^String file-format delete-puml]
  (println (format "%s processing file %s"
             (.format (ZonedDateTime/now) (DateTimeFormatter/ofPattern "yyyy-MM-dd HH:mm:ss"))
             edn-filename))
  (let [edn-content            (engine/read-gantt-struct edn-filename)
        output-folder-modified (if (= ":input-folder" output-folder)
                                 (fs/parent edn-filename)
                                 output-folder)
        gantt-content          (engine/make-gantt-content edn-content)
        puml-content           (engine/gantt-content->puml-content gantt-content)
        puml-filename          (str output-folder-modified fs/file-separator (fs/file-name edn-filename) ".puml")
        _                      (do (fs/delete-if-exists puml-filename) (fs/create-file puml-filename))
        _                      (println "puml file:" puml-filename)
        _                      (engine/write-content->file puml-content puml-filename)
        result                 (engine/generate-gantt-picture puml-filename :img-format (keyword file-format) :output-folder output-folder-modified
                                 :output-filename edn-filename)]
    (when delete-puml (println "deleting puml file:" puml-filename) (fs/delete-if-exists puml-filename))
    (println "picture file:" (:output-filename result) \newline)
    result))


(defn generate
  "Generate Gantt diagrams for data from input folder and put generated files to output folder"
  [{:keys [input-folder output-folder file-format delete-puml]}]
  (let [input-files-list (fs/glob input-folder "**{.edn}")]
    (doseq [edn-file input-files-list]
      (try
        (generate-gantt-from-edn (str edn-file) output-folder file-format delete-puml)
        (catch Exception e
          (println (.getMessage e) \newline))))))


(defn watch
  "Catch file changes in folders and generate Gantt diagrams for them"
  [{:keys [input-folder output-folder file-format delete-puml]}]
  (println "Starting watchdog for folder:" input-folder)
  (hawk/watch! [{:paths   [input-folder]
                 :filter  (fn [_ e] (and (fs/regular-file? (:file e)) (= "edn" (fs/extension (:file e)))))
                 :handler (fn [_ e]
                            (when (some #{:create :modify} [(:kind e)])
                              (try
                                (generate-gantt-from-edn (str (:file e)) output-folder file-format delete-puml)
                                (catch Exception e
                                  (println (.getMessage e) \newline)))))}])
  (println "Press <Enter> to stop watching.")
  (read-line))

