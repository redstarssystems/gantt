(ns org.rssys.gantt.files
  (:require
    [babashka.fs :as fs]
    [org.rssys.gantt.engine :as engine]
    [hawk.core :as hawk])
  (:import
    (java.io
      File)))


(defn- generate-gantt-from-edn
  "Process EDN file to PNG/SVG file and save it to output folder"
  [^String edn-filename ^String output-folder ^String file-format]
  (let [edn-content   (engine/read-gantt-struct edn-filename)
        gantt-content (engine/make-gantt-content edn-content)
        output-folder (if (= ":input-folder" output-folder)
                        (fs/parent edn-filename)
                        output-folder)
        puml-content  (engine/gantt-content->puml-content gantt-content)
        temp-filename (str (fs/delete-on-exit (File/createTempFile "gantt-" ".puml")))
        _             (engine/write-content->file puml-content temp-filename)
        result        (engine/generate-gantt-picture temp-filename :img-format (keyword file-format) :output-folder output-folder
                        :output-filename edn-filename)]
    (fs/delete-if-exists temp-filename)
    result))


(defn generate
  "Generate Gantt diagrams for data from input folder and put generated picture to output folder"
  [{:keys [input-folder output-folder file-format]}]
  (let [input-files-list (fs/glob input-folder "**{.edn}")]
    (doseq [edn-file input-files-list]
      (try
        (println "processing file:" (str edn-file))
        (let [result (generate-gantt-from-edn (str edn-file) output-folder file-format)]
          (println "generated file:" (:output-filename result) \newline))
        (catch Exception e
          (println (.getMessage e) \newline))))))


(defn watch
  "Catch file changes in folders and generate Gantt diagrams for them"
  [{:keys [input-folder output-folder file-format]}]
  (println "Starting watchdog for folder:" input-folder)
  (hawk/watch! [{:paths   [input-folder]
                 :filter  (fn [ctx e] (and (fs/regular-file? (:file e)) (= "edn" (fs/extension (:file e)))))
                 :handler (fn [ctx e]
                            (when (some #{:create :modify} [(:kind e)])
                              (try
                                (let [result (generate-gantt-from-edn (str (:file e)) output-folder file-format)]
                                  (println "generated file:" (:output-filename result) \newline))
                                (catch Exception e
                                  (println (.getMessage e) \newline)))))}])
  (println "Press <Enter> to exit.")
  (read-line))

