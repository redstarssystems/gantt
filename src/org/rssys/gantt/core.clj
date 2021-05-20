(ns org.rssys.gantt.core
  (:gen-class)
  (:require
    [babashka.fs :as fs]
    [clojure.string :as string]
    [clojure.tools.cli :as cli]
    [org.rssys.gantt.files :as files])
  (:import
    (java.net
      InetAddress)))


(defn set-global-exception-hook
  "Catch any uncaught exceptions and print them."
  []
  (Thread/setDefaultUncaughtExceptionHandler
    (reify Thread$UncaughtExceptionHandler
      (uncaughtException
        [_ thread ex]
        (println "uncaught exception" :thread (.getName thread) :desc ex)))))


(def cli-options
  [["-p" "--port PORT" "HTTP server port number"
    :default 8080
    :parse-fn #(Integer/parseInt %)
    :validate [#(< 0 % 0x10000) "Must be a number between 0 and 65536"]]

   ["-s" "--server HOST" "start HTTP server using this hostname"
    :default (InetAddress/getByName "localhost")
    :default-desc "localhost"
    :parse-fn #(InetAddress/getByName %)]

   ["-i" "--input-folder FOLDER" "Input folder with EDN-files"
    :validate [#(fs/directory? %) "Input folder should exist"]]

   ["-f" "--file-format FORMAT" "PNG or SVG format for output files"
    :default "png"
    :validate [#(some #{"png" "svg"} [%]) "Should be `png` or `svg`."]]

   ["-o" "--output-folder FOLDER" "Output folder to write Gantt diagrams"
    :validate [#(fs/directory? %) "Output folder should exist"]]

   ["-h" "--help"]])


(defn usage
  [options-summary]
  (->> ["Gantt diagram generator."
        ""
        "Usage: program-name [options] action"
        ""
        "Options:"
        options-summary
        ""
        "Actions:"
        "  server   Start HTTP server to show generated Gantt diagrams via HTTP"
        "  watch    Run watchdog for input folder changes to produce Gantt diagrams to output folder"
        "  generate Generate Gantt diagrams from input folder to output folder and exit"
        ""
        "Please refer to the manual page https://github.com/redstarssystems/gantt for more information."]
    (string/join \newline)))


(defn error-msg
  [errors]
  (str "The following errors occurred while parsing your command:\n\n"
    (string/join \newline errors)))


(defn validate-args
  "Validate command line arguments. Either return a map indicating the program
  should exit (with a error message, and optional ok status), or a map
  indicating the action the program should take and the options provided."
  [args]
  (let [{:keys [options arguments errors summary]} (cli/parse-opts args cli-options)]
    (cond
      (:help options)                                       ; help => exit OK with usage summary
      {:exit-message (usage summary) :ok? true}
      errors                                                ; errors => exit with description of errors
      {:exit-message (error-msg errors)}
      (not (and (:input-folder options) (:output-folder options)))
      {:exit-message (error-msg ["input and output folders are mandatory options"])}
      ;; custom validation on arguments
      (and (= 1 (count arguments))
        (#{"server" "watch" "generate"} (first arguments)))
      {:action (first arguments) :options options}
      :else                                                 ; failed custom validation => exit with usage summary
      {:exit-message (usage summary)})))


(defn exit
  [status msg]
  (println msg)
  (System/exit status))


(defn -main
  "entry point to app."
  [& args]
  (set-global-exception-hook)
  (let [{:keys [action options exit-message ok?]} (validate-args args)]
    (if exit-message
      (exit (if ok? 0 1) exit-message)
      (case action
        "server" (println "running http server...")
        "watch" (println "running watchdog for input folder...")
        "generate" (files/generate options))))
  (System/exit 0))
