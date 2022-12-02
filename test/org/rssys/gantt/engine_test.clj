(ns org.rssys.gantt.engine-test
  (:require
    [babashka.fs :as fs]
    [clojure.test :as test :refer [deftest testing is]]
    [matcho.core :refer [match]]
    [org.rssys.gantt.engine :as sut])
  (:import
    (clojure.lang
      ExceptionInfo)
    (java.io
      File)))


(deftest ^:unit read-gantt-struct-test

  (testing "read correct EDN-data completes successfully"
    (let [correct-edn1 (sut/read-gantt-struct "test/data/01-fixed-dates-calendar.edn")
          correct-edn2 (sut/read-gantt-struct "test/data/02-durations-only.edn")
          correct-edn3 (sut/read-gantt-struct "test/data/03-reverse-order-planning.edn")]
      (match correct-edn1 {:project-starts "2021-05-01"
                           :project-scale  :daily
                           :tasks-colors   {:color/in-progress "GreenYellow/Red"
                                            :color/completed   "GreenYellow/Green"}
                           :closed-days    #{:saturday :sunday}
                           :holidays       ["2021-05-03" "2021-05-10"]})
      (match (-> correct-edn2 :project-content first) {:task             "task1"
                                                       :alias            :t1
                                                       :days-lasts       10
                                                       :percent-complete 100})
      (match (-> correct-edn3 :project-content first) {:task             "whole project"
                                                       :alias            :t5
                                                       :days-lasts       30
                                                       :percent-complete 0
                                                       :color            "Gray"})))

  (testing "read bad EDN-data throws Exception"
    (is (thrown-with-msg?
          ExceptionInfo
          #"has invalid structure"
          (sut/read-gantt-struct "test/data/bad-data/01-bad-data.edn")))))


(deftest ^:unit write-content->file-test

  (testing "Asciidoc && PUML content generates successfully"
    (let [edn-files-list               ["test/data/01-fixed-dates-calendar.edn"
                                        "test/data/02-durations-only.edn"
                                        "test/data/03-reverse-order-planning.edn"
                                        "test/data/04-header-title-footer.edn"
                                        "test/data/05-colored-named-days.edn"
                                        "test/data/06-pause-days-for-task.edn"
                                        "test/data/07-task-resources.edn"
                                        "test/data/08-project-scale-zoom.edn"
                                        "test/data/09-multiple-starts-after.edn"]

          expected-asciidoc-files-list ["test/data/results/01-fixed-dates-calendar.adoc"
                                        "test/data/results/02-durations-only.adoc"
                                        "test/data/results/03-reverse-order-planning.adoc"
                                        "test/data/results/04-header-title-footer.adoc"
                                        "test/data/results/05-colored-named-days.adoc"
                                        "test/data/results/06-pause-days-for-task.adoc"
                                        "test/data/results/07-task-resources.adoc"
                                        "test/data/results/08-project-scale-zoom.adoc"
                                        "test/data/results/09-multiple-starts-after.adoc"]

          expected-puml-files-list     ["test/data/results/01-fixed-dates-calendar.puml"
                                        "test/data/results/02-durations-only.puml"
                                        "test/data/results/03-reverse-order-planning.puml"
                                        "test/data/results/04-header-title-footer.puml"
                                        "test/data/results/05-colored-named-days.puml"
                                        "test/data/results/06-pause-days-for-task.puml"
                                        "test/data/results/07-task-resources.puml"
                                        "test/data/results/08-project-scale-zoom.puml"
                                        "test/data/results/09-multiple-starts-after.puml"]]

      (match (count edn-files-list) (count expected-asciidoc-files-list))
      (match (count edn-files-list) (count expected-puml-files-list))

      (dotimes [i (count expected-asciidoc-files-list)]
        (let [gantt-struct     (sut/read-gantt-struct (nth edn-files-list i))
              adoc-temp-file        (str (fs/delete-on-exit (File/createTempFile "gantt-" ".adoc")))
              puml-temp-file        (str (fs/delete-on-exit (File/createTempFile "gantt-" ".puml")))
              gantt-content    (sut/make-gantt-content gantt-struct)
              expected-adoc-content (slurp (nth expected-asciidoc-files-list i))
              expected-puml-content (slurp (nth expected-puml-files-list i))]

          (sut/write-content->file (sut/gantt-content->asciidoc-content gantt-content :img-format :png) adoc-temp-file)
          (sut/write-content->file (sut/gantt-content->puml-content gantt-content) puml-temp-file)

          (match (slurp adoc-temp-file) expected-adoc-content)
          (match (slurp puml-temp-file) expected-puml-content)

          (fs/delete-if-exists adoc-temp-file)
          (fs/delete-if-exists puml-temp-file))))))


(deftest ^:unit generate-gantt-picture-test

  (testing "PNG generated from PUML file successfully"
    (let [temp-dir (fs/create-temp-dir {:prefix "puml-"})]
      (try
        (let [result (sut/generate-gantt-picture "test/data/results/04-header-title-footer.puml" :img-format :png :output-folder (str temp-dir))]
          (match (fs/extension (:output-filename result)) "png")
          (match (fs/size (:output-filename result)) pos-int?))
        (finally
          (fs/delete-tree temp-dir)))))

  (testing "SVG generated from PUML file successfully"
    (let [temp-dir (fs/create-temp-dir {:prefix "puml-"})]
      (try
        (let [result (sut/generate-gantt-picture "test/data/results/04-header-title-footer.puml" :img-format :svg :output-folder (str temp-dir))]
          (match (fs/extension (:output-filename result)) "svg")
          (match (fs/size (:output-filename result)) pos-int?))
        (finally
          (fs/delete-tree temp-dir))))))
