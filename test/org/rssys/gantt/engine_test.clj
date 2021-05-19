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
    (let [correct-edn1 (sut/read-gantt-struct "test/data/ex01-fixed-dates-calendar.edn")
          correct-edn2 (sut/read-gantt-struct "test/data/ex02-durations-only.edn")
          correct-edn3 (sut/read-gantt-struct "test/data/ex03-reverse-order-planning.edn")]
      (match correct-edn1 {:project-starts "2021-05-01"
                           :project-scale  :daily
                           :task-colors    {:color/in-progress "GreenYellow/Red"
                                            :color/completed   "GreenYellow/Green"}
                           :closed-days    #{:saturday :sunday}
                           :holidays       ["2021-05-03" "2021-05-10"]})
      (match (-> correct-edn2 :tasks first) {:name             "task1"
                                             :alias            :t1
                                             :days-lasts       10
                                             :percent-complete 100})
      (match (-> correct-edn3 :tasks first) {:name             "whole project"
                                             :alias            :t5
                                             :days-lasts       30
                                             :percent-complete 0
                                             :color            "Gray"})))

  (testing "read bad EDN-data throws Exception"
    (let []
      (is (thrown-with-msg?
            ExceptionInfo
            #"has invalid structure"
            (sut/read-gantt-struct "test/data/bad-data/ex01-bad-data.edn"))))))


(deftest ^:unit write-content->file-test

  (testing "Asciidoc content generates successfully"
    (let [gantt-struc1          (sut/read-gantt-struct "test/data/ex01-fixed-dates-calendar.edn")
          gantt-struc2          (sut/read-gantt-struct "test/data/ex02-durations-only.edn")
          gantt-struc3          (sut/read-gantt-struct "test/data/ex03-reverse-order-planning.edn")
          gantt-struc4          (sut/read-gantt-struct "test/data/ex04-header-title-footer.edn")

          ex-01-temp-file       (str (fs/delete-on-exit (File/createTempFile "ex01-" ".adoc")))
          ex-02-temp-file       (str (fs/delete-on-exit (File/createTempFile "ex02-" ".adoc")))
          ex-03-temp-file       (str (fs/delete-on-exit (File/createTempFile "ex03-" ".adoc")))
          ex-04-temp-file       (str (fs/delete-on-exit (File/createTempFile "ex04-" ".adoc")))

          gantt-content1        (sut/make-gantt-content gantt-struc1)
          gantt-content2        (sut/make-gantt-content gantt-struc2)
          gantt-content3        (sut/make-gantt-content gantt-struc3)
          gantt-content4        (sut/make-gantt-content gantt-struc4)

          ex01-expected-content (slurp "test/data/results/ex01-fixed-dates-calendar.adoc")
          ex02-expected-content (slurp "test/data/results/ex02-durations-only.adoc")
          ex03-expected-content (slurp "test/data/results/ex03-reverse-order-planning.adoc")
          ex04-expected-content (slurp "test/data/results/ex04-header-title-footer.adoc")]

      (sut/write-content->file
        (sut/gantt-content->asciidoc-content gantt-content1 :img-format :svg)
        ex-01-temp-file)

      (sut/write-content->file
        (sut/gantt-content->asciidoc-content gantt-content2 :img-format :svg)
        ex-02-temp-file)

      (sut/write-content->file
        (sut/gantt-content->asciidoc-content gantt-content3 :img-format :svg)
        ex-03-temp-file)

      (sut/write-content->file
        (sut/gantt-content->asciidoc-content gantt-content4 :img-format :png)
        ex-04-temp-file)

      (match (slurp ex-01-temp-file) ex01-expected-content)
      (match (slurp ex-02-temp-file) ex02-expected-content)
      (match (slurp ex-03-temp-file) ex03-expected-content)
      (match (slurp ex-04-temp-file) ex04-expected-content)

      (fs/delete-if-exists ex-01-temp-file)
      (fs/delete-if-exists ex-02-temp-file)
      (fs/delete-if-exists ex-03-temp-file)
      (fs/delete-if-exists ex-04-temp-file)))

  (testing "PUML content generates successfully"
    (let [gantt-struc1          (sut/read-gantt-struct "test/data/ex01-fixed-dates-calendar.edn")
          gantt-struc2          (sut/read-gantt-struct "test/data/ex02-durations-only.edn")
          gantt-struc3          (sut/read-gantt-struct "test/data/ex03-reverse-order-planning.edn")
          gantt-struc4          (sut/read-gantt-struct "test/data/ex04-header-title-footer.edn")

          ex-01-temp-file       (str (fs/delete-on-exit (File/createTempFile "ex01-" ".puml")))
          ex-02-temp-file       (str (fs/delete-on-exit (File/createTempFile "ex02-" ".puml")))
          ex-03-temp-file       (str (fs/delete-on-exit (File/createTempFile "ex03-" ".puml")))
          ex-04-temp-file       (str (fs/delete-on-exit (File/createTempFile "ex04-" ".puml")))

          gantt-content1        (sut/make-gantt-content gantt-struc1)
          gantt-content2        (sut/make-gantt-content gantt-struc2)
          gantt-content3        (sut/make-gantt-content gantt-struc3)
          gantt-content4        (sut/make-gantt-content gantt-struc4)

          ex01-expected-content (slurp "test/data/results/ex01-fixed-dates-calendar.puml")
          ex02-expected-content (slurp "test/data/results/ex02-durations-only.puml")
          ex03-expected-content (slurp "test/data/results/ex03-reverse-order-planning.puml")
          ex04-expected-content (slurp "test/data/results/ex04-header-title-footer.puml")]

      (sut/write-content->file
        (sut/gantt-content->puml-content gantt-content1)
        ex-01-temp-file)

      (sut/write-content->file
        (sut/gantt-content->puml-content gantt-content2)
        ex-02-temp-file)

      (sut/write-content->file
        (sut/gantt-content->puml-content gantt-content3)
        ex-03-temp-file)

      (sut/write-content->file
        (sut/gantt-content->puml-content gantt-content4)
        ex-04-temp-file)

      (match (slurp ex-01-temp-file) ex01-expected-content)
      (match (slurp ex-02-temp-file) ex02-expected-content)
      (match (slurp ex-03-temp-file) ex03-expected-content)
      (match (slurp ex-04-temp-file) ex04-expected-content)

      (fs/delete-if-exists ex-01-temp-file)
      (fs/delete-if-exists ex-02-temp-file)
      (fs/delete-if-exists ex-03-temp-file)
      (fs/delete-if-exists ex-04-temp-file))))

(deftest ^:unit generate-gantt-picture-test

  (testing "PNG generated from PUML file successfully"
    (let [temp-dir (fs/create-temp-dir {:prefix "puml-"})]
      (try
        (let [result (sut/generate-gantt-picture "test/data/results/ex04-header-title-footer.puml" :img-format :png :output-folder (str temp-dir))]
          (match (fs/extension (:output-filename result)) "png")
          (match (fs/size (:output-filename result)) pos-int?))
        (finally
          (fs/delete-tree temp-dir)))))

  (testing "SVG generated from PUML file successfully"
    (let [temp-dir (fs/create-temp-dir {:prefix "puml-"})]
      (try
        (let [result (sut/generate-gantt-picture "test/data/results/ex04-header-title-footer.puml" :img-format :svg :output-folder (str temp-dir))]
          (match (fs/extension (:output-filename result)) "svg")
          (match (fs/size (:output-filename result)) pos-int?))
        (finally
          (fs/delete-tree temp-dir)))))

  )
