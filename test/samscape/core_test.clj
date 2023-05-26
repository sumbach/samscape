(ns samscape.core-test
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is]]
            [samscape.core :as sut])
  (:import (java.net ServerSocket)))

(deftest parse-test
  (is (thrown? java.lang.AssertionError (sut/parse "foo")))
  (is (thrown? java.lang.AssertionError (sut/parse "http:")))
  (is (thrown? java.lang.AssertionError (sut/parse "http:/")))
  (is (= {:host ""            :path "/"} (sut/parse "http://")))
  (is (= {:host "example.com" :path "/"} (sut/parse "http://example.com")))
  (is (= {:host "example.com" :path "/"} (sut/parse "http://example.com/")))
  (is (= {:host "example.com" :path "/foo/bar"} (sut/parse "http://example.com/foo/bar")))
  (is (= {:host "example.com" :path "/foo/bar/"} (sut/parse "http://example.com/foo/bar/"))))

(deftest request-test
  (with-open [socket (ServerSocket. 22222)]
    (let [f-server (future (.accept socket))
          f-client (future (sut/request "http://example.com/foo/bar"))]
      (try
        (is (not= ::TIMEOUT (deref f-server 500 ::TIMEOUT)))
        (with-open [i (sut/is @f-server)
                    o (sut/os @f-server)]
          (is (= "GET /foo/bar HTTP/1.0\r\nHost: example.com\r\n\r\n" (slurp (io/reader i))))
          (.write o (.getBytes "HTTP/1.0 200 OK\r\nfoo:bar\r\nbaz:  z  \r\n\r\n" "UTF-8")))
        (is (not= ::TIMEOUT (deref f-client 500 ::TIMEOUT)))
        (is (= [["HTTP/1.0" "200" "OK"] [["foo" "bar"] ["baz" "z"]]] @f-client))
        (finally
          (try (some-> f-client deref .close) (catch Exception _)) ;; TODO: more specific catch?
          (some-> f-server deref .close))))))
