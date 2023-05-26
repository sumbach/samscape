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
  (with-redefs [sut/resolve-host-port (constantly {:host "localhost" :port 22222})]
    (with-open [server-socket (ServerSocket. 22222)]
      (let [f-socket (future (.accept server-socket))
            f-client (future (sut/request "http://example.com/foo/bar"))]
        (try
          (with-open [socket (deref f-socket 500 ::TIMEOUT)]
            (is (not= ::TIMEOUT socket))
            (with-open [i (sut/is socket)
                        o (sut/os socket)]
              (is (= "GET /foo/bar HTTP/1.0\r\nHost: example.com\r\n\r\n" (slurp (io/reader i))))
              (.write o (.getBytes "HTTP/1.0 200 OK\r\nfoo:bar\r\nbaz:  z  \r\n\r\n" "UTF-8")))
            (let [res (deref f-client 500 ::TIMEOUT)]
              (is (not= ::TIMEOUT res))
              (is (= [["HTTP/1.0" "200" "OK"] [["foo" "bar"] ["baz" "z"]]] res))))
          (finally
            (some-> f-socket (deref 0 nil) .close)))))))
