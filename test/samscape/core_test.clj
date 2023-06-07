(ns samscape.core-test
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is]]
            [samscape.core :as sut :refer [try-with-resources]])
  (:import (java.net ServerSocket)))

(deftest parse-test
  (is (thrown? java.lang.AssertionError (sut/parse "foo")))
  (is (thrown? java.lang.AssertionError (sut/parse "http:")))
  (is (thrown? java.lang.AssertionError (sut/parse "https:")))
  (is (thrown? java.lang.AssertionError (sut/parse "http:/")))
  (is (thrown? java.lang.AssertionError (sut/parse "https:/")))
  (is (= {:scheme "http" :host ""            :path "/"} (sut/parse "http://")))
  (is (= {:scheme "http" :host "example.com" :path "/"} (sut/parse "http://example.com")))
  (is (= {:scheme "http" :host "example.com" :path "/"} (sut/parse "http://example.com/")))
  (is (= {:scheme "http" :host "example.com" :path "/foo/bar"} (sut/parse "http://example.com/foo/bar")))
  (is (= {:scheme "http" :host "example.com" :path "/foo/bar/"} (sut/parse "http://example.com/foo/bar/")))
  (is (= {:scheme "https" :host "example.com" :path "/foo/bar/"} (sut/parse "https://example.com/foo/bar/"))))

(defn read-request [buffered-reader]
  (loop [lines []]
    (let [line (.readLine buffered-reader)]
      (if (not= "" line)
        (recur (conj lines line))
        lines))))

(deftest request-test
  (with-redefs [sut/resolve-host-port (fn [url] (merge url {:host "localhost" :port 22222}))]
    (try-with-resources [server-socket (ServerSocket. 22222)]
      (let [f-socket (future (.accept server-socket))
            f-client (future (sut/request "http://example.com/foo/bar"))]
        (try
          (try-with-resources [socket (deref f-socket 500 ::TIMEOUT)]
            (is (not= ::TIMEOUT socket))
            (try-with-resources [i (sut/is socket)
                                 o (sut/os socket)]
              (is (= ["GET /foo/bar HTTP/1.0" "Host: example.com"] (read-request (io/reader i))))
              (.write o (.getBytes "HTTP/1.0 200 OK\r\nfoo:bar\r\nbaz:  z  \r\n\r\nhello\n" "UTF-8")))
            (let [res (deref f-client 500 ::TIMEOUT)]
              (is (not= ::TIMEOUT res))
              (is (= [["HTTP/1.0" "200" "OK"] [["foo" "bar"] ["baz" "z"]] "hello\n"] res))))
          (finally
            (some-> f-socket (deref 0 nil) .close)))))
    (try-with-resources [server-socket (ServerSocket. 22222)]
      (future
        (loop []
          (when-let [socket (.accept server-socket)]
            (try-with-resources [i (sut/is socket)
                                 o (sut/os socket)]
              (read-request (io/reader i)) ;; silently discard request
              (io/copy (-> "fixtures/http://example.org/index.html" io/resource io/input-stream) o))
            (recur))))
      (is (= [["HTTP/1.1" "200" "OK"]
              [["Accept-Ranges" "bytes"]
               ["Age" "319376"]
               ["Cache-Control" "max-age=604800"]
               ["Content-Type" "text/html; charset=UTF-8"]
               ["Date" "Wed, 07 Jun 2023 17:54:39 GMT"]
               ["Etag" "\"3147526947\""]
               ["Expires" "Wed, 14 Jun 2023 17:54:39 GMT"]
               ["Last-Modified" "Thu, 17 Oct 2019 07:18:26 GMT"]
               ["Server" "ECS (nyb/1D10)"]
               ["Vary" "Accept-Encoding"]
               ["X-Cache" "HIT"]
               ["Content-Length" "1256"]]]
             (butlast (sut/request "http://example.org/index.html"))))
      (is (re-find #"(?m)^ *Example Domain *$"
                   (with-out-str (sut/load-page "http://example.org/index.html")))))
    ;; source: https://stackoverflow.com/a/10176685
    ;; openssl req -x509 -newkey rsa:4096 -keyout example.org.pem -out example.org.pem -sha256 -days 3650 -nodes -subj "/CN=example.org"
    ;; source: https://docs.oracle.com/cd/E19509-01/820-3503/ggfhb/index.html
    ;; openssl pkcs12 -export -in example.org.pem -out example.org.p12 -name example.org -noiter -nomaciter
    (try-with-resources [server-socket (-> (sut/ssl-context {:keystore {:type "PKCS12" :filename "example.org.p12" :passphrase "foobarbaz"}})
                                           (.getServerSocketFactory)
                                           (.createServerSocket 22222))]
      (future
        (loop []
          (when-let [socket (.accept server-socket)]
            (try-with-resources [i (sut/is socket)
                                 o (sut/os socket)]
              (read-request (io/reader i)) ;; silently discard request
              (io/copy (-> "fixtures/https://example.org/index.html" io/resource io/input-stream) o))
            (recur))))
      (with-redefs [sut/ssl-socket-factory #(.getSocketFactory (sut/ssl-context {:truststore {:type "PKCS12" :filename "example.org.p12" :passphrase "foobarbaz"}}))] ;; TODO: HACK: should not use the keystore (including private key) as truststore
        (is (= [["HTTP/1.1" "200" "OK"]
                [["Accept-Ranges" "bytes"]
                 ["Age" "319303"]
                 ["Cache-Control" "max-age=604800"]
                 ["Content-Type" "text/html; charset=UTF-8"]
                 ["Date" "Wed, 07 Jun 2023 17:54:47 GMT"]
                 ["Etag" "\"3147526947+gzip\""]
                 ["Expires" "Wed, 14 Jun 2023 17:54:47 GMT"]
                 ["Last-Modified" "Thu, 17 Oct 2019 07:18:26 GMT"]
                 ["Server" "ECS (nyb/1D10)"]
                 ["Vary" "Accept-Encoding"]
                 ["X-Cache" "HIT"]
                 ["Content-Length" "1256"]]]
               (butlast (sut/request "https://example.org/index.html"))))
        (is (re-find #"(?m)^ *Example Domain *$"
                     (with-out-str (sut/load-page "https://example.org/index.html"))))))))

(comment
  (with-redefs [sut/resolve-host-port (fn [url] (merge url {:scheme "http" :host "localhost" :port 22222}))]
    (try-with-resources [server-socket (ServerSocket. 22222)]
      (future
        (loop []
          (when-let [socket (.accept server-socket)]
            (try-with-resources [i (sut/is socket)
                                 o (sut/os socket)]
              (slurp i) ;; silently discard request
              #_(io/copy (-> "fixtures/http://example.org/index.html" io/resource io/input-stream) o)
              (io/copy (-> "fixtures/https://browser.engineering/examples/xiyouji.html" io/resource io/input-stream) o))
            (recur))))
      #_(sut/load-page-gui "http://example.org/index.html")
      (sut/load-page-gui "https://browser.engineering/examples/xiyouji.html")))
  )
