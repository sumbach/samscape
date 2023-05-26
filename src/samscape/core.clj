(ns samscape.core
  (:require [clojure.java.io :as io]
            [clojure.string :as string])
  (:import (java.net Socket)
           (java.io FilterInputStream FilterOutputStream)))

(defn parse [s]
  (assert (string/starts-with? s "http://"))
  (let [x (subs s (.length "http://"))
        [host path] (string/split x #"/" 2)
        path (str "/" path)]
    {:host host :path path}))

(defn request-payload [url]
  (format (str "GET %s HTTP/1.0\r\n"
               "Host: %s\r\n"
               "\r\n")
          (:path url)
          (:host url)))

(defn read-headers [r]
  (loop [headers []]
    (let [line (.readLine r)]
      (if (= "" line)
        headers
        (let [[header value] (string/split line #":" 2)
              value (string/trim value)]
          (recur (conj headers [header value])))))))

(defn os [socket]
  (proxy [FilterOutputStream] [(.getOutputStream socket)]
    (close []
      (.shutdownOutput socket))))

(defn is [socket]
  (proxy [FilterInputStream] [(.getInputStream socket)]
    (close []
      #_(.shutdownInput socket)))) ;; TODO: do we need to catch and ignore when other side has already shutdown output?

(defn resolve-host-port [url]
  (merge {:port 80} url))

(defn request [s]
  (let [url (parse s)
        {:keys [host port]} (resolve-host-port url)
        payload (request-payload url)]
    (with-open [socket (Socket. host port)]
      (with-open [o (os socket)]
        (.write o (.getBytes payload "UTF-8"))
        (.flush o)) ;; TODO: is this necessary?
      (with-open [i (is socket)
                  r (io/reader i)]
        (let [status-line (.readLine r)
              [version status explanation] (string/split status-line #" " 3)
              _ (assert (= "200" status) status-line)
              headers (read-headers r)
              _ (assert (not-any? (fn [[k _]] (#{"transfer-encoding" "content-encoding"} (string/lower-case k))) headers) (pr-str headers))
              body (slurp r)]
          [[version status explanation] headers body])))))
