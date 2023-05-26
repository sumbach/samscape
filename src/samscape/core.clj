(ns samscape.core
  (:require [clojure.java.io :as io]
            [clojure.string :as string])
  (:import (java.net Socket InetSocketAddress)
           (java.io FilterInputStream FilterOutputStream)
           (javax.net.ssl SSLSocketFactory)))

(defn parse [s]
  (let [[scheme rst] (string/split s #"://" 2)
        _ (assert (#{"http" "https"} scheme) (format "Unknown scheme %s" (pr-str scheme)))
        [host path] (string/split rst #"/" 2)
        path (str "/" path)]
    {:scheme scheme :host host :path path}))

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
  (merge
   (cond
     (= "http"  (:scheme url)) {:port 80}
     (= "https" (:scheme url)) {:port 443})
   url))

(defn socket-for [{:keys [scheme host port]}]
  (case scheme
    "http"
    (doto (Socket.)
      (.connect (InetSocketAddress. host port) 5000))

    "https"
    (doto (-> (SSLSocketFactory/getDefault) (.createSocket))
      (.connect (InetSocketAddress. host port) 5000))))

(defn request [s]
  (let [url (parse s)
        payload (request-payload url)]
    (with-open [socket (socket-for (resolve-host-port url))]
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

(defn show [body]
  (loop [body (seq body)
         in-angle false]
    (when-let [c (first body)]
      (cond
        (= \< c)
        (recur (rest body) true)

        (= \> c)
        (recur (rest body) false)

        :else
        (do
          (when (not in-angle)
            (print c))
          (recur (rest body) in-angle))))))

(defn load-page [s]
  (let [[_ _headers body] (request s)]
    (show body)))
