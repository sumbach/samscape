(ns samscape.core
  (:require [cljfx.api :as fx]
            [clojure.java.io :as io]
            [clojure.string :as string])
  (:import (java.io FilterInputStream FilterOutputStream)
           (java.lang StringBuilder)
           (java.net Socket InetSocketAddress)
           (javafx.scene.canvas Canvas)
           (javafx.scene.paint Color)
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

(defmacro try-with-resources [bindings & body]
  (if (= 0 (count bindings))
    `(do ~@body)
    `(let ~(subvec bindings 0 2)
       (let [[v# e#] (try
                       [(try-with-resources ~(subvec bindings 2) ~@body) nil]
                       (catch java.lang.Throwable e#
                         [nil e#]))
             [~'_ e2#] (try
                         [(. ^java.lang.AutoCloseable ~(bindings 0) close) nil]
                         (catch java.lang.Throwable e#
                           [nil e#]))]
         (when (and e# e2#)
           (.addSuppressed e# e2#))
         (if-let [e# (or e# e2#)]
           (throw e#)
           v#)))))

(defn request [s]
  (let [url (parse s)
        payload (request-payload url)]
    (with-open [socket (socket-for (resolve-host-port url))]
      (try-with-resources [o (os socket)]
        (.write o (.getBytes payload "UTF-8"))
        (.flush o)) ;; TODO: is this necessary?
      (try-with-resources [i (is socket)
                           r (io/reader i)]
        (let [status-line (.readLine r)
              [version status explanation] (string/split status-line #" " 3)
              _ (assert (= "200" status) status-line)
              headers (read-headers r)
              _ (assert (not-any? (fn [[k _]] (#{"transfer-encoding" "content-encoding"} (string/lower-case k))) headers) (pr-str headers))
              body (slurp r)]
          [[version status explanation] headers body])))))

(defn lex [body]
  (loop [body (seq body)
         sb (StringBuilder.)
         in-angle false]
    (if-let [c (first body)]
      (cond
        (= \< c)
        (recur (rest body) sb true)

        (= \> c)
        (recur (rest body) sb false)

        in-angle
        (recur (rest body) sb in-angle)

        :else
        (recur (rest body) (.append sb c) in-angle))
      (.toString sb))))

(defn show [body]
  (print (lex body)))

(defn load-page [s]
  (let [[_ _headers body] (request s)]
    (show body)))

(defonce *state
  (atom {:url nil
         :scroll 0
         :display-list []}))

(defn title-input [{:keys [title]}]
  {:fx/type :text-field
   :on-text-changed #(swap! *state assoc :title %)
   :text title})

(def hstep 13)
(def vstep 18)

(defn layout [width text]
  (->> (seq text)
       (reduce (fn [{:keys [display-list x y]} c] ;; TODO: change this to use `update` on a state map
                 (cond
                   (= \newline c)
                   {:display-list display-list :x hstep :y (+ y vstep vstep)}

                   (>= x (- width hstep hstep))
                   {:display-list (conj display-list [x y c]) :x hstep       :y (+ y vstep)}

                   :else
                   {:display-list (conj display-list [x y c]) :x (+ x hstep) :y y}))
               {:display-list [] :x hstep :y vstep})
       :display-list))

(defn canvas [{:keys [width height scroll display-list]}]
  (let [#_{:clj-kondo/ignore [:unused-binding]} progress 0.8]
    {:fx/type :canvas
     :width 800
     :height 600
     :draw (fn [^Canvas canvas]
             (let [ctx (.getGraphicsContext2D canvas)]
               (doto ctx
                 (.clearRect 0 0 width height)
                 (.setFill Color/LIGHTGREY)
                 #_(.fillRect 10 20 400 300)
                 #_(.strokeOval 100 100 150 150)
                 #_(.strokeText "Hi!" 5 5)
                 #_(.fillRoundRect 0 0 width height height height)
                 #_(.setFill Color/GREEN)
                 #_(.fillRoundRect 0 0 (* width progress) height height height))
               (doseq [[x y c] display-list]
                 (when (< (- scroll 0) y (+ height scroll vstep))
                   (.strokeText ctx (str c) x (- y scroll))))))}))

(def scroll-step 5)

(defn root [{:keys [url scroll display-list]}]
  {:fx/type :stage
   :showing (boolean url)
   :title url
   :scene {:fx/type :scene
           :accelerators {[:down] {:event/type ::scroll :offset scroll-step}
                          [:shift :down] {:event/type ::scroll :offset (* 10 scroll-step)}
                          [:up] {:event/type ::scroll :offset (- scroll-step)}
                          [:shift :up] {:event/type ::scroll :offset (* 10 (- scroll-step))}
                          ["j"] {:event/type ::scroll :offset scroll-step}
                          [:shift "j"] {:event/type ::scroll :offset (* 10 scroll-step)}
                          ["k"] {:event/type ::scroll :offset (- scroll-step)}
                          [:shift "k"] {:event/type ::scroll :offset (* 10 (- scroll-step))}}
           :root {:fx/type :v-box
                  :children [#_{:fx/type :label
                              :text "Window title input"}
                             #_{:fx/type title-input ;; TODO: how can we use accelerators _and_ this input?
                              :title url}
                             {:fx/type canvas
                              :width 800
                              :height 600
                              :scroll scroll
                              :display-list display-list}]}}})

(defmulti event-handler :event/type)

(defmethod event-handler ::scroll [{:keys [offset]}]
  #_(prn :args args)
  (swap! *state update :scroll + offset))

(defonce renderer
  (let [r (fx/create-renderer
           :middleware (fx/wrap-map-desc assoc :fx/type root)
           :opts {:fx.opt/map-event-handler event-handler})
        *mounted? (atom false)]
    (fn [& args]
      (when (compare-and-set! *mounted? false true)
        (fx/mount-renderer *state r))
      (apply r args))))

(defn load-page-gui [s]
  (let [[_ _headers body] (request s)
        display-list (layout 800 (lex body))] ;; TODO: width is duplicated here
    (swap! *state merge {:url s :display-list display-list})
    (renderer)))

(comment
  (load-page-gui "http://example.org/index.html")
  )

(comment
  (fx/on-fx-thread
   (fx/create-component
    {:fx/type :stage
     :showing true
     :title "Cljfx example"
     :width 300
     :height 100
     :scene {:fx/type :scene
             :root {:fx/type :v-box
                    :alignment :center
                    :children [{:fx/type :label
                                :text "Hello world"}]}}}))
  )
