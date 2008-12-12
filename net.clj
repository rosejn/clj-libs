(ns net
  (:use [clojure.contrib.javalog :only (log)])
  (:import (java.net InetSocketAddress)
           (java.util.concurrent Executors)
           (org.jboss.netty.bootstrap ServerBootstrap ClientBootstrap)
           (org.jboss.netty.channel ChannelFactory)
           (org.jboss.netty.channel.socket.nio NioServerSocketChannelFactory NioClientSocketChannelFactory)
           (java.util ArrayList List)
           (java.util.concurrent.atomic AtomicLong)

           (org.jboss.netty.channel ChannelEvent ChannelHandlerContext ChannelPipelineCoverage
                                    ChannelState ChannelStateEvent ExceptionEvent MessageEvent
                                    SimpleChannelHandler)
           (org.jboss.netty.handler.codec.serialization ObjectDecoder ObjectEncoder)))

(defn responder [event]
  (let [channel (.getChannel event)]
    #(.write channel %)))

(defn try-handler [handlers which & args]
  (if (contains? handlers which)
    (apply (which handlers) args)))

; TODO: Optionally take a hash of handlers to override all of these...
(defn generic-handler [handlers]
  (let [msg-counter 0]
    (proxy [SimpleChannelHandler] []
      ;      (handleUpstream [context event] (proxy-super handleUpstream context event))
      ; Setup serialization when the channel is created
      (channelOpen [context event] 
                   (log :info "Channel open... with handlers: " (keys handlers))
                   (try-handler handlers :on-open (.getPipeline (.getChannel event))))

      (channelConnected [context event] 
                        (log :info "Channel connected...")
                        (try-handler handlers :on-connect (responder event)))

      (messageReceived [context event] 
                       (log :info "Message received: " (.getMessage event))
                       (try-handler handlers :on-msg (.getMessage event) (responder event)))

      (exceptionCaught [context event] 
                       (let [cause (.getCause event)
                             s-writer (new java.io.StringWriter)
                             p-writer (new java.io.PrintWriter s-writer)]
                         (.printStackTrace cause p-writer)
                         ;(log :info "Got an exception: " (.getMessage cause) (.toString p-writer))
                         (try-handler handlers :on-error cause) 
                         (.close (.getChannel event)))))))

; The monitor is just a separate thread that periodically gathers some data
; about the status of the server channels.   
; TODO: have a generic periodic logging system connected to the monitor
(defn throughput-monitor [who]
  (proxy [Thread] []
    (run [] 
         (loop []
           (Thread/sleep 3000) 
           (println who "is alive...")
           (recur)))))

(def DEFAULT-SERVER-ADDR "localhost")

(defstruct net-server :type :channel :monitor)

(defn make-net-server [channel monitor] 
  (struct net-server :server channel monitor))

(defn setup-object-pipeline [pipeline]
  (log :info "Setting up an object encode/decode pipeline...")
  (.addFirst pipeline "encoder" (new ObjectEncoder))
  (.addFirst pipeline "decoder" (new ObjectDecoder)))

(defn setup-handlers [handler-or-hash]
  (let [handlers (if (associative? handler-or-hash)
                   handler-or-hash
                   {:on-msg handler-or-hash})
        handlers (if (contains? handlers :on-open)
                   handlers
                   (assoc handlers :on-open setup-object-pipeline))]
    handlers))

(defn server 
  ([port-num handler-or-hash]
             (let [chan-factory (new NioServerSocketChannelFactory
                                     (Executors/newCachedThreadPool)
                                     (Executors/newCachedThreadPool))
                   bootstrap (new ServerBootstrap chan-factory)
                   handlers (setup-handlers handler-or-hash)
                   handler (generic-handler handlers)
                   monitor (throughput-monitor "server")
                   p (.getPipeline bootstrap)]
               (.setOption bootstrap "child.tcpNoDelay" true)
               (.setOption bootstrap "child.keepAlive" true)
               (.addLast p "handler" handler)
               ;(.start monitor)
               (make-net-server 
                 (.bind bootstrap (new InetSocketAddress port-num))
                 monitor))))

(defmulti close :type)

(defmethod close :server 
  ([s] (.close s))
  ([s async] 
      (let [future (.close s)]
        (.await future)))
  ([s async timeout]
      (let [future (.close s)]
        (.await future timeout))))

(defstruct net-client :type :channel :monitor)

(defn make-net-client [channel monitor] 
  (struct net-client :client channel monitor))

(defn client [host port-num handler-or-hash]
  (let [factory (new NioClientSocketChannelFactory 
                     (Executors/newCachedThreadPool)
                     (Executors/newCachedThreadPool))
        bootstrap (new ClientBootstrap factory)
        handlers (setup-handlers handler-or-hash) 
        handler (generic-handler handlers)
        monitor (throughput-monitor "client")]
    (let [p (.getPipeline bootstrap)]
      (.addLast p "handler" handler))
    (.setOption bootstrap "child.tcpNoDelay" true)
    (.setOption bootstrap "child.keepAlive" true)
    ;(.start monitor)
    (make-net-client 
      (.connect bootstrap (new InetSocketAddress host port-num))
      monitor)))

