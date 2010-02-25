;;    Copyright 2010 Adam C. Foltzer
;;    Copyright 2010 Jukka Zitting
;;
;;    Licensed under the Apache License, Version 2.0 (the "License");
;;    you may not use this file except in compliance with the License.
;;    You may obtain a copy of the License at
;;
;;      http://www.apache.org/licenses/LICENSE-2.0
;;
;;    Unless required by applicable law or agreed to in writing, software
;;    distributed under the License is distributed on an "AS IS" BASIS,
;;    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;;    See the License for the specific language governing permissions and
;;    limitations under the License.

(ns cljcr
  (:use (clojure.contrib [except :only (throwf)])
	clojure.test)
  (:import (javax.jcr Repository Session Node)))

(def *repo* nil)
(def *session* nil)

(defn repository []
  (or *repo* (throwf "No repository active")))

(defn session []
  (or *session* (throwf "No session active")))

(defn repository-factory []
  (first (java.util.ServiceLoader/load javax.jcr.RepositoryFactory)))

(defn get-repository [param-map]
  (if-let [factory (repository-factory)]
    (if-let [repo (. factory getRepository param-map)]
      repo
      (throwf "Could not get repository with parameters %s" param-map))
    (throwf "Could not load RepositoryFactory implementation")))

(defn credentials
  [{user :user pass :password}] 
  (if user
    (let [pass (if pass (char-array pass))]
      (javax.jcr.SimpleCredentials. user pass))
    (javax.jcr.GuestCredentials. )))

(defn get-session
  ([] (get-session {}))
  ([{user :user pass :password ws :workspace}]
     (let [creds (credentials {:user user :password pass})]
       (if ws
	 (. (repository) login creds ws)
	 (. (repository) login creds)))))

(defmacro with-repository
  [repo-params & body]
  `(with-bindings 
     {(var *repo*) (get-repository ~repo-params)}
     ~@body))

(defn descriptor
  #^{ :doc "Returns the string value(s) of a repository descriptor." }
  ([key]
    (descriptor (repository) key))
  ([repository key]
    (if (. repository isSingleValueDescriptor key)
      (. (. repository getDescriptorValue key) getString)
      (map #(. %1 getString) (. repository getDescriptorValues key)))))

(defn descriptors
  #^{ :doc "Returns a map of the repository descriptors." }
  ([] (descriptors (repository)))
  ([repository]
    (let [keys (. repository getDescriptorKeys)]
      (reduce #(assoc %1 %2 (descriptor repository %2)) {} keys))))

(defmacro with-session
  [session-params & body]
  `(with-bindings 
     {(var *session*) (get-session ~session-params)}
     (let [v# (do ~@body)]
       (. (session) logout)
       v#)))

(defmacro with-guest-session [& body]
  `(with-session {} ~@body))

(defn has-changes?
  #^{ :doc "Checks whether the current session has unsaved changes." }
  []
  (. (session) hasPendingChanges))

(defn save
  #^{ :doc "Saves the changes in the current session." }
  []
  (. (session) save))

(defn refresh
  #^{ :doc "Refreshes the current session." }
  []
  (. (session) refresh true))

(defn node? [x]
  (instance? javax.jcr.Node x))

(defn property? [x]
  (instance? javax.jcr.Property x))

(defn root-node []
  (. (session) getRootNode))

(defn node-at
  ([abs-path] (. (session) getNode abs-path))
  ([node rel-path] (. node getNode rel-path)))

(defn property-at
  ([abs-path] (. (session) getProperty abs-path))
  ([node rel-path] (. node getProperty rel-path)))

(defn parent [item]
  (. item getParent))

(defn properties [node]
  (iterator-seq (. node getProperties)))

(defn children [node]
  (iterator-seq (. node getNodes)))
