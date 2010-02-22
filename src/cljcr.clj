;;    Copyright 2010 Adam C. Foltzer
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
  (first (java.util.ServiceLoader/load
	  javax.jcr.RepositoryFactory)))

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

(defmacro with-session
  [session-params & body]
  `(with-bindings 
     {(var *session*) (get-session ~session-params)}
     (let [v# (do ~@body)]
       (. (session) logout)
       v#)))

(defmacro with-guest-session [& body]
  `(with-session {} ~@body))

(defn node? [x]
  (instance? javax.jcr.Node x))

(defn property? [x]
  (instance? javax.jcr.Property))

(defn root-node []
  (. (session) getRootNode))

(defn node-at [abs-path]
  (. (session) getNode abs-path))

(defn children [node]
  (iterator-seq (. node getNodes)))

(def jackrabbit-repo
     {org.apache.jackrabbit.core.RepositoryFactoryImpl/REPOSITORY_HOME 
      "repository/repository"
      org.apache.jackrabbit.core.RepositoryFactoryImpl/REPOSITORY_CONF
      "repository/repository.xml"})

;; Examples of using the API so far

;; TODO better way to access descriptors. Perhaps bind a map along
;; with *repo* containing the descriptors as Clojure keys?
(defn print-jackrabbit-info []
  (with-repository jackrabbit-repo
    (let [rep-name (. (repository) getDescriptor
		      javax.jcr.Repository/REP_NAME_DESC)
	  rep-vendor (. (repository) getDescriptor
			javax.jcr.Repository/REP_VENDOR_DESC)
	  rep-version (. (repository) getDescriptor
			 javax.jcr.Repository/REP_VERSION_DESC)]
      (println (format "Name: %s\nVendor: %s\nVersion: %s"
		       rep-name rep-vendor rep-version)))))

;; TODO better property access
(deftest test-node?
  (with-repository jackrabbit-repo
    (with-guest-session
      (is (node? (root-node)) 
	  "root node a node?")
      (is (not (node? (first (iterator-seq (. (root-node) getProperties)))))
	  "property a node?"))))

(deftest test-node-at
  (with-repository jackrabbit-repo
    (with-guest-session
      (let [path "/jcr:system"]
	(is (= (. (node-at path) getPath) path)
	    "the path of (node-at \"/jcr:system\") /jcr:system?")))))