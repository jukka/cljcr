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

(ns test-cljcr
  (:use [clojure.test]
        [cljcr]))

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
    (let [rep-name (. (repository) getDescriptor javax.jcr.Repository/REP_NAME_DESC)
          rep-vendor (. (repository) getDescriptor javax.jcr.Repository/REP_VENDOR_DESC)
          rep-version (. (repository) getDescriptor javax.jcr.Repository/REP_VERSION_DESC)]
      (println (format "Name: %s\nVendor: %s\nVersion: %s"
                        rep-name rep-vendor rep-version)))))

(use-fixtures
  :once
  (fn [test] 
    (with-repository jackrabbit-repo 
      (with-session {:username "username" :password "password"} 
	(test)))))

(deftest test-descriptors
  (testing "descriptors"
    (is (= (get (descriptors) "jcr.repository.name") "Jackrabbit"))))

(deftest test-has-changes?
  (is (not (has-changes?)) "there should be no unsaved changes"))

(deftest test-node?
  (testing "node?"
    (is (node? (root-node)) "a node")
    (is (not (node? (property-at "/jcr:primaryType"))) "a property")))

(deftest test-property?
  (testing "property"
    (is (property? (property-at "/jcr:primaryType")) "a property")
    (is (not (property? (root-node))) "a node")))

(deftest test-node-at
  (testing "node-at"
    (let [path "/jcr:system"]
      (is (= (. (node-at path) getPath) path)
          "the path of (node-at \"/jcr:system\") /jcr:system?"))))

(deftest test-property-at
  (testing "property-at"
    (let [path "/jcr:primaryType"]
      (is (= (. (property-at path) getPath) path)
          "the path of (property-at \"/jcr:primaryType\") /jcr:primaryType?"))))

(deftest test-set-properties
  (testing "set-properties"
    (let [test-node (add-node (root-node) "testNode")]
      (set-properties test-node {"testProp" 5.0})
      (let [test-prop (property-at "/testNode/testProp")]
	(is (property? test-prop) "a property")
	(is (= (value (. test-prop getValue)) 5.0))))))