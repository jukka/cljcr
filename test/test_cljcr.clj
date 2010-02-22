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

;; TODO better property access
(deftest test-node?
  (with-repository jackrabbit-repo
    (with-guest-session
      (is (node? (root-node))
          "root node a node?")
      (is (not (node? (property-at "/jcr:primaryType")))
          "property a node?"))))

(deftest test-property?
  (with-repository jackrabbit-repo
    (with-guest-session
      (is (property? (property-at "/jcr:primaryType"))
          "/jcr:primaryType property a property?")
      (is (not (property? (root-node)))
          "node a property?"))))

(deftest test-node-at
  (with-repository jackrabbit-repo
    (with-guest-session
      (let [path "/jcr:system"]
        (is (= (. (node-at path) getPath) path)
            "the path of (node-at \"/jcr:system\") /jcr:system?")))))

(deftest test-property-at
  (with-repository jackrabbit-repo
    (with-guest-session
      (let [path "/jcr:primaryType"]
        (is (= (. (property-at path) getPath) path)
            "the path of (property-at \"/jcr:primaryType\") /jcr:primaryType?")))))
