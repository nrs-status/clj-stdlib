(ns clj-stdlib.portal-tools
  (:require
   [portal.client.jvm :as portal-client]))

(def mk-submit
  (fn [host port]
    (fn [x]
      (portal-client/submit {:port port :host host} (with-meta x {:portal.viewer/default :portal.viewer/pprint})))))

(def mk-submit-no-meta
  (fn [host port]
    (fn [x]
      (portal-client/submit {:port port :host host} x))))

(def mk-psubmit
  (fn [submit]
    (let [prev-atm (atom nil)]
      (fn [x]
        (let [prev-deref @prev-atm]
          (reset! prev-atm x)
          (submit {:prev prev-deref :next x}))))))
