(ns app
  (:require [clojure.string :as str]
            [db]
            [menu]))

(def state (atom {}))

(menu/core)

(def db (db/core))

(defn str->number [str]
  (Integer/parseInt (str/replace str  "\"" "")))

(defn str->float [str]
  (Double/parseDouble (str/replace str  "\"" "")))


;; (defn fix-points [float]
;;   (let [point-index (str/index-of float ".")
;;         cut (str/)]))
(defn customer-table []
  (let [customers (:customers db)]
    (doseq [customer customers]
      (let [{id :id name :name address :address phone :phone} customer]
        (print (str (str->number id) ": [" name " " address " " phone "] \n"))))))

(defn products-table []
  (let [customers (:products db)]
    (doseq [customer customers]
      (let [{id :id item :description amount :cost} customer]
        (print (str (str->number id) ": [" item " " amount "] \n"))))))

(defn search- [text key]
  (first (filter #(= (str->number (:id %)) (str->number text)) (key db))))

(defn search-customer-by-name [state text]
  (when-not (empty? text)
    (filter #(str/includes? (str/lower-case (:name %)) (str/lower-case text)) (vals (:search @state)))))

(defn search-product-by-name [state text]
  (when-not (empty? text)
    (filter #(str/includes? (str/lower-case (:name %)) (str/lower-case text)) (vals (:search @state)))))

(defn sales-table []
  (let [sales (:sales db)]
    (doseq [sale sales]
      (let [{id :id pid :pid cid :cid qty :qty} sale
            name (:name (search- cid :customers))
            description (:description (search- pid :products))]
        (print (str (str->number id) ": [" name " " description " " (str->number qty) "] \n"))))))


(defn search-sales []
  (flush)
  (let [sales (:sales db)
        text (read-line)]
    (mapv (fn [sale]
            (let [{id :id pid :pid cid :cid qty :qty} sale
                  name (:name (search- cid :customers))
                  price (:cost (search- pid :products))]
              (swap! state assoc-in [:search id] {:name name :total (* (str->number qty) (str->float price))})))
          sales)
    (let [v (search-product-by-name state text)
          total (if-not (or (empty? v) (and (> (count v) 1) (seq? v))) (reduce (fn [curr acc]
                                                           (+ (:total curr) (:total acc)))  (when-let [t (search-customer-by-name state text)] t))
                        "No user found")]
      (if (map? total)
        (print (str (:name total) ": " (:total total)))
        (if (number? total) (print (str (:name (first v)) ": $" total)) (print total))))))

(defn search-sales-items []
  (flush)
  (let [sales (:sales db)
        text (read-line)]
    (mapv (fn [sale]
            (let [{id :id pid :pid qty :qty} sale
                  description (:description (search- pid :products))]
              (swap! state assoc-in [:search id] {:name description :total (str->number qty)})))
          sales)
    (let [v (search-customer-by-name state text)
          total (if-not (or (empty? v) (and (> (count v) 1) (seq? v))) (reduce (fn [curr acc]
                                                           (+ (:total curr) (:total acc)))  (when-let [t (search-product-by-name state text)] t))
                        "No Product found")]
      (if (map? total)
        (print (str (:name total) ": " (:total total)))
        (if (number? total) (print (str (:name (first v)) ": " total)) (print total))))))



(def input (read-line))
(flush)
(defn -main []
  (case input
    "1" (customer-table)
    "2" (products-table)
    "3" (sales-table)
    "4" (do  (print "Search a customer by name...\n")
             (search-sales))
    "5" (do  (print "Search a customer by name...\n")
             (search-sales-items))
    "exit" nil
    (print "Please select an option")))

