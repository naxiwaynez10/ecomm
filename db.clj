(ns db
  (:require [clojure.string :as str]))

(def database (atom {}))

(defn split-pipe [data]
  (map (fn [v]
         (str "\"" v "\"")) (str/split  data #"\|")))

(defn- customers->map [text]
  (let [customers (slurp text)
        each-lines (str/split-lines customers)
        spl-fn (mapv (fn [line]
                       (let [lines (split-pipe line)
                             id (first lines)
                             name (nth lines 1)
                             address (nth lines 2)
                             phone (last lines)]
                         {:id id :address address :name name :phone phone})) each-lines)]
    spl-fn))

(defn- products->map [text]
  (let [products (slurp text)
        each-lines (str/split-lines products)
        spl-fn (mapv (fn [line]
                       (let [lines (split-pipe line)
                             id (first lines)
                             desc (second lines)
                             cost (last lines)]
                         {:id id :description desc :cost cost})) each-lines)]
    spl-fn))

(defn- sales->map [text]
  (let [sales (slurp text)
        each-lines (str/split-lines sales)
        spl-fn (mapv (fn [line]
                       (let [lines (split-pipe line)
                             id (first lines)
                             custID (nth lines 1)
                             prodID (nth lines 2)
                             itemCount (last lines)]
                         {:id id :pid prodID :cid custID :qty itemCount})) each-lines)]
    spl-fn))

(defn customers []
  (customers->map "cust.txt"))

(defn products []
  (products->map "prod.txt"))

(defn sales []
  (sales->map "sales.txt"))

(defn core []
  (let
   [customers (customers)
    products (products)
    sales (sales)]
    (swap! database assoc :customers customers :sales sales :products products))
  @database)
