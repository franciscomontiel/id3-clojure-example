(ns id3.core
  (:require [clojure.java.jdbc :as jdbc])
  (:gen-class
    :name id3.core
    :methods [#^{:static true} [getid3 [String String String String String String] String]])
)

(def db-spec {:classname "com.mysql.jdbc.Driver"
         :subprotocol "mysql"
         :subname "//localhost:3306/id3"
         :user "root"
         :password "paco"})

(defn set-db-conexion[subname user password] 
	(def db-spec {:classname "com.mysql.jdbc.Driver"
         :subprotocol "mysql"
         :subname subname
         :user user
         :password password}
         )
	)

(def ^:dynamic arbol
	)

(def letras )

(defn agregarLetra [letra]
	(def letras (str letras letra))
)

(defn reiniciarLetras []
	(def letras "")
)

(defn agregarValor [valor]
	(def letras (str letras valor "|"))
)
(defn getLetra [letra]
letras
)

(defn log2 [n]
	(if(zero? n)
	0
(/ (Math/log n) (Math/log 2))))

(defn doQuery [query]
(jdbc/query db-spec [query]))

(defn get-list-from-string [cadena]
	(clojure.string/split cadena #",")
)

(defn get-list-from-first-listmap [mapa lista]
	(if (empty? mapa)
		lista
		(cons 
			(first(vals(first mapa))) 
			(get-list-from-first-listmap (rest mapa) lista)
		)
	)
)

(defn get-value-from-listmap [mapa]
	(first(get-list-from-first-listmap mapa [])
))

(defn get-lists-from-listmap [mapa lista]
	(if (empty? mapa)
		lista
		(cons 
			(vals(first mapa))
			(get-lists-from-listmap (rest mapa) lista)
		)
	)
)

(defn get-list-from-listmap [mapa lista]
	(if (empty? mapa)
		lista
		(concat
			(vals(first mapa))
			(get-list-from-listmap (rest mapa) lista)
		)
	)
)
(defn get-distinct [column-name-base table-name]
(let [resultado (doQuery (str "SELECT DISTINCT " column-name-base " FROM " table-name))]
	(get-list-from-first-listmap resultado [])
	)
)

(defn get-count-from-query [column column-value table-name]
(let [resultado (doQuery (str 
	"SELECT COUNT(*) FROM " table-name 
	" WHERE " column
	" = '" column-value "'"))]
	(get-value-from-listmap resultado)
	)
)

(defn get-distinct-base-from-query [column column-value column-name-base table-name]
(let [resultado (doQuery (str 
	"SELECT DISTINCT " column-name-base 
	" FROM " table-name 
	" WHERE " column
	" = '" column-value 
	"'"))]
	(get-lists-from-listmap resultado [])
	)
)

(defn get-key [column-name table-name]
	(let [resultado (doQuery (str 
	"SELECT " column-name
	" FROM " table-name
	" LIMIT 1" 
	))]
	(first (keys (first resultado)))
		)
	)

(defn get-list-from-mapa-with-keyword [mapa keyw lista]
	(if (empty? mapa)
		lista
		(cons
			(get (first mapa) keyw)
			(get-list-from-mapa-with-keyword (rest mapa) keyw lista)
			)
		)
	)

(defn get-valor-mas-popular [mapa keyw valores-posible]
	(let [lista (frequencies(get-list-from-mapa-with-keyword mapa keyw []))]
		(let [cantidadP (get lista (first valores-posible))]
		(let [cantidadN (get lista (second valores-posible))]
		(if (> cantidadP cantidadN)
			(first valores-posible)
			(second valores-posible)
			)
		)
	)
	)
	)

(defn entropy-column [column-name table-name column-name-base]
(let [valores-posible 
	(get-list-from-first-listmap
		(doQuery (str "SELECT DISTINCT " column-name " FROM " table-name))[])]
	(let [positivo (first (get-distinct column-name-base table-name))]
	(let [negativo (second(get-distinct column-name-base table-name))]
	(loop[i 0 sum 0]
		(if(< i  (count valores-posible))
			(let [ queryPositiva (str "SELECT COUNT(" column-name 
				") FROM " table-name 
				" WHERE " column-name " = '" (nth valores-posible i) "' 
				AND " column-name-base " = '" positivo "'")]
			;(println queryPositiva)
			(let [ queryNegativa (str "SELECT COUNT(" column-name 
				") FROM " table-name 
				" WHERE " column-name " = '" (nth valores-posible i) "' 
				AND " column-name-base " = '" negativo "'")]
			;(println queryNegativa)
			(let [pos (get-value-from-listmap(doQuery queryPositiva))]
			(let [neg (get-value-from-listmap(doQuery queryNegativa))]
			;(println pos)
			;(println neg)
			(let [res 
				(+ ( * (- pos) (log2 (/ pos (+ pos neg))))
				   ( * (- neg) (log2 (/ neg (+ pos neg)))) )  ]
			;(println res)
			(if (= i (- (count valores-posible) 1))
				(+ sum res)
				(recur (inc i) ( + sum res))
			)
		)))))
	)
)))))


(defn get-best-node [list-columns table-name column-name-base]
	(if(empty? (rest list-columns))
		(hash-map "entropia" (entropy-column (first list-columns) table-name column-name-base) 
			"column" (first list-columns) )
	(let [entropiaX 
		(hash-map 
			"entropia" (entropy-column (first list-columns) table-name column-name-base)
			"column" (first list-columns)
			)]
			(if (< (get entropiaX "entropia") 
				((get-best-node (rest list-columns) table-name column-name-base) "entropia"))
				entropiaX
				(get-best-node (rest list-columns) table-name column-name-base)
				)
		
	)
)
)

(defn get-id3-helper [ejemplos atributosPredictores column-name-base table-name valores-base lista viejosEjemplos]
	(cond 
		(every? (fn [row] (= (first valores-base) (get row (get-key column-name-base table-name))))ejemplos)
		(do 
			(println "-")
			(conj lista (conj [(first valores-base)] 3))
		    (agregarValor (first valores-base))
		    (agregarValor 3)
		)
		(every? (fn [row] (= (second valores-base) (get row (get-key column-name-base table-name))))ejemplos)
		(do 
			(println "+")
			(conj lista (conj [(second valores-base)] 3))
		    (agregarValor (second valores-base))
		    (agregarValor 3)
		)
		(empty? atributosPredictores)
		(do 
			(println " popular")
			(conj lista (conj [3] "Valor + popular"))
		    (agregarValor (str "Popular :" (get-valor-mas-popular ejemplos (keyword column-name-base) valores-base)))
		   	(agregarValor 3)
		)
		(empty? ejemplos)
		(do
			(println " popular")
			(conj lista (conj [3] "Valor + popular"))
		    (agregarValor (str "Popular :" (get-valor-mas-popular viejosEjemplos (keyword column-name-base) valores-base)))
		   	(agregarValor 3)
		)
		:else (let [nodo (get (get-best-node atributosPredictores table-name column-name-base) "column")]
			(let [valores-posible (get-list-from-first-listmap(doQuery (str "SELECT DISTINCT " nodo " FROM " table-name))[])]
				(println (str"Entro a variables de mejor nodo con " nodo))
				(agregarValor nodo)
				(agregarValor 1)
				(loop[i 0 listaM []]
					(if (< i  (count valores-posible))
						(let [nuevosAtributosPredictores (remove (fn [row] (= nodo row)) atributosPredictores)]
							;(let [nuevosAtributosPredictores (rest atributosPredictores) ]
							(println (str"Entro a variables con valor " (nth valores-posible i)))
							(agregarValor (nth valores-posible i))
							(agregarValor 2)
							;(println nuevosEjemplos)
							;(println atributosPredictores)
							(let [nuevosEjemplos (filter (fn [row] (=
										(nth valores-posible i) 
										(get row (get-key nodo table-name)))
									) ejemplos)]
							(let [res
								(get-id3-helper 
									nuevosEjemplos
									nuevosAtributosPredictores
									column-name-base
									table-name
									valores-base
									(conj (conj lista (nth valores-posible i))2)
									ejemplos)]
							(if (= i (- (count valores-posible) 1))
								(do(agregarValor "REGRESA")
		   						(agregarValor 4)
							(conj  [nodo , 1] (concat res listaM)))
							(recur (inc i)(conj listaM res))
							))
						)))
				)
				)))
	)

(defn get-id3 [atributosPredictores column-name-base table-name]
	(reiniciarLetras)
	(get-id3-helper (doQuery (str 
		"SELECT "(clojure.string/join ", " atributosPredictores) 
		" , " column-name-base
		" FROM " table-name
		))
	atributosPredictores
	column-name-base 
	table-name 
	(get-list-from-listmap (doQuery (str "SELECT DISTINCT " column-name-base " FROM " table-name))[])
	[]
 	(doQuery (str 
		"SELECT "(clojure.string/join ", " atributosPredictores) 
		" , " column-name-base
		" FROM " table-name
		))
	)
)

(defn -getid3 [atributosPredictores column-name-base table-name subname user password]
	(set-db-conexion subname user password)
	(get-id3 (get-list-from-string atributosPredictores)
	column-name-base 
	table-name)
	letras
)

(defn -main [atributosPredictores column-name-base table-name]
	[]
)

