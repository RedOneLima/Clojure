; Kyle Hewitt
; PPL Noon 
; Spring 2017

;===================== Pascal's Triangle ==================================


(defn generateRow  
  ([x] (generateRow x '(1)))
  ([x y] 
   (if (= (first (rest x)) nil)
      (cons 1 y)
      (generateRow (rest x)(cons (+' (first x) (first (rest x))) y))     
   )
  )
)

(defn findRow [row]
  (if (= row 0) '(1)
    (generateRow (findRow (dec row)))
  )
)

(defn mypastri [row column]
 (cond
    (= row 0) 1 
    (> column row) (println "Outside Triangle!")
  :else
    (nth (findRow row) column)
 )
)

;====================== Merge / Mergesort ==============================

(defn finish-list [list1 list2]
 (if (empty? list1) list2 
   (finish-list (rest list1) (cons (first list1) list2))
 )
)

(defn merge-aux [list1 list2 newList]
 (let [first1 (first list1) first2 (first list2) rest1 (rest list1) rest2 (rest list2)]
  (cond
    (empty? list1) (finish-list list2 newList)
    (empty? list2) (finish-list list1 newList) 
    (> first1 first2) (merge-aux list1 rest2 (cons first2 newList))  
    (< first1 first2) (merge-aux rest1 list2 (cons first1 newList))
   :else
    (merge-aux rest1 rest2 (do (cons first1 newList) (cons first1 newList)))  
  ) 
 )
)


(defn mymerge [list1 list2]
 (if(empty? list1) list2
    (if(empty? list2) list1
        (finish-list (merge-aux list1 list2 ()) ())
    )
  )
)


(defn mymergesort  [x]
  (let [n (count x) [left right] (split-at (/ n 2) x)]
    (if (= n 1) x
      (mymerge (merge-sort left) (merge-sort right))
    )
  )
)

;========================= BST/ IOT ===============================================================

(defn Insert [Tree Element]
 (cond 
      (= Tree ())   (list Element nil nil)
      (= Tree nil)  (list Element nil nil)
      (<= Element (first Tree))  (list (first Tree) (Insert (first (rest Tree)) Element) (first (rest (rest Tree))))
  :else	     
      (list (first Tree) (first (rest Tree)) (Insert (first (rest (rest Tree))) Element))
  )
)

	

(defn mybuildbst
   ([List] (mybuildbst '() List))  
   ([Tree List] 
         (if (= List ()) 
           Tree 
           (mybuildbst (Insert Tree (first List)) (rest List)))  
   )
)

(defn myiot [Tree] 
     (if (= (first (rest Tree)) nil)
       (if (= (first (rest (rest Tree))) nil ) 
  	 (list (first Tree)) 
	 (concat (list (first Tree)) (myiot (first (rest (rest Tree)))))
       )
       (if (= (first (rest (rest Tree))) nil )
	    (concat  (myiot (first (rest Tree))) (list (first Tree)))
	    (concat (myiot(first (rest Tree))) (list (first Tree)) (myiot (first (rest (rest Tree)))))	  
      )
    )  
)
