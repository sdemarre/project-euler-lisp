(in-package :project-euler)

(defun problem-83-graph ()
  (let ((g (make-container 'graph-container :default-edge-type :directed :directed-edge-class 'directed-weighted-edge))
;;; 	 #(#(131	673	234	103	18)
;;; 	   #(201	96	342	965	150)
;;; 	   #(630	803	746	422	111)
;;; 	   #(537	699	497	121	956)
;;; 	   #(805	732	524	37	331))))
	(data (read-problem-n-data 83)))
    (let ((number-rows (length data))
	  (number-columns (length (aref data 0))))
      (let ((heapable-nodes (make-array (list number-rows number-columns) :initial-element nil)))
	(labels ((data-elt (row column) (aref (aref data row) column))
		 (vertex-symbol (row column) 
		   (sif (aref heapable-nodes row column)
			it
			(setf it (make-instance 'heapable-node :name (format nil "node-~a-~a" row column)))))
		 (add-edge-if-possible (source-row source-column dest-row dest-column)
		   (when (and (>= source-row 0) (>= source-column 0)
			      (< source-row number-rows) (< source-column number-columns))
		     (add-edge-between-vertexes g (vertex-symbol source-row source-column) 
						(vertex-symbol dest-row dest-column) :edge-type :directed :weight (data-elt dest-row dest-column)))))
	  (let ((source (make-instance 'heapable-node :name "source"))
		(drain (make-instance 'heapable-node :name "drain")))	    
	    (add-vertex g source)
	    (add-vertex g drain)
	    (add-edge-between-vertexes g source (vertex-symbol 0 0) :edge-type :directed :weight (data-elt 0 0))
	    (add-edge-between-vertexes g (vertex-symbol (1- number-rows) (1- number-columns)) drain :edge-type :directed :weight 0)
	    (loop for row from 0 below number-rows do
		 (loop for column from 0 below number-columns do
		      (add-edge-if-possible row (1- column) row column)
		      (add-edge-if-possible (1- row) column row column)
		      (add-edge-if-possible (1+ row) column row column)
		      (add-edge-if-possible row (1+ column) row column)))))))
    g))

(defun problem-83 ()
  (let ((g (problem-83-graph)))
    (let ((previouses (shortest-path-dijkstra g (find-vertex-if g (vertex-name-is-p "source")))))
      (sum-weights g (find-vertex-if g (vertex-name-is-p "source")) (find-vertex-if g (vertex-name-is-p "drain")) previouses))))
		   