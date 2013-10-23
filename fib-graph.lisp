(let ((data (with-open-file (s "problem-25-graph-data.lisp") (read s))))
  (cllib:plot-lists (list data)))