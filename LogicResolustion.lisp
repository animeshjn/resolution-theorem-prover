;Animesh Jain
(defun resolution(lc) ;lc is list of all clauses to be considered

;if empty list then no contradiction
(cond ((null lc)       (print "No contradiction (Empty clauses)") (return-from resolution t)))

(setf remainder lc) ;initialize remaining clauses to be considered to all

(loop for current_clause in lc ;remove first clause
  ;for each clause check for atoms in LHS and RHS
  do (cond ((and (atom current_clause) (not (equal current_clause nil)))        (print "Invalid input, enter clausal form") ) )

  (remove current_clause remainder)
  ; for each next clause consider finding all atoms of LHS current_clause in RHS
  ;if left  = right remove one side



  ;if remainder is nil or nil nil then contradiction
  ; for each atom in LHS of this clause
  (loop for atomic_value_left in (first current_clause) ;LHS
  do
  (loop for next_clause in remainder
    do
    ; pick next clause and remove from remainder

    (setf remainder (remove next_clause remainder))
    ;use option R checks RHS of next_clause
    (setf remainder (resolve_each next_clause remainder atomic_value_left 'R))
    ;returns modified clause
    )

    )
    (loop for atomic_value_right in (first (rest current_clause)) ;RHS
    do
    (loop for next_clause in remainder
      do
      (setf remainder (remove next_clause remainder))
      ;use option L to check LHS
      (setf remainder (resolve_each next_clause remainder atomic_value_right 'L))
      ;returns modified clause
      )

      )
      ;get LHS to compare with all
      (setf current_left (first current_clause))
      (setf current_right (first (rest current_clause))))
      ; (print remainder) ; to print the current resolution

      ;condition to check for contradiction or resolution
      (cond ((null remainder)         (print "A Contradiction has been found")  NIL)
      ((and(null (first remainder)) (equal (rest remainder) (list NIL)))          (print "A Contradiction has been found")  NIL)
      (t          (print "No contradiction has been found") t)
      ))


      ;resolve each function to resolve single clause with remaining list
      (defun resolve_each(current_clause remaining current_atom option)
      ;parameters: current_clause(clause to be considered)
      ;remaining : list with all the resolutions
      ;current_atom: atom to look for
      ;option: R to check RHS L to check LHS of given clause

      ;option L Left Side
      (if (equal option 'L)
      (progn
        ;remove duplicates from this side first
        (setf LHS (remove-duplicates (first current_clause)))

        (loop for atom_value in LHS
          do (if (equal atom_value current_atom)
          ;remove the atom if match found
          (progn
            (setf current_clause (append (list (remove atom_value (first current_clause))) (rest current_clause)))

            ))))
            ;else option R : Right Side

            (progn
              ;remove duplicates from this side first
              (setf RHS (remove-duplicates (first (rest current_clause))))
              ;remove the atom if match found
              (loop for atom_value in RHS
                do (if (equal atom_value current_atom)
                (progn
                  (setf current_clause (append (list(first current_clause)) (list (remove atom_value (first (rest current_clause)))) ))
                  ))
                  )))
                  ;condition to avoid appending NIL
                  (cond ((and(null (first current_clause)) (equal (rest current_clause) (list NIL)))        remaining)
                  (t          (append (list current_clause) remaining))
                  ) ;return the remaining list with resolvent
                  ) ; end of resolve_each

                  ;testing session
                  (defun test-resolution()
                  (print(resolution '( ((A)(B C)) ((B)()) ((C)()))))
                  (print(resolution '( ((A)(B C)) ((B)()) (()(A)) ((C)()) )))
                  (print(resolution '( ((C D)(A)) ((A D E)()) (()(A C)) (()(D)) (()(E)) )))
                  (print(resolution '(((A B)()))))
                  (print(resolution '( ((A B)()) ((B)()) ( (C)() ) ) ))
                  (print(resolution '( ((A B)()) ((B C)()) ( ()(A B C) ) ) ))
                  (print(resolution '( ((C D)()) ((B)())  ( ()(B C D) ) ) ))
                  (print(resolution '( (()(X Y)) ((B)())  ) ))
                  (print(resolution '() )) ;special case
                  (print(resolution '( ((A)(B)) ((B)(A))   ) ))
                  (print(resolution '(((X Y)()) (()(Y)) (() (X)) ) ))
                  (print(resolution '(((X Z)()) ((Y)()) (() (X)) (() (Y Z))) ))
                  t)

                  (test-resolution)
