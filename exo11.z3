;(s = 4, d = 3, c = 3) 
; L’ensemble des sommets du graphe est S={s1,s2,s3,s4}
(declare-datatypes () ((S s1 s2 s3 s4))) 

; Déclaration de la fonction A qui détermine si une arête existe entre deux sommets
(declare-fun A (S S) Bool)

; Vérifie qu'il n'y a pas d'arête bouclée
(assert (forall ((x S)) (not (A x x))))

; Vérifie que le graphe est non orienté
(assert (forall ((x S) (y S)) (= (A x y) (A y x))))

; Vérifie que chaque sommet a au moins 3 voisins
(assert (forall ((x S)) 
  (exists ((y1 S) (y2 S) (y3 S)) 
    (and (A x y1) (A x y2) (A x y3) 
      (distinct y1 y2 y3)))))

; Vérifie qu'il n'y a pas de clique de taille 3
(assert (forall ((x S) (y S) (z S)) 
  (not (and (A x y) (A y z) (A z x) (distinct x y z)))))

(check-sat)
(get-model)
