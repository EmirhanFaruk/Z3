open Printf

(* Fonction qui demande les parametres, sommets, degré minimal et taille maximale de clique *)
let askParameter () = 
  print_endline "Entrez le nombre de sommets (s):";
  let s = read_int () in
  print_endline "Entrez la valeur de degré minimal (d):";
  let d = read_int () in
  print_endline "Entrez la valeur de la taille maximale de clique (c):";
  let c = read_int () in
  (s, d, c)

(* Fonction qui déclare les sommets *)
let declareASummit oc s = 
  fprintf oc "(declare-datatypes () ((S ";
  for i = 1 to s do
    fprintf oc "s%d " i
  done;
  fprintf oc ")))\n\n"

(* Fonction qui vérifie les contraintes *)
let checkConstraint oc s d c =
  (* Vérification qu'il n'y a pas d'aretes reliant un sommet à lui meme *)
  fprintf oc "(assert (forall ((x S)) (not (A x x))))\n\n";

  (* Vérification que le graphe est non orienté *)
  fprintf oc "(assert (forall ((x S) (y S)) (= (A x y) (A y x))))\n\n";

  (* Vérification que chaque sommet a au moins d voisins *)
  fprintf oc "(assert (forall ((x S))\n  (exists (";
  for i = 1 to d do
    fprintf oc "(y%d S) " i
  done;
  fprintf oc ")\n    (and ";
  for i = 1 to d do
    fprintf oc "(A x y%d) " i
  done;
  for i = 1 to d do
    for j = i + 1 to d do
      fprintf oc "(not (= y%d y%d)) " i j
    done
  done;
  fprintf oc "))))\n\n";

  (* Vérification qu'il n'y a pas de clique de taille c *)
  fprintf oc "(assert (forall (";
  for i = 1 to c do
    fprintf oc "(x%d S) " i
  done;
  fprintf oc ")\n  (not (and ";
  for i = 1 to c do
    for j = i + 1 to c do
      fprintf oc "(A x%d x%d) " i j
    done
  done;
  fprintf oc "(distinct ";
  for i = 1 to c do
    fprintf oc "x%d " i
  done;
  fprintf oc ")))))\n\n"

(* Fonction principale qui génère un fichier SMT-LIB *)
let generate_instance () =
  (* Ajout des parametres *)
  let s, d, c = askParameter () in
  (* Création du fichier output.z3 *)
  let oc = open_out "output.z3" in

  (* Déclaration des sommets *)
  declareASummit oc s;

  (* Déclaration de la fonction A pour les aretes *)
  fprintf oc "(declare-fun A (S S) Bool)\n\n";

  (* Verification des contraintes *)
  checkConstraint oc s d c;

  (* Verification et modele *)
  fprintf oc "(check-sat)\n(get-model)\n";

  (* Fermeture du fichier *)
  close_out oc;
  print_endline "Le fichier output.z3 a été généré."

(* Execution principale *)
let () = generate_instance ()
