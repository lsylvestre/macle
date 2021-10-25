* création de fermetures, notamment en cas d'HOF récursives. Par exemple : 

  ```
  let rec f(g) =
    let h(x) = g(x) + g(x) in
    f(h)
  ```
* ajout d'une pile d'appel spécifique à chaque fonction récursive.

* ajout du polymorphisme (l'inlineur opère déjà une monomorphisation
   mais les annotations de types ne sont pas mises à jour).