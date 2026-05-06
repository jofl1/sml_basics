exception NotFound;

(* A binary search tree keyed by int. Polymorphic over the value type 'a.
   Invariant: for every Node ((k, _), L, R), every key in L is < k and every
   key in R is > k. *)
datatype 'a bst =
    Empty
  | Node of (int * 'a) * 'a bst * 'a bst;

(* insert (t, k, v): return a new tree with k mapped to v.
   If k is already present, its value is replaced. *)
fun insert (Empty, k, v) = Node ((k, v), Empty, Empty)
  | insert (Node ((key, value), left, right), k, v) =
        if k = key then Node ((k, v), left, right)
        else if k < key then Node ((key, value), insert (left, k, v), right)
        else Node ((key, value), left, insert (right, k, v));

(* case-style *)
fun findkey1 (t, a) =
    case t of
        Empty => raise NotFound
      | Node ((key, value), left, right) =>
            if a = key then value
            else if a < key then findkey1 (left, a)
            else findkey1 (right, a);

(* clause-style: same behaviour *)
fun findkey2 (Empty, _) = raise NotFound
  | findkey2 (Node ((key, value), left, right), a) =
        if a = key then value
        else if a < key then findkey2 (left, a)
        else findkey2 (right, a);

(* Same key/value pairs as jofltree, built up by insertion.
   Insertion order chosen so the resulting tree is reasonably balanced. *)
val joflbst =
    let
      val t = Empty
      val t = insert (t, 4, "d")
      val t = insert (t, 2, "b")
      val t = insert (t, 1, "a")
      val t = insert (t, 3, "c")
      val t = insert (t, 5, "e")
      val t = insert (t, 6, "f")
    in
      t
    end;
