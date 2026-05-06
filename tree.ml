exception NotFound;

datatype ('a, 'b) tree =
    Empty
  | Node of ('a * 'b) * ('a, 'b) tree * ('a, 'b) tree;

val jofltree =
    Node ((1, "a"),
        Node ((2, "b"),
            Node ((3, "c"), Empty, Empty),
            Empty),
        Node ((4, "d"),
            Empty,
            Node ((5, "e"),
                Node ((6, "f"), Empty, Empty),
                Empty)));

(* case-style: search left, fall through to right on NotFound *)
fun findkey1 (t, a) =
    case t of
        Empty => raise NotFound
      | Node ((key, value), left, right) =>
            if a = key then value
            else (findkey1 (left, a)
                  handle NotFound => findkey1 (right, a));

(* clause-style: same behaviour, written with multiple clauses *)
fun findkey2 (Empty, _) = raise NotFound
  | findkey2 (Node ((key, value), left, right), a) =
        if a = key then value
        else (findkey2 (left, a)
              handle NotFound => findkey2 (right, a));
