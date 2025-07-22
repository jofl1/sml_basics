datatype ('a, 'b) tree = 
    nil
    | Node of ('a * 'b) * ('a, 'b) tree * ('a, 'b) tree;

 val jofltree = 
 	Node((1, "a"),
 		Node((2, "b"), 
 			Node((3, "c"), nil, nil),
 			nil),
 		Node((4, "d"),
 			nil,
 			Node((5, "e"), 
 				Node((6, "f"), nil, nil),
 				nil)));

 fun findkey1 (jofltree, a) = 
 	case jofltree of
 		Node((key, value) , left, right) =>
 			if a = key then value
 			else (findkey1(left, a)
 				handle Match => findkey1(right,a));

 fun findkey2 (Node((key, value) , left, right), a) = 
 		if a = key then value
 			else (findkey2(left, a)
 				handle Match => findkey2(right,a))
  |  findkey2 (nil, a) = 0;
        
        