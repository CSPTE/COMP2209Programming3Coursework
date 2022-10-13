import Challenges

--Challenge1
--challenge1Test0 = calcBBInteractions 3 [] == [((North,1),Path (South,1)),((North,2),Path (South,2)),((North,3),Path (South,3)),((East,1),Path (West,1)),((East,2),Path (West,2)),((East,3),Path (West,3)),((South,1),Path (North,1)),((South,2),Path (North,2)),((South,3),Path (North,3)),((West,1),Path (East,1)),((West,2),Path (East,2)),((West,3),Path (East,3))]
challenge1Test1 = calcBBInteractions 8 [(2,3),(7,3),(4,6),(7,8)] == [((North,1),Path (West,2)),((North,2),Absorb), ((North, 3), Path (North, 6)),((North,4),Absorb),((North,5),Path (East,5)),((North,6),Path (North,3)),((North,7),Absorb),((North,8),Path (East,2)), ((East,1),Path (West,1)),((East,2),Path (North,8)),((East,3),Absorb),((East,4),Path (East,7)),((East,5),Path (North,5)),((East,6),Absorb),((East,7),Path (East,4)),((East,8),Absorb),((South,1),Path (West,4)),((South,2),Absorb),((South,3),Path (West,7)),((South,4),Absorb),((South,5),Path (West,5)),((South,6),Reflect),((South,7),Absorb),((South,8),Reflect),((West,1),Path (East,1)),((West,2),Path (North,1)),((West,3),Absorb),((West,4),Path (South,1)),((West,5),Path (South,5)),((West,6),Absorb),((West,7),Path (South,3)),((West,8),Absorb)]

--Challenge2

--Challenge3
challenge3Test1 = prettyPrint (LamApp (LamAbs 1 (LamVar 1)) (LamAbs 1 (LamVar 1))) == "( \\x1 -> x1 ) \\x1 -> x1"
challenge3Test2 = prettyPrint (LamAbs 1 (LamApp (LamVar 1) (LamAbs 1 (LamVar 1)))) == "\\x1 -> x1 \\x1 -> x1"
challenge3Test3 = prettyPrint (LamApp (LamVar 2) (LamAbs 1 (LamAbs 2 (LamVar 1)))) == "x2 0"
challenge3Test4 = prettyPrint (LamAbs 1 (LamAbs 1 (LamApp (LamVar 1) (LamAbs 1 (LamAbs 2 (LamVar 1)))))) == "1"

--Challenge4

--Challenge5

--Challenge6