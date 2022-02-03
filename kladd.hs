
data HuffmanTree = Leaf Char Int | Node Int (HuffmanTree) (HuffmanTree)

ex_tree =
  Node 36
    (Node 16
      (Node 8
        (Leaf 'e' 4)
        (Node 4
          (Leaf 'n' 2)
          (Node 2
            (Leaf 'o' 1)
            (Leaf 'u' 1))))
      (Leaf 'a' 4)
      (Node 4
        (Leaf 't' 2)
        (Leaf 'm' 2))
    (Node 20
      (Node 8
        (Node 4
          (Leaf 'i' 2
          (Node 2
            (Leaf 'x' 1)
            (Leaf 'p' 1))))))
        (Node 4
        (Leaf 'h' 2)
        (Leaf 's' 2))
      (Node 12
        (Node 5
          (Node 2
            (Leaf 'r' 1)
            (Leaf 'l' 1))))
          (Leaf 'f' 3)
        (Leaf ' ' 7))




all_leaves :: HuffmanTree -> [(Char, Int)]
all_leaves (Leaf chr freq) = [(chr, freq)]
all_leaves (Node a l r) = all_leaves l ++ all_leaves r

-- BÖrja om i träd efter löf och lägg in i used_leaf sedan om char finns i used_leaf
-- ta andra

pathCode :: HuffmanTree -> [Int] -- du får inte fullständiga paths på det ena lövet i ett slut
pathCode (Leaf c n) = [3]
-- pathCode (Node a (Leaf c1 n1) (Leaf c2 n2)) = pathCode
pathCode (Node a l r) = (0 : pathCode l) ++ (1 : pathCode r)
