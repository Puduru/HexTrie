-- Hexadecimal Trie
module HexTrie where

import Data.Bits (shiftR, (.&.))

data HexTrie a = Null | Leaf !a | Node {
                                    t0 :: HexTrie a, t1 :: HexTrie a, t2 :: HexTrie a, t3 :: HexTrie a,
                                    t4 :: HexTrie a, t5 :: HexTrie a, t6 :: HexTrie a, t7 :: HexTrie a,
                                    t8 :: HexTrie a, t9 :: HexTrie a, ta :: HexTrie a, tb :: HexTrie a,
                                    tc :: HexTrie a, td :: HexTrie a, te :: HexTrie a, tf :: HexTrie a
                                    }
  deriving Show

nullNode = Node Null Null Null Null Null Null Null Null Null Null Null Null Null Null Null Null

setLeaf :: HexTrie a -> Int -> a -> HexTrie a
setLeaf (Leaf _) 0 value = Leaf value
setLeaf (Leaf v) index value = (setLeaf $!(setLeaf Null index value)) 0 v
setLeaf Null 0 value = Leaf value
setLeaf Null index value = setLeaf nullNode index value
setLeaf trie index0 value = let index1 = shiftR index0 4
                                key = index0 .&. 15
                            in case key of
                                 00 -> trie {t0 = ((setLeaf $! t0 trie) $! index1) value}
                                 01 -> trie {t1 = ((setLeaf $! t1 trie) $! index1) value}
                                 02 -> trie {t2 = ((setLeaf $! t2 trie) $! index1) value}
                                 03 -> trie {t3 = ((setLeaf $! t3 trie) $! index1) value}
                                 04 -> trie {t4 = ((setLeaf $! t4 trie) $! index1) value}
                                 05 -> trie {t5 = ((setLeaf $! t5 trie) $! index1) value}
                                 06 -> trie {t6 = ((setLeaf $! t6 trie) $! index1) value}
                                 07 -> trie {t7 = ((setLeaf $! t7 trie) $! index1) value}
                                 08 -> trie {t8 = ((setLeaf $! t8 trie) $! index1) value}
                                 09 -> trie {t9 = ((setLeaf $! t9 trie) $! index1) value}
                                 10 -> trie {ta = ((setLeaf $! ta trie) $! index1) value}
                                 11 -> trie {tb = ((setLeaf $! tb trie) $! index1) value}
                                 12 -> trie {tc = ((setLeaf $! tc trie) $! index1) value}
                                 13 -> trie {td = ((setLeaf $! td trie) $! index1) value}
                                 14 -> trie {te = ((setLeaf $! te trie) $! index1) value}
                                 15 -> trie {tf = ((setLeaf $! tf trie) $! index1) value}

unsetLeaf :: HexTrie a -> Int -> HexTrie a
unsetLeaf (Leaf _) 0 = Null
unsetLeaf leaf@(Leaf _) index = leaf
unsetLeaf Null _ = Null
unsetLeaf trie index0
  | reduceNode trie1 = t0 trie
  | otherwise = trie1
  where index1 = shiftR index0 4
        key = index0 .&. 15
        trie1 = case key of
                  00 -> trie { t0 = (unsetLeaf $! t0 trie) $! index1 }
                  01 -> trie { t1 = (unsetLeaf $! t1 trie) $! index1 }
                  02 -> trie { t2 = (unsetLeaf $! t2 trie) $! index1 }
                  03 -> trie { t3 = (unsetLeaf $! t3 trie) $! index1 }
                  04 -> trie { t4 = (unsetLeaf $! t4 trie) $! index1 }
                  05 -> trie { t5 = (unsetLeaf $! t5 trie) $! index1 }
                  06 -> trie { t6 = (unsetLeaf $! t6 trie) $! index1 }
                  07 -> trie { t7 = (unsetLeaf $! t7 trie) $! index1 }
                  08 -> trie { t8 = (unsetLeaf $! t8 trie) $! index1 }
                  09 -> trie { t9 = (unsetLeaf $! t9 trie) $! index1 }
                  10 -> trie { ta = (unsetLeaf $! ta trie) $! index1 }
                  11 -> trie { tb = (unsetLeaf $! tb trie) $! index1 }
                  12 -> trie { tc = (unsetLeaf $! tc trie) $! index1 }
                  13 -> trie { td = (unsetLeaf $! td trie) $! index1 }
                  14 -> trie { te = (unsetLeaf $! te trie) $! index1 }
                  15 -> trie { tf = (unsetLeaf $! tf trie) $! index1 }
        reduceNode (Node Null Null Null Null Null Null Null Null Null Null Null Null Null Null Null Null) = True
        reduceNode (Node (Leaf _) Null Null Null Null Null Null Null Null Null Null Null Null Null Null Null) = True
        reduceNode _ = False

getLeaf :: HexTrie a -> Int -> Maybe a
getLeaf Null _ = Nothing
getLeaf (Leaf v) 0 = Just v
getLeaf (Leaf _) _ = Nothing
getLeaf trie index0 = let index1 = shiftR index0 4
                          key = index0 .&. 15
                      in case key of
                           00 -> (getLeaf $! t0 trie) $! index1
                           01 -> (getLeaf $! t1 trie) $! index1
                           02 -> (getLeaf $! t2 trie) $! index1
                           03 -> (getLeaf $! t3 trie) $! index1
                           04 -> (getLeaf $! t4 trie) $! index1
                           05 -> (getLeaf $! t5 trie) $! index1
                           06 -> (getLeaf $! t6 trie) $! index1
                           07 -> (getLeaf $! t7 trie) $! index1
                           08 -> (getLeaf $! t8 trie) $! index1
                           09 -> (getLeaf $! t9 trie) $! index1
                           10 -> (getLeaf $! ta trie) $! index1
                           11 -> (getLeaf $! tb trie) $! index1
                           12 -> (getLeaf $! tc trie) $! index1
                           13 -> (getLeaf $! td trie) $! index1
                           14 -> (getLeaf $! te trie) $! index1
                           15 -> (getLeaf $! tf trie) $! index1
