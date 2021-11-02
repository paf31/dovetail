Prelude.Debug.show
  { map: Prelude.Array.map (Prelude.Int.mul 2) [1, 2, 3]
  , filter: Prelude.Array.filter (_ `Prelude.Int.lte` 2) [1, 2, 3]
  , foldl: Prelude.Array.foldl Prelude.Int.add 0 [1, 2, 3]
  , zipWith: Prelude.Array.zipWith (\x y -> { x, y }) [1, 2, 3] [1, 2, 3]
  , append: Prelude.Array.append [1, 2, 3] [4, 5, 6]
  }