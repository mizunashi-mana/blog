{-# LANGUAGE BangPatterns       #-}
{-# LANGUAGE BlockArguments     #-}
{-# LANGUAGE DeriveFunctor      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE MultiWayIf         #-}

module Problem02 where


-- Header

data Inst a
  = GetChild Int (Inst a)
  | Put Char [Inst a]
  | Var a
  | Bottom
  deriving (Eq, Show, Functor)

data HoledInst a
  = HGetChild Int
  | HPut Char [Inst a] [Inst a]
  deriving (Eq, Show, Functor)

substHoledInst :: Inst a -> HoledInst a -> Inst a
substHoledInst t ht = case ht of
    HGetChild i  -> GetChild i t
    HPut c lt rt -> Put c $ go lt $ t:rt
  where
    go []     !ys = ys
    go (x:xs) !ys = go xs (x:ys)

data InstZipper a = InstZipper
  { zipperCrumbs :: [HoledInst a]
  , zipperTree   :: Inst a
  }
  deriving (Eq, Show, Functor)

toZipper :: Inst a -> InstZipper a
toZipper t = InstZipper [] t

setInstZipper :: Inst a -> InstZipper a -> InstZipper a
setInstZipper t (InstZipper cs _) = InstZipper cs t

upInst :: InstZipper a -> Maybe (InstZipper a)
upInst (InstZipper cs t) = case cs of
  []     -> Nothing
  ht:cs' -> Just $ InstZipper cs' $ substHoledInst t ht

leftInst :: InstZipper a -> Maybe (InstZipper a)
leftInst (InstZipper cs t) = case cs of
  []     -> Nothing
  ht:cs' -> case ht of
    HGetChild{}  -> Nothing
    HPut c lt rt -> case lt of
      []     -> Nothing
      t':lt' -> Just $ InstZipper (HPut c lt' (t:rt):cs') t'

rightInst :: InstZipper a -> Maybe (InstZipper a)
rightInst (InstZipper cs t) = case cs of
  []     -> Nothing
  ht:cs' -> case ht of
    HGetChild{}  -> Nothing
    HPut c lt rt -> case rt of
      []     -> Nothing
      t':rt' -> Just $ InstZipper (HPut c (t:lt) rt':cs') t'

downInst :: Int -> InstZipper a -> Maybe (InstZipper a)
downInst n _ | n < 0         = error "expect non-negative number"
downInst n (InstZipper cs t) = case t of
    Var{}         -> Nothing
    Bottom        -> Nothing
    GetChild i t'
      | n == 0    -> Just $ InstZipper (HGetChild i:cs) t'
      | otherwise -> Nothing
    Put c ts'     -> go c n [] ts'
  where
    go _ _ _   []      = Nothing
    go c 0 !ls (t':rs) = Just $ InstZipper (HPut c ls rs:cs) t'
    go c m !ls (t':rs) = go c (m - 1) (t':ls) rs


-- Main

fromZipper :: InstZipper a -> Inst a
fromZipper = go
  where
    go z0 = case upInst z0 of
      Nothing -> zipperTree z0
      Just z1 -> go z1

evalInst :: Inst a -> Inst a
evalInst = fromZipper . go . toZipper
  where
    go z0 =
      let z1 = goWeak z0
      in case zipperTree z1 of
        Put{} -> case downInst 0 z1 of
          Nothing -> z1
          Just z2 -> goPut z2
        _ -> z1

    goWeak z0 = case zipperTree z0 of
      GetChild i _ -> goGet i z0
      _            -> z0

    goPut z0 =
      let z1 = go z0
      in case rightInst z1 of
        Just z2 -> goPut z2
        Nothing -> z1

    goGet i z0 =
      let
        z1 = goWeak case downInst 0 z0 of
          Nothing -> error "unreachable"
          Just z  -> z
        z2 = case upInst z1 of
          Nothing -> error "unreachable"
          Just z  -> z
      in case zipperTree z1 of
        Put _ ts -> case getChild i ts of
          Nothing -> setInstZipper Bottom z2
          Just t  -> setInstZipper t z2
        Bottom   -> setInstZipper Bottom z1
        _        -> z2

    getChild i _ | i < 0 = Nothing
    getChild _ []        = Nothing
    getChild 0 (t:_)     = Just t
    getChild n (_:ts)    = getChild (n - 1) ts


-- Sample


-- |
--
-- >>> evalInst sampleInst1
-- Put 'b' [Var "y"]
--
sampleInst1 :: Inst String
sampleInst1
  = GetChild 1
  $ Put 'a'
    [ GetChild 0 $ Var "x"
    , Put 'b'
      [ GetChild 2
      $ GetChild 1
      $ Put 'a'
        [ Var "y"
        , Put 'c'
          [ Var "z"
          , Put 'b'
            [ Put 'c' []
            ]
          , Var "y"
          ]
        , Var "x"
        ]
      ]
    ]


-- |
--
-- >>> evalInst sampleInst2
-- Bottom
--
sampleInst2 :: Inst a
sampleInst2 = GetChild 1 $ GetChild 0 $ GetChild 10 $ Put 'c' []

-- |
--
-- >>> evalInst sampleInst3
-- GetChild 0 (GetChild 1 (GetChild 2 (Var ())))
--
sampleInst3 :: Inst ()
sampleInst3 = GetChild 0 $ GetChild 1 $ GetChild 2 $ Var ()

-- |
--
-- >>> evalInst sampleInst4
-- Put 'b' [GetChild 1 (GetChild 2 (Var ())),Bottom,Put 'a' [Bottom]]
--
sampleInst4 :: Inst ()
sampleInst4
  = GetChild 1
  $ Put 'a'
    [ GetChild 0 $ Var ()
    , Put 'b'
      [ GetChild 1
      $ GetChild 2
      $ GetChild 2
      $ GetChild 1
      $ Put 'a'
        [ Var ()
        , Put 'c'
          [ Var ()
          , Put 'b'
            [ Put 'c' []
            ]
          , Var ()
          ]
        , Var ()
        ]
      , GetChild 0
      $ GetChild 1
      $ GetChild 1
      $ GetChild 1
      $ Put 'a'
        [ Var ()
        , Put 'c'
          [ Var ()
          , Put 'b'
            [ Put 'c' []
            ]
          , Var ()
          ]
        , Var ()
        ]
      , Put 'a'
        [ GetChild 0
        $ Put 'b' []
        ]
      ]
    ]
