{-# LANGUAGE StandaloneDeriving, DataKinds, TypeOperators, DeriveFunctor, DeriveGeneric, FlexibleContexts, UndecidableInstances, FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, TypeFamilies #-}
module Abstract.Syntax where

import Abstract.Primitive
import Abstract.Set
import Abstract.Term
import Data.Bifoldable
import Data.Bifunctor
import Data.Bitraversable
import Data.Foldable (fold)
import Data.Functor.Classes
import Data.Functor.Foldable hiding (fold)
import Data.List (intersperse)
import Data.Pointed
import Data.Text.Prettyprint.Doc
import Data.Union
import GHC.Generics
import Data.Functor.Classes.Show.Generic

type Syntax = Union '[Variable, Primitive, Lambda, Application]

newtype Term a = In { out :: Syntax (Term a) }
  deriving (Eq, Ord, Show)


newtype Variable a = Variable String deriving (Eq, Ord, Show, Functor, Generic1)
instance Show1 Variable where liftShowsPrec = genericLiftShowsPrec
instance Eq1 Variable where liftEq _ (Variable name1) (Variable name2) = name1 == name2
instance Ord1 Variable where liftCompare _ (Variable name1) (Variable name2) = compare name1 name2

newtype Primitive a = Primitive Prim deriving (Eq, Ord, Show, Functor, Generic1)
instance Show1 Primitive where liftShowsPrec = genericLiftShowsPrec
instance Eq1 Primitive where liftEq _ (Primitive a1) (Primitive a2) = a1 == a2
instance Ord1 Primitive where liftCompare _ (Primitive a1) (Primitive a2) = compare a1 a2

data Lambda a = Lambda Name a deriving (Eq, Ord, Show, Functor, Generic1)
instance Show1 Lambda where liftShowsPrec = genericLiftShowsPrec
instance Eq1 Lambda where liftEq comp (Lambda name1 body1) (Lambda name2 body2) = name1 == name2 && comp body1 body2
instance Ord1 Lambda where liftCompare comp (Lambda name1 body1) (Lambda name2 body2) = compare name1 name2 `mappend` comp body1 body2

data Application a = Application a a deriving (Eq, Ord, Show, Functor, Generic1)
instance Show1 Application where liftShowsPrec = genericLiftShowsPrec
instance Eq1 Application where liftEq comp (Application a1 b1) (Application a2 b2) = comp a1 a2 && comp b1 b2
instance Ord1 Application where liftCompare comp (Application a1 b1) (Application a2 b2) = comp a1 a2 `mappend` comp b1 b2


-- Smart constructors for various Terms.
var :: Name -> Term a
var = In . inj . Variable

prim :: Prim -> Term a
prim = In . inj . Primitive

int :: Int -> Term Prim
int x = prim (PInt x)

true :: Term Prim
true = prim (PBool True)

false :: Term Prim
false = prim (PBool False)

infixl 9 #
(#) :: Term a -> Term a -> Term a
(#) a b = In (inj (Application a b))

lam :: Name -> (Term a -> Term a) -> Term a
lam s f = makeLam s (f (var s))

makeLam :: Name -> Term a -> Term a
makeLam name body = In (inj (Lambda name body))

-- Instances

type instance Base (Term a) = Syntax


-- data Syntax a r
--   = Other (Union '[Lambda, Application, Variable, Primitive] r)
  -- | Var Name
  -- | Prim a
  -- | Op1 Op1 r
  -- | Op2 Op2 r r
  -- | App r r
  -- | Lam Name r
  -- | Rec Name r
  -- | If r r r
  -- deriving (Eq, Ord, Show)

-- eq :: Term a -> Term a -> Term a
-- eq = (In .) . Op2 Eq
--
-- lt :: Term a -> Term a -> Term a
-- lt = (In .) . Op2 Lt
--
-- lte :: Term a -> Term a -> Term a
-- lte = (In .) . Op2 LtE
--
-- gt :: Term a -> Term a -> Term a
-- gt = (In .) . Op2 Gt
--
-- gte :: Term a -> Term a -> Term a
-- gte = (In .) . Op2 GtE
--
-- and' :: Term a -> Term a -> Term a
-- and' = (In .) . Op2 And
--
-- or' :: Term a -> Term a -> Term a
-- or' = (In .) . Op2 Or
--
-- not' :: Term a -> Term a
-- not' = In . Op1 Not
--
-- div' :: Term a -> Term a -> Term a
-- div' = (In .) . Op2 DividedBy
--
-- quot' :: Term a -> Term a -> Term a
-- quot' = (In .) . Op2 Quotient
--
-- rem' :: Term a -> Term a -> Term a
-- rem' = (In .) . Op2 Remainder
--
-- mod' :: Term a -> Term a -> Term a
-- mod' = (In .) . Op2 Modulus

-- mu :: Name -> (Term a -> Term a) -> Term a
-- mu f b = makeRec f (b (var f))

-- makeRec :: Name -> Term a -> Term a
-- makeRec name body = In (Rec name body)
--
-- if' :: Term a -> Term a -> Term a -> Term a
-- if' c t e = In (If c t e)

-- let' :: Name -> Term a -> (Term a -> Term a) -> Term a
-- let' var val body = lam var body # val

--
-- freeVariables :: Term a -> Set Name
-- freeVariables = cata (\ syntax -> case syntax of
--   Var n -> point n
--   Lam n body -> delete n body
--   -- Rec n body -> delete n body
--   _ -> fold syntax)
--
--
-- showsConstructor :: String -> Int -> [Int -> ShowS] -> ShowS
-- showsConstructor name d fields = showParen (d > 10) $ showString name . showChar ' ' . foldr (.) id (intersperse (showChar ' ') ([($ 11)] <*> fields))



-- instance Recursive (Term a) where
--   project = out
--
-- instance Corecursive (Term a) where
--   embed = In

-- instance Foldable Term where
--   foldMap f = go where go = bifoldMap f go . out

-- instance Bifoldable Syntax where
--   bifoldMap f g s = case s of
--     Var _ -> mempty
--     Prim i -> f i
--     -- Op1 _ a -> g a
--     -- Op2 _ a b -> g a `mappend` g b
--     App a b -> g a `mappend` g b
--     Lam _ a -> g a
--     -- Rec _ a -> g a
--     -- If c t e -> g c `mappend` g t `mappend` g e

-- instance Foldable (Syntax a) where
--   foldMap = bifoldMap (const mempty)


-- instance Functor Term where
--   fmap f = go where go = In . bimap f go . out

-- instance Bifunctor Syntax where
--   bimap f g s = case s of
--     Var n -> Var n
--     Prim v -> Prim (f v)
--     -- Op1 o a -> Op1 o (g a)
--     -- Op2 o a b -> Op2 o (g a) (g b)
--     App a b -> App (g a) (g b)
--     Lam n a -> Lam n (g a)
--     -- Rec n a -> Rec n (g a)
--     -- If c t e -> If (g c) (g t) (g e)

-- instance Functor (Syntax a) where
--   fmap = second
--
--
-- instance Traversable Term where
--   traverse f = go where go = fmap In . bitraverse f go . out

-- instance Bitraversable Syntax where
--   bitraverse f g s = case s of
--     Var n -> pure (Var n)
--     Prim v -> Prim <$> f v
--     -- Op1 o a -> Op1 o <$> g a
--     -- Op2 o a b -> Op2 o <$> g a <*> g b
--     App a b -> App <$> g a <*> g b
--     Lam n a -> Lam n <$> g a
--     -- Rec n a -> Rec n <$> g a
--     -- If c t e -> If <$> g c <*> g t <*> g e

-- instance Traversable (Syntax a) where
--   traverse = bitraverse pure
--
--
-- instance Eq1 Term where
--   liftEq eqA = go where go t1 t2 = liftEq2 eqA go (out t1) (out t2)

-- instance Eq2 Syntax where
--   liftEq2 eqV eqA s1 s2 = case (s1, s2) of
--     (Var n1, Var n2) -> n1 == n2
--     (Prim v1, Prim v2) -> eqV v1 v2
--     -- (Op1 o1 a1, Op1 o2 a2) -> o1 == o2 && eqA a1 a2
--     -- (Op2 o1 a1 b1, Op2 o2 a2 b2) -> o1 == o2 && eqA a1 a2 && eqA b1 b2
--     (App a1 b1, App a2 b2) -> eqA a1 a2 && eqA b1 b2
--     (Lam n1 a1, Lam n2 a2) -> n1 == n2 && eqA a1 a2
--     -- (Rec n1 a1, Rec n2 a2) -> n1 == n2 && eqA a1 a2
--     -- (If c1 t1 e1, If c2 t2 e2) -> eqA c1 c2 && eqA t1 t2 && eqA e1 e2
--     _ -> False

-- instance Eq a => Eq1 (Syntax a) where
--   liftEq = liftEq2 (==)
--
-- instance Ord1 Term where
--   liftCompare compareA = go where go t1 t2 = liftCompare2 compareA go (out t1) (out t2)

-- instance Ord2 Syntax where
--   liftCompare2 compareV compareA s1 s2 = case (s1, s2) of
--     (Var n1, Var n2) -> compare n1 n2
--     (Var{}, _) -> LT
--     (_, Var{}) -> GT
--     (Prim v1, Prim v2) -> compareV v1 v2
--     (Prim{}, _) -> LT
--     (_, Prim{}) -> GT
--     -- (Op1 o1 a1, Op1 o2 a2) -> compare o1 o2 <> compareA a1 a2
--     -- (Op1{}, _) -> LT
--     -- (_, Op1{}) -> GT
--     -- (Op2 o1 a1 b1, Op2 o2 a2 b2) -> compare o1 o2 <> compareA a1 a2 <> compareA b1 b2
--     -- (Op2{}, _) -> LT
--     -- (_, Op2{}) -> GT
--     (App a1 b1, App a2 b2) -> compareA a1 a2 <> compareA b1 b2
--     (App{}, _) -> LT
--     (_, App{}) -> GT
--     (Lam n1 a1, Lam n2 a2) -> compare n1 n2 <> compareA a1 a2
--     (Lam{}, _) -> LT
--     (_, Lam{}) -> GT
--     -- (Rec n1 a1, Rec n2 a2) -> compare n1 n2 <> compareA a1 a2
--     -- (Rec{}, _) -> LT
--     -- (_, Rec{}) -> GT
--     -- (If c1 t1 e1, If c2 t2 e2) -> compareA c1 c2 <> compareA t1 t2 <> compareA e1 e2

-- instance Ord a => Ord1 (Syntax a) where
--   liftCompare = liftCompare2 compare


-- instance Show1 Term where
--   liftShowsPrec spA slA = go where go d t = showsUnaryWith (liftShowsPrec2 spA slA go (liftShowList spA slA)) "In" d (out t)
--
-- instance Show a => Show (Term a) where
--   showsPrec = showsPrec1
--
-- instance Show2 Syntax where
--   liftShowsPrec2 spV _ spA _ d s = case s of
--     Var n -> showsConstructor "Var" d [ flip showsPrec n ]
--     Prim v -> showsConstructor "Prim" d [ flip spV v ]
--     -- Op1 o a -> showsConstructor "Op1" d [ flip showsPrec o, flip spA a ]
--     -- Op2 o a b -> showsConstructor "Op2" d [ flip showsPrec o, flip spA a, flip spA b ]
--     App a b -> showsConstructor "App" d (flip spA <$> [ a, b ])
--     Lam n a -> showsConstructor "Lam" d [ flip showsPrec n, flip spA a ]
--     -- Rec n a -> showsConstructor "Rec" d [ flip showsPrec n, flip spA a ]
--     -- If c t e -> showsConstructor "If" d (flip spA <$> [c, t, e])
--
-- instance Show a => Show1 (Syntax a) where
--   liftShowsPrec = liftShowsPrec2 showsPrec showList
--
--
-- instance Num a => Num (Term a) where
--   fromInteger = prim . fromInteger
--
--   -- signum = In . Op1 Signum
--   -- abs    = In . Op1 Abs
--   -- negate = In . Op1 Negate
--   -- (+) = (In .) . Op2 Plus
--   -- (-) = (In .) . Op2 Minus
--   -- (*) = (In .) . Op2 Times
--
--
-- instance Pretty1 Term where
--   liftPretty p pl = go where go = liftPretty2 p pl go (list . map (liftPretty p pl)) . out
--
-- instance Pretty a => Pretty (Term a) where
--   pretty = liftPretty pretty prettyList
--
-- instance Pretty2 Syntax where
--   liftPretty2 pv _ pr _ s = case s of
--     Var n -> pretty n
--     Prim v -> pv v
--     -- Op1 o a -> pretty o <+> pr a
--     -- Op2 o a b -> pr a <+> pretty o <+> pr b
--     App a b -> pr a <+> parens (pr b)
--     Lam n a -> parens (pretty '\\' <+> pretty n <+> pretty "." <> nest 2 (line <> pr a))
--     -- Rec n a -> pretty "mu" <+> parens (pretty '\\' <+> pretty n <+> pretty "." <> nest 2 (line <> pr a))
--     -- If c t e -> pretty "if" <+> pr c <+> pretty "then" <> nest 2 (line <> pr t) <> line <> pretty "else" <> nest 2 (line <> pr e)
--
-- instance Pretty a => Pretty1 (Syntax a) where
--   liftPretty = liftPretty2 pretty prettyList
