{-# LANGUAGE FlexibleInstances, GeneralizedNewtypeDeriving, TypeFamilies #-}
module Abstract.Syntax where

import Abstract.Primitive
import Data.Bifoldable
import Data.Bifunctor
import Data.Bitraversable
import Data.Functor.Classes
import Data.Functor.Classes.Pretty
import Data.Functor.Foldable
import qualified Data.Set as Set

data Syntax a r
  = Var Name
  | Prim a
  | Op1 Op1 r
  | Op2 Op2 r r
  | App r r
  | Lam Name r
  | Rec Name r
  | If r r r
  deriving (Eq, Ord, Show)

type Name = String

newtype Term a = In { out :: Syntax a (Term a) }
  deriving (Eq, Ord, Show)


var :: Name -> Term a
var = In . Var

prim :: a -> Term a
prim = In . Prim

infixl 9 #
(#) :: Term a -> Term a -> Term a
(#) = (In .) . App

lam :: Name -> (Term a -> Term a) -> Term a
lam s f = makeLam s (f (var s))

makeLam :: Name -> Term a -> Term a
makeLam = (In .) . Lam

rec :: Name -> Name -> (Term a -> Term a -> Term a) -> Term a
rec f x b = makeRec f (lam x (b (var "f")))

makeRec :: Name -> Term a -> Term a
makeRec = (In .) . Rec

if' :: Term a -> Term a -> Term a -> Term a
if' c t e = In (If c t e)

let' :: Name -> Term a -> (Term a -> Term a) -> Term a
let' var val body = lam var body # val

immediateSubterms :: Ord a => Term a -> Set.Set (Term a)
immediateSubterms = foldMap Set.singleton . out

subterms :: Ord a => Term a -> Set.Set (Term a)
subterms term = para (foldMap (uncurry ((<>) . Set.singleton))) term <> Set.singleton term


showsTernaryWith :: (Int -> a -> ShowS) -> (Int -> b -> ShowS) -> (Int -> c -> ShowS) -> String -> Int -> a -> b -> c -> ShowS
showsTernaryWith sp1 sp2 sp3 name d x y z = showParen (d > 10) $ showString name . showChar ' ' . sp1 11 x . showChar ' ' . sp2 11 y . showChar ' ' . sp3 11 z


-- Instances

type instance Base (Term a) = Syntax a

instance Recursive (Term a) where
  project = out

instance Corecursive (Term a) where
  embed = In

instance Foldable Term where
  foldMap f = go where go = bifoldMap f go . out

instance Bifoldable Syntax where
  bifoldMap f g s = case s of
    Var _ -> mempty
    Prim i -> f i
    Op1 _ a -> g a
    Op2 _ a b -> g a `mappend` g b
    App a b -> g a `mappend` g b
    Lam _ a -> g a
    Rec _ a -> g a
    If c t e -> g c `mappend` g t `mappend` g e

instance Foldable (Syntax a) where
  foldMap = bifoldMap (const mempty)


instance Functor Term where
  fmap f = go where go = In . bimap f go . out

instance Bifunctor Syntax where
  bimap f g s = case s of
    Var n -> Var n
    Prim v -> Prim (f v)
    Op1 o a -> Op1 o (g a)
    Op2 o a b -> Op2 o (g a) (g b)
    App a b -> App (g a) (g b)
    Lam n a -> Lam n (g a)
    Rec n a -> Rec n (g a)
    If c t e -> If (g c) (g t) (g e)

instance Functor (Syntax a) where
  fmap = second


instance Traversable Term where
  traverse f = go where go = fmap In . bitraverse f go . out

instance Bitraversable Syntax where
  bitraverse f g s = case s of
    Var n -> pure (Var n)
    Prim v -> Prim <$> f v
    Op1 o a -> Op1 o <$> g a
    Op2 o a b -> Op2 o <$> g a <*> g b
    App a b -> App <$> g a <*> g b
    Lam n a -> Lam n <$> g a
    Rec n a -> Rec n <$> g a
    If c t e -> If <$> g c <*> g t <*> g e

instance Traversable (Syntax a) where
  traverse = bitraverse pure


instance Eq1 Term where
  liftEq eqA = go where go t1 t2 = liftEq2 eqA go (out t1) (out t2)

instance Eq2 Syntax where
  liftEq2 eqV eqA s1 s2 = case (s1, s2) of
    (Var n1, Var n2) -> n1 == n2
    (Prim v1, Prim v2) -> eqV v1 v2
    (Op1 o1 a1, Op1 o2 a2) -> o1 == o2 && eqA a1 a2
    (Op2 o1 a1 b1, Op2 o2 a2 b2) -> o1 == o2 && eqA a1 a2 && eqA b1 b2
    (App a1 b1, App a2 b2) -> eqA a1 a2 && eqA b1 b2
    (Lam n1 a1, Lam n2 a2) -> n1 == n2 && eqA a1 a2
    (Rec n1 a1, Rec n2 a2) -> n1 == n2 && eqA a1 a2
    (If c1 t1 e1, If c2 t2 e2) -> eqA c1 c2 && eqA t1 t2 && eqA e1 e2
    _ -> False

instance Eq a => Eq1 (Syntax a) where
  liftEq = liftEq2 (==)

instance Ord1 Term where
  liftCompare compareA = go where go t1 t2 = liftCompare2 compareA go (out t1) (out t2)

instance Ord2 Syntax where
  liftCompare2 compareV compareA s1 s2 = case (s1, s2) of
    (Var n1, Var n2) -> compare n1 n2
    (Var{}, _) -> LT
    (_, Var{}) -> GT
    (Prim v1, Prim v2) -> compareV v1 v2
    (Prim{}, _) -> LT
    (_, Prim{}) -> GT
    (Op1 o1 a1, Op1 o2 a2) -> compare o1 o2 <> compareA a1 a2
    (Op1{}, _) -> LT
    (_, Op1{}) -> GT
    (Op2 o1 a1 b1, Op2 o2 a2 b2) -> compare o1 o2 <> compareA a1 a2 <> compareA b1 b2
    (Op2{}, _) -> LT
    (_, Op2{}) -> GT
    (App a1 b1, App a2 b2) -> compareA a1 a2 <> compareA b1 b2
    (App{}, _) -> LT
    (_, App{}) -> GT
    (Lam n1 a1, Lam n2 a2) -> compare n1 n2 <> compareA a1 a2
    (Lam{}, _) -> LT
    (_, Lam{}) -> GT
    (Rec n1 a1, Rec n2 a2) -> compare n1 n2 <> compareA a1 a2
    (Rec{}, _) -> LT
    (_, Rec{}) -> GT
    (If c1 t1 e1, If c2 t2 e2) -> compareA c1 c2 <> compareA t1 t2 <> compareA e1 e2

instance Ord a => Ord1 (Syntax a) where
  liftCompare = liftCompare2 compare


instance Show1 Term where
  liftShowsPrec spA slA = go where go d t = showsUnaryWith (liftShowsPrec2 spA slA go (liftShowList spA slA)) "In" d (out t)

instance Show2 Syntax where
  liftShowsPrec2 spV _ spA _ d s = case s of
    Var n -> showsUnaryWith showsPrec "Var" d n
    Prim v -> showsUnaryWith spV "Prim" d v
    Op1 o a -> showsBinaryWith showsPrec spA "Op1" d o a
    Op2 o a b -> showsTernaryWith showsPrec spA spA "Op2" d o a b
    App a b -> showsBinaryWith spA spA "App" d a b
    Lam n a -> showsBinaryWith showsPrec spA "Lam" d n a
    Rec n a -> showsBinaryWith showsPrec spA "Rec" d n a
    If c t e -> showsTernaryWith spA spA spA "If" d c t e

instance Show a => Show1 (Syntax a) where
  liftShowsPrec = liftShowsPrec2 showsPrec showList


instance Num a => Num (Term a) where
  fromInteger = In . Prim . fromInteger

  signum = In . Op1 Signum
  abs = In . Op1 Abs
  negate = In . Op1 Negate
  (+) = (In .) . Op2 Plus
  (-) = (In .) . Op2 Minus
  (*) = (In .) . Op2 Times


instance Pretty1 Term where
  liftPretty p pl = go where go = liftPretty2 p pl go (liftPrettyList p pl) . out

instance Pretty a => Pretty (Term a) where
  pretty = pretty1

instance Pretty2 Syntax where
  liftPretty2 pv _ pr _ s = case s of
    Var n -> pretty n
    Prim v -> pv v
    Op1 o a -> pretty o <+> pr a
    Op2 o a b -> pr a <+> pretty o <+> pr b
    App a b -> pr a <+> parens (pr b)
    Lam n a -> parens (pretty '\\' <+> pretty n <+> pretty "->" <> nest 2 (line <> pr a))
    Rec n a -> pretty "fix" <+> parens (pretty '\\' <+> pretty n <+> pretty "->" <> nest 2 (line <> pr a))
    If c t e -> pretty "if" <+> pr c <+> pretty "then" <> nest 2 (line <> pr t) <> line <> pretty "else" <> nest 2 (line <> pr e)

instance Pretty a => Pretty1 (Syntax a) where
  liftPretty = liftPretty2 pretty prettyList
