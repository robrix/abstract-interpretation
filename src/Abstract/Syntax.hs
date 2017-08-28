{-# LANGUAGE FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, TypeFamilies #-}
module Abstract.Syntax where

import Abstract.Primitive
import Abstract.Set
import Abstract.Type
import Data.Bifoldable
import Data.Bifunctor
import Data.Bitraversable
import Data.Foldable (fold)
import Data.Functor.Classes
import Data.Functor.Foldable hiding (fold)
import Data.Pointed
import Data.Text.Prettyprint.Doc

data Syntax a r
  = Var Name
  | Prim a
  | Op1 Op1 r
  | Op2 Op2 r r
  | App r r
  | Lam Name Type r
  | Rec Name Type r
  | If r r r
  deriving (Eq, Ord, Show)

type Name = String

newtype Term a = In { out :: Syntax a (Term a) }
  deriving (Eq, Ord, Show)


var :: Name -> Term a
var = In . Var

prim :: a -> Term a
prim = In . Prim

true :: Term Prim
true = prim (PBool True)

false :: Term Prim
false = prim (PBool False)


infixl 9 #
(#) :: Term a -> Term a -> Term a
(#) = (In .) . App


eq :: Term a -> Term a -> Term a
eq = (In .) . Op2 Eq

lt :: Term a -> Term a -> Term a
lt = (In .) . Op2 Lt

lte :: Term a -> Term a -> Term a
lte = (In .) . Op2 LtE

gt :: Term a -> Term a -> Term a
gt = (In .) . Op2 Gt

gte :: Term a -> Term a -> Term a
gte = (In .) . Op2 GtE

and' :: Term a -> Term a -> Term a
and' = (In .) . Op2 And

or' :: Term a -> Term a -> Term a
or' = (In .) . Op2 Or

not' :: Term a -> Term a
not' = In . Op1 Not

div' :: Term a -> Term a -> Term a
div' = (In .) . Op2 DividedBy

quot' :: Term a -> Term a -> Term a
quot' = (In .) . Op2 Quotient

rem' :: Term a -> Term a -> Term a
rem' = (In .) . Op2 Remainder

mod' :: Term a -> Term a -> Term a
mod' = (In .) . Op2 Modulus


lam :: Name -> Type -> (Term a -> Term a) -> Term a
lam s ty f = makeLam s ty (f (var s))

makeLam :: Name -> Type -> Term a -> Term a
makeLam name ty body = In (Lam name ty body)

rec :: Name -> Type -> (Term a -> Term a) -> Term a
rec f ty1 b = makeRec f ty1 (b (var f))

makeRec :: Name -> Type -> Term a -> Term a
makeRec name ty body = In (Rec name ty body)

if' :: Term a -> Term a -> Term a -> Term a
if' c t e = In (If c t e)

let' :: Name -> Term a -> Type -> (Term a -> Term a) -> Term a
let' var val ty body = lam var ty body # val


freeVariables :: Term a -> Set Name
freeVariables = cata (\ syntax -> case syntax of
  Var n -> point n
  Lam n _ body -> delete n body
  Rec n _ body -> delete n body
  _ -> fold syntax)


showsConstructor :: String -> Int -> [Int -> ShowS] -> ShowS
showsConstructor name d fields = showParen (d > 10) $ showString name . showChar ' ' . foldr (.) id ([($ 11)] <*> fields)


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
    Lam _ _ a -> g a
    Rec _ _ a -> g a
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
    Lam n t a -> Lam n t (g a)
    Rec n t a -> Rec n t (g a)
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
    Lam n t a -> Lam n t <$> g a
    Rec n t a -> Rec n t <$> g a
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
    (Lam n1 t1 a1, Lam n2 t2 a2) -> n1 == n2 && t1 == t2 && eqA a1 a2
    (Rec n1 t1 a1, Rec n2 t2 a2) -> n1 == n2 && t1 == t2 && eqA a1 a2
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
    (Lam n1 t1 a1, Lam n2 t2 a2) -> compare n1 n2 <> compare t1 t2 <> compareA a1 a2
    (Lam{}, _) -> LT
    (_, Lam{}) -> GT
    (Rec n1 t1 a1, Rec n2 t2 a2) -> compare n1 n2 <> compare t1 t2 <> compareA a1 a2
    (Rec{}, _) -> LT
    (_, Rec{}) -> GT
    (If c1 t1 e1, If c2 t2 e2) -> compareA c1 c2 <> compareA t1 t2 <> compareA e1 e2

instance Ord a => Ord1 (Syntax a) where
  liftCompare = liftCompare2 compare


instance Show1 Term where
  liftShowsPrec spA slA = go where go d t = showsUnaryWith (liftShowsPrec2 spA slA go (liftShowList spA slA)) "In" d (out t)

instance Show2 Syntax where
  liftShowsPrec2 spV _ spA _ d s = case s of
    Var n -> showsConstructor "Var" d [ flip showsPrec n ]
    Prim v -> showsConstructor "Prim" d [ flip spV v ]
    Op1 o a -> showsConstructor "Op1" d [ flip showsPrec o, flip spA a ]
    Op2 o a b -> showsConstructor "Op2" d [ flip showsPrec o, flip spA a, flip spA b ]
    App a b -> showsConstructor "App" d (flip spA <$> [ a, b ])
    Lam n t a -> showsConstructor "Lam" d [ flip showsPrec n, flip showsPrec t, flip spA a ]
    Rec n t a -> showsConstructor  "Rec" d [ flip showsPrec n, flip showsPrec t, flip spA a ]
    If c t e -> showsConstructor "If" d (flip spA <$> [c, t, e])

instance Show a => Show1 (Syntax a) where
  liftShowsPrec = liftShowsPrec2 showsPrec showList


instance Num a => Num (Term a) where
  fromInteger = prim . fromInteger

  signum = In . Op1 Signum
  abs    = In . Op1 Abs
  negate = In . Op1 Negate
  (+) = (In .) . Op2 Plus
  (-) = (In .) . Op2 Minus
  (*) = (In .) . Op2 Times


instance Pretty1 Term where
  liftPretty p pl = go where go = liftPretty2 p pl go (list . map (liftPretty p pl)) . out

instance Pretty a => Pretty (Term a) where
  pretty = liftPretty pretty prettyList

instance Pretty2 Syntax where
  liftPretty2 pv _ pr _ s = case s of
    Var n -> pretty n
    Prim v -> pv v
    Op1 o a -> pretty o <+> pr a
    Op2 o a b -> pr a <+> pretty o <+> pr b
    App a b -> pr a <+> parens (pr b)
    Lam n t a -> parens (pretty '\\' <+> pretty n <+> colon <+> pretty t <+> pretty "." <> nest 2 (line <> pr a))
    Rec n t a -> pretty "fix" <+> parens (pretty '\\' <+> pretty n <+> colon <+> pretty t <+> pretty "." <> nest 2 (line <> pr a))
    If c t e -> pretty "if" <+> pr c <+> pretty "then" <> nest 2 (line <> pr t) <> line <> pretty "else" <> nest 2 (line <> pr e)

instance Pretty a => Pretty1 (Syntax a) where
  liftPretty = liftPretty2 pretty prettyList
