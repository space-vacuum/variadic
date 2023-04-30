{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Control.Variadic.Varargs where

import Data.Kind (Type)

-- | Glorified HList representing variadic arguments.
type Varargs :: [Type] -> Type
data family Varargs l
data instance Varargs '[] = Nil
data instance Varargs (x ': xs) = x `Cons` Varargs xs
infixr 2 `Cons`
