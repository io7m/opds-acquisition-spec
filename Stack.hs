{-# LANGUAGE Haskell2010, ExplicitForAll #-}

module Stack where

data Stack a
  = Stack [a]
  deriving (Eq, Show)

stackEmpty :: forall a. Stack a
stackEmpty = Stack []

stackOf :: forall a. a -> Stack a
stackOf x = Stack [x]

stackPush :: forall a. Stack a -> a -> Stack a
stackPush (Stack xs) x = Stack (xs ++ [x])

stackPop :: forall a. Stack a -> Stack a
stackPop (Stack []) = error "Stack is empty"
stackPop (Stack xs) = Stack (reverse $ drop 1 $ reverse xs)

stackList :: forall a. Stack a -> [a]
stackList (Stack xs) = xs
