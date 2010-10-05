module DataCss where

type Css  = [Rule]
type Rule = (Selector, [Property])

data Selector = SimpSelector SSelector
              | CombSelector SSelector Combinator Selector -- combinators: ' ', >, +
                    deriving Show

data SSelector = TypeSelector String
               | UnivSelector
                    deriving Show

type Combinator = String

data Property = Property String ValueProperty
                    deriving Show

data ValueProperty = None | Inline | Block
        deriving Show

