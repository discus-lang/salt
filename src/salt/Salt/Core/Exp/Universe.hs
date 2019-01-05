
module Salt.Core.Exp.Universe where


-- | What level of the hierarchy we might be talking about.
data Universe
        = UKind         -- level 2
        | UType         -- level 1
        | UTerm         -- level 0
        deriving (Eq, Show)


-- | Get the next universe up.
universeUp :: Universe -> Maybe Universe
universeUp uni
 = case uni of
        UKind   -> Nothing
        UType   -> Just UKind
        UTerm   -> Just UTerm
