
module Salt.Core.Codec.Text.Parser where
import Salt.Core.Codec.Text.Parser.Term
import Salt.Core.Codec.Text.Parser.Base
import Salt.Core.Codec.Text.Lexer
import Salt.Core.Codec.Text.Token
import Salt.Core.Exp

import Text.Parsec                              ((<?>))
import qualified Text.Parsec                    as P


---------------------------------------------------------------------------------------------------
-- | Run a parser on a list of located tokens.
parse   :: Parser a -> FilePath -> [At Token]
        -> Either P.ParseError (a, [At Token])
parse p f ts
 = P.parse
        (do result <- p
            rest   <- P.getInput
            return (result, rest))
        f ts

---------------------------------------------------------------------------------------------------
-- | Parser for a module.
pModule :: Parser (Module Location)
pModule
 = do   decls   <- P.many pDecl
        pTok KEnd
        return  $ Module decls


---------------------------------------------------------------------------------------------------
-- | Parser for a top-level declaration.
pDecl :: Parser (Decl Location)
pDecl
 = P.choice
 [ do   -- 'test' Name? 'print'  Term
        -- 'test' Name? 'assert' Term
        loc <- getLocation
        pTok KTest
        P.choice
         [ do   P.lookAhead $ do
                        P.optionMaybe pVar
                        P.choice [ pTok KPrint, pTok KAssert, pTok KScenario ]

                mName   <- P.optionMaybe pVar

                P.choice
                 [ do   pTok KPrint
                        mBody   <- pTerm
                        return  $ DTest $ DeclTestPrint
                                { declAnnot     = loc
                                , declTestName  = mName
                                , declTestBody  = mBody }

                 , do   pTok KAssert
                        mBody   <- pTerm
                        return  $ DTest $ DeclTestAssert
                                { declAnnot     = loc
                                , declTestName  = mName
                                , declTestBody  = mBody }

                 ]

          , do  mBody <- pTerm
                return  $ DTest $ DeclTestPrint
                        { declAnnot     = loc
                        , declTestName  = Nothing
                        , declTestBody  = mBody }
         ]
         <?> "a test declaration"
 ]
 <?> "a declaration"


