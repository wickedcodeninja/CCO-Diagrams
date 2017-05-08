-------------------------------------------------------------------------------
-- |
-- Module      :  CCO.Diag.Parser
-- Copyright   :  (c) 2008 Utrecht University
-- License     :  All rights reserved
--
-- Maintainer  :  stefan@cs.uu.nl
-- Stability   :  provisional
-- Portability :  portable
--
-- A 'Parser' for 'Diag's.
--
-------------------------------------------------------------------------------

module CCO.Diag.Parser (
    -- * Parser
    parser    -- :: Component String Diag
) where

import CCO.Component                 (Component)
import qualified CCO.Component as C  (parser)
import CCO.Diag.Base                 (Diag (..), Diag_ (..), DiagBinds (..))
import CCO.Diag.Lexer                (Token, lexer, keyword, ident)
import CCO.Parsing                   (Parser, eof, sourcePos, (<!>), manySepBy)
import Control.Applicative

-------------------------------------------------------------------------------
-- Token parsers
-------------------------------------------------------------------------------

-- | Type of 'Parser's that consume symbols described by 'Token's.
type TokenParser = Parser Token

-------------------------------------------------------------------------------
-- Parser
-------------------------------------------------------------------------------

-- | A 'Component' for parsing 'Diag's.
parser :: Component String Diag
parser = C.parser lexer (pDiag <* eof)

pDiag :: TokenParser Diag
pDiag = pPlain <|> pLet <|> pUse

-- | Parses a 'Diag'.
pPlain :: TokenParser Diag
pPlain = Diag <$> sourcePos <*> pure BindNil <*> pDiag_

-- | Parses a 'Use'.
pUse :: TokenParser Diag
pUse = Use <$> sourcePos <* keyword "use" <*> ident  
                      
-- | Parses a 'Let'.
pLet :: TokenParser Diag
pLet = Diag <$> sourcePos <* keyword "let" <*> pBinds <*
               keyword "in" <*> pDiag_
-- | Parses a 'Let'.
pBinds :: TokenParser DiagBinds
pBinds = createBinds <$ keyword "[" <*> manySepBy (keyword ",") pBind <* keyword "]"
  where
    createBinds []               = BindNil
    createBinds ( (name, d):ds ) = BindCons name d (createBinds ds)
    
pBind :: TokenParser (String, Diag)
pBind = (\name d -> (name, d)) <$> ident <* keyword "=" <*> pDiag 

-- | Parses a 'Diag_'.
pDiag_ :: TokenParser Diag_
pDiag_ = pProgram <|> pPlatform <|> pInterpreter <|> pCompiler <|>
         pExecute <|> pCompile <!>
         "diagram"

-- | Parses a 'Program'.
pProgram :: TokenParser Diag_
pProgram = Program <$ keyword "program" <*> ident <*
                      keyword "in"      <*> ident

-- | Parses a 'Platform'.
pPlatform :: TokenParser Diag_
pPlatform = Platform <$ keyword "platform" <*> ident

-- | Parses an 'Interpreter'.
pInterpreter :: TokenParser Diag_
pInterpreter = Interpreter <$ keyword "interpreter" <*> ident <*
                              keyword "for"         <*> ident <*
                              keyword "in"          <*> ident

-- | Parses a 'Compiler'.
pCompiler :: TokenParser Diag_
pCompiler = Compiler <$ keyword "compiler" <*> ident <*
                        keyword "from"     <*> ident <*
                        keyword "to"       <*> ident <*
                        keyword "in"       <*> ident

-- | Parses an 'Execute'.
pExecute :: TokenParser Diag_
pExecute = Execute <$ keyword "execute" <*> pDiag <*
                      keyword "on"     <*> pDiag <*
                      keyword "end"

-- | Parses a 'Compile'.
pCompile :: TokenParser Diag_
pCompile = Compile <$ keyword "compile" <*> pDiag <*
                      keyword "with"    <*> pDiag <*
                      keyword "end"
