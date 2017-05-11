-------------------------------------------------------------------------------
-- |
-- Module      :  CCO.Diag.Parser
-- Copyright   :  (c) 2008 Utrecht University
--                    2017 Wout Elsinghorst, Xander van der Goot
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
import CCO.Diag.Base                 (Diag (..), Diag_ (..), DiagBinds (..), DiagType (..), Implementation, Language, Platform)
import CCO.Diag.Lexer                (Token, lexer, keyword, ident)
import CCO.Parsing                   (Parser, eof, sourcePos, (<!>), manySepBy, opt)
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

-- | Parses a 'Diag'.
pDiag :: TokenParser Diag
pDiag = pPlain <|> pLet <|> pUse

pPlain :: TokenParser Diag
pPlain = uncurry <$> (Diag <$> sourcePos <*> pure BindNil) <*> pDiag_

-- | Parses a 'Use'.
pUse :: TokenParser Diag
pUse = uncurry <$> (Use <$> sourcePos <* keyword "use") <*> pTypeSignature ident
                      
-- | Parses a 'Let'.
pLet :: TokenParser Diag
pLet = uncurry <$> (Diag <$> sourcePos <* keyword "let" <*> pBinds <* keyword "in") <*> pDiag_

-- | Parses the declarations in a 'Let' expression.
pBinds :: TokenParser DiagBinds
pBinds = createBinds <$ keyword "[" <*> manySepBy (keyword ",") pBind <* keyword "]"
  where
    createBinds []               = BindNil
    createBinds ( (name, d):ds ) = BindCons name d (createBinds ds)
    
pBind :: TokenParser (String, Diag)
pBind = (\name d -> (name, d)) <$> ident <* keyword "=" <*> pDiag 

-- | Parses a 'Diag_'.
pDiag_ :: TokenParser (Maybe DiagType, Diag_)
pDiag_ = pProgram <|> pPlatform <|> pInterpreter <|> pCompiler <|>
         pExecute <|> pCompile <!>
         "diagram"

-- | Parses a 'Platform'.
pPlatform :: TokenParser (Maybe DiagType, Diag_)
pPlatform = (,) <$> pure Nothing
                <*> ( Platform <$ keyword "platform" <*> ident
                    )

-- | Parses a 'Program'.
pProgram :: TokenParser (Maybe DiagType, Diag_)
pProgram = (,) <$> pKeywordWithTypeSignature "program" 
               <*> ( Program <$> ident <*
                                 keyword "in" <*> ident
                   )
                
-- | Parses an 'Interpreter'.
pInterpreter :: TokenParser (Maybe DiagType, Diag_)
pInterpreter = (,) <$> pKeywordWithTypeSignature "interpreter"
                   <*> ( Interpreter <$> ident <*
                                         keyword "for"         <*> ident <*
                                         keyword "in"          <*> ident
                       )

-- | Parses a 'Compiler'.
pCompiler :: TokenParser (Maybe DiagType, Diag_)
pCompiler = (,) <$> pKeywordWithTypeSignature "compiler"
                <*> ( Compiler <$> ident <*
                                   keyword "from" <*> ident <*
                                   keyword "to"       <*> ident <*
                                   keyword "in"       <*> ident
                    )

-- | Parses an 'Execute'.
pExecute :: TokenParser (Maybe DiagType, Diag_)
pExecute = (,) <$> pKeywordWithTypeSignature "execute"
               <*> ( Execute <$> pDiag <*
                                 keyword "on"     <*> pDiag <*
                                 keyword "end"
                   )

-- | Parses a 'Compile'.
pCompile :: TokenParser (Maybe DiagType, Diag_)
pCompile = (,) <$> pKeywordWithTypeSignature "compile"
               <*> ( Compile <$> pDiag <*
                                 keyword "with"    <*> pDiag <*
                                 keyword "end" 
                   )
                   
-- Type Signatures
                   
pKeywordWithTypeSignature :: String -> TokenParser (Maybe DiagType)
pKeywordWithTypeSignature = fmap snd . pTypeSignature . keyword
               
pTypeSignature :: TokenParser a -> TokenParser (a, Maybe DiagType)
pTypeSignature p = (,) <$> p <*> opt ( Just <$ keyword "::" <*> pDiagType ) Nothing
       
pDiagType :: TokenParser DiagType
pDiagType = pProgramType <|> pCompilerType <|> pInterpreterType <|> pPlatformType
       
pProgramType :: TokenParser DiagType
pProgramType = Type_Program 
  <$ keyword "Program" 
  <*> pImplementationLanguage
 
pCompilerType :: TokenParser DiagType
pCompilerType = (\l1 l2 m -> Type_Compiler (l1, l2) m) 
  <$  keyword "Compiler" 
  <*> pInputLanguageIdent <* keyword "~>" 
  <*> pOutputLanguageIdent <* keyword "/" 
  <*> pImplementationLanguage 
  
pInterpreterType :: TokenParser DiagType
pInterpreterType = Type_Interpreter
  <$  keyword "Interpreter" 
  <*> pInputLanguageIdent <* keyword "~>"
  <*> pImplementationLanguage 
 
pPlatformType :: TokenParser DiagType
pPlatformType = Type_Platform
  <$  keyword "Platform" 
  <*> pPlatformIdent
 
pImplementationLanguage :: TokenParser Implementation
pImplementationLanguage =  Right <$> pOutputLanguageIdent 
                       <|> Left  <$> pPlatformIdent

pOutputLanguageIdent :: TokenParser Language
pOutputLanguageIdent = keyword "{" *> ident <* keyword "}"

pInputLanguageIdent :: TokenParser Language
pInputLanguageIdent = keyword "[" *> ident <* keyword "]"

pPlatformIdent :: TokenParser Platform
pPlatformIdent = keyword "{!" *> ident <* keyword "!}"
