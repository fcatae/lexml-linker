{-# LANGUAGE FlexibleContexts #-}

module LexML.Linker.RegrasDebug (parseCases) where

-- import Data.Char
-- import Data.Maybe
-- import Data.Typeable

-- import Control.Monad
-- import Control.Monad.Trans
-- import Control.Monad.Except
-- import Control.Monad.Identity
-- import Control.Monad.State
-- import Control.Monad.Writer

-- import qualified Data.Map as M
import Text.Parsec
-- import Text.Parsec.Prim
-- import Text.Parsec.Pos (newPos)

-- import LexML.URN.Show
-- import LexML.URN.Utils
-- import qualified LexML.Linker.Municipios as M
-- import qualified LexML.Linker.Estados as E
-- import qualified Data.Set as S
-- import Data.List(intercalate)

-- import Prelude hiding (log)
-- import System.Log.Logger

import LexML.URN.Types
import LexML.Linker.ParserBase
import qualified LexML.URN.Atalhos as U
import qualified LexML.Linker.Regras2 as R2

parseCases :: [ParseCase2]
parseCases = (checkDebug >> runDebugParser) : R2.parseCases

checkDebug :: LinkerParserMonad ()
checkDebug = do
   a <- constanteI "debug"
   return ()

runDebugParser :: ParseCase2
runDebugParser = choice [ dbgParseMes , dbgParseDataAbrev ]
  where
  dbgParseMes = do
    i <- constanteI "parseMes"
    (p, mes) <- R2.parseMes
    return [(p, p, createLink "parseMes" $ show mes)]

  dbgParseDataAbrev = do
    i <- constanteI "parseDataAbrev"
    (p, f, dt) <- R2.parseDataAbrev
    return [(p, f, createLink "parseDataAbrev" $ show dt)]


createLink :: String -> String -> URNLexML -> URNLexML
createLink categoria nome ctx = URNLexML (Local Brasil Nothing) 
                (Documento (A_Normal [
                            SJ_Cargo (Cargo $ Nome ["test"])]) 
                            (TipoDocumento1 (STD1_Norma (TipoNorma $ Nome [categoria])) Nothing) 
                            (Descritor (TD_Apelido Nothing (ApelidoDocumento $ Nome [nome])) [] Nothing) )
                Nothing
                (Just $ Forma (TipoForma $ Nome ["debug"]) [])
                Nothing
