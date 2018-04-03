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
-- import Text.Parsec
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
parseCases = (check2 >> get2) : R2.parseCases

check2 :: LinkerParserMonad ()
check2 = do
   a <- constanteI "debug"
   return ()

get2 :: ParseCase2
get2 = do
   i <- constanteI "debug"
   return [(i, i, lei)]
   where
     lei = \_ -> U.leiFederal [123] 1 4 2018
