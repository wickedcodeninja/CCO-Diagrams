-------------------------------------------------------------------------------
-- |
-- Module      :  CCO.Diag.Base
-- Copyright   :  (c) 2008 Utrecht University
-- License     :  All rights reserved
--
-- Maintainer  :  stefan@cs.uu.nl
-- Stability   :  provisional
-- Portability :  portable
--
-- T-diagrams.
--
-------------------------------------------------------------------------------

module CCO.Diag.Base (
    -- * Syntax
    Ident
  , Diag (..)      -- instances: Tree
  , Diag_ (..)     -- instances: Tree
  , DiagBinds (..) -- instances: Tree
  , DiagType (..)  -- instances: Tree
  
  , Platform (..)
  , Language (..)
  , Implementation (..)
) where

import CCO.Diag.AG
