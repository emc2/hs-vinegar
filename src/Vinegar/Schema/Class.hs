-- Copyright (c) 2017 Eric McCorkle.  All rights reserved.
--
-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions
-- are met:
--
-- 1. Redistributions of source code must retain the above copyright
--    notice, this list of conditions and the following disclaimer.
--
-- 2. Redistributions in binary form must reproduce the above copyright
--    notice, this list of conditions and the following disclaimer in the
--    documentation and/or other materials provided with the distribution.
--
-- 3. Neither the name of the author nor the names of any contributors
--    may be used to endorse or promote products derived from this software
--    without specific prior written permission.
--
-- THIS SOFTWARE IS PROVIDED BY THE AUTHORS AND CONTRIBUTORS ``AS IS''
-- AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
-- TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
-- PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHORS
-- OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
-- SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
-- LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF
-- USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
-- ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
-- OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT
-- OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
-- SUCH DAMAGE.
{-# OPTIONS_GHC -funbox-strict-fields -Wall -Werror #-}
{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}

module Vinegar.Schema.Class(
       Schema(..),
       ) where

import Vinegar.Encoding.Class

-- | A class for schemas.  Schemas are data structures which connect
-- unpickled data structures to their serialized representations.
-- These take the form of an AST-like structure equipped with a pickle
-- and unpickle function.
class Encoding decodety encodedty encty =>
      Schema decodety encodedty encty schematy |
        encty -> decodety where
  -- | Convert a data type to its pickled form, according to a format
  -- and a schema.
  encode :: encty
         -- ^ The format to use.
         -> schematy rawty
         -- ^ The schema to use.
         -> rawty
         -- ^ The raw data object.
         -> encodedty
         -- ^ The serialized form of the object.

  -- | Deserialize an object according to a format and a schema.
  decode :: encty
         -- ^ The format to use.
         -> schematy rawty
         -- ^ The schema to use.
         -> encodedty
         -- ^ The serialized object.
         -> decodety rawty
         -- ^ The unpickled object.
