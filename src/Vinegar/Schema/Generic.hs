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
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses,
             ScopedTypeVariables, Rank2Types, UndecidableInstances #-}

module Vinegar.Schema.Generic(
       -- * Schema
       Generic,

       -- * Base Types
       bool,
       unboundedIntegral,
       boundedIntegral,
       variableBytes,
       variableLazyBytes,
       fixedBytes,
       fixedLazyBytes,

       -- * Wrapped
       wrap,

       -- * Derived Structures

       -- ** Option and Choice
       optional,
       choice,

       -- ** Uniform Sets and Sequences
       sequenceOf,
       setOf,

       -- ** Non-Uniform Structs and Sets

       -- *** Structs
       struct1,
       struct2,
       struct3,
       struct4,
       struct5,
       struct6,
       struct7,
       struct8,
       struct9,
       struct10,
       struct11,
       struct12,
       struct13,
       struct14,
       struct15,
       struct16,
       struct17,
       struct18,
       struct19,
       struct20,

       -- *** Sets
       set2,
       set3,
       set4,
       set5,
       set6,
       set7,
       set8,
       set9,
       set10,
       set11,
       set12,
       set13,
       set14,
       set15,
       set16,
       set17,
       set18,
       set19,
       set20,
) where

import Control.Applicative(Alternative, (<|>))
import Vinegar.Encoding.Generic
import Vinegar.Schema.Class

import qualified Data.Array as Array
import qualified Data.ByteString as Strict
import qualified Data.ByteString.Lazy as Lazy

-- | Generic schemas.  These are just structures of function pointers.
data Generic dataty =
  Generic {
    -- | Encoding function.
    genericEncode :: forall decodedty encodedty encty.
                     (GenericEncoding decodedty encodedty encty) =>
                     encty
                  -> dataty
                  -> encodedty,
    -- | Decoding function.
    genericDecode :: forall decodedty encodedty encty.
                     (Alternative decodedty,
                      GenericEncoding decodedty encodedty encty) =>
                     encty
                  -> encodedty
                  -> decodedty dataty,
    genericDesc :: forall descty.
                   GenericEncodingDesc descty =>
                   descty
  }

-- | Generic schema element for booleans.
bool :: Generic Bool
bool = Generic { genericEncode = encodeBool, genericDecode = decodeBool,
                 genericDesc = boolDesc }

-- | Generic schema element for unbounded integers.
unboundedIntegral :: (Integral n) =>
                     Generic n
unboundedIntegral = Generic { genericEncode = encodeUnboundedIntegral,
                              genericDecode = decodeUnboundedIntegral,
                              genericDesc = unboundedIntegralDesc }

-- | Generic schema element for bounded integers.
boundedIntegral :: (Integral n) =>
                   n
                -- ^ The lower bound on the number.
                -> n
                -- ^ The upper bound on the number.
                -> Generic n
boundedIntegral lo hi = Generic { genericEncode = encodeBoundedIntegral lo hi,
                                  genericDecode = decodeBoundedIntegral lo hi,
                                  genericDesc = boundedIntegralDesc lo hi }

-- | Generic schema element for variable-length strict 'ByteString's
variableBytes :: Generic Strict.ByteString
variableBytes = Generic { genericEncode = encodeVariableBytes,
                          genericDecode = decodeVariableBytes,
                          genericDesc = variableBytesDesc }

-- | Generic schema element for variable-length lazy 'ByteString's
variableLazyBytes :: Generic Lazy.ByteString
variableLazyBytes = Generic { genericEncode = encodeVariableLazyBytes,
                              genericDecode = decodeVariableLazyBytes,
                              genericDesc = variableBytesDesc }

-- | Generic schema element for fixed-length strict 'ByteString's
fixedBytes :: (Integral n) =>
              n
           -- ^ Length of the fixed byte string.
           -> Generic Strict.ByteString
fixedBytes n = Generic { genericEncode = encodeFixedBytes n,
                         genericDecode = decodeFixedBytes n,
                         genericDesc = fixedBytesDesc n }

-- | Generic schema element for fixed-length lazy 'ByteString's
fixedLazyBytes :: (Integral n) =>
                  n
               -- ^ Length of the fixed byte string.
               -> Generic Lazy.ByteString
fixedLazyBytes n = Generic { genericEncode = encodeFixedLazyBytes n,
                             genericDecode = decodeFixedLazyBytes n,
                             genericDesc = fixedBytesDesc n  }

-- | Wrap a schema in a type translation function that changes the
-- result type.
wrap :: (b -> a)
     -- ^ The encoding transform.
     -> (a -> b)
     -- ^ The decoding transform.
     -> Generic a
     -- ^ The base schema.
     -> Generic b
wrap fwd rev Generic { genericEncode = innerenc, genericDecode = innerdec,
                       genericDesc = innerdesc } =
  Generic { genericEncode = \enc -> innerenc enc . fwd,
            genericDecode = \enc schema -> rev  <$> (innerdec enc schema),
            genericDesc = innerdesc }

sequenceOf :: Generic a
           -> Generic [a]
sequenceOf Generic { genericEncode = innerenc, genericDecode = innerdec,
                     genericDesc = innerdesc } =
  Generic { genericEncode = \enc -> encodeSeqOf (innerenc enc) enc,
            genericDecode = \enc -> decodeSeqOf (innerdec enc) enc,
            genericDesc = sequenceOfDesc innerdesc }

setOf :: Generic a
      -> Generic [a]
setOf Generic { genericEncode = innerenc, genericDecode = innerdec,
                genericDesc = innerdesc } =
  Generic {
    genericEncode = \enc -> encodeSetOf (innerenc enc) enc,
    genericDecode = \enc schema -> decodeSetOf (innerdec enc) enc schema,
    genericDesc = setOfDesc innerdesc
  }

optional :: Generic a
         -> Generic (Maybe a)
optional Generic { genericEncode = innerenc, genericDecode = innerdec,
                   genericDesc = innerdesc } =
  Generic { genericEncode = \enc -> encodeOptional (innerenc enc) enc,
            genericDecode = \enc -> decodeOptional (innerdec enc) enc,
            genericDesc = optionalDesc innerdesc }

choice :: [(String, Generic a)]
       -- ^ The option list.
       -> (a -> Int)
       -- ^ Choice function, picks an element of the option list,
       -- starting at 0.
       -> Generic a
choice opts selector =
  let
    arr = Array.listArray (0, length opts - 1) (map snd opts)

    encodeImpl enc val =
      let
        Generic { genericEncode = inner } = arr Array.! selector val
      in
        encodeChoice (inner enc) enc val

    decodeImpl enc val =
      let
        mapfun Generic { genericDecode = inner } =
          decodeChoice (inner enc) enc val
      in
        foldr1 (<|>) (map mapfun (Array.elems arr))
  in
    Generic {
      genericEncode = encodeImpl, genericDecode = decodeImpl,
      genericDesc = choiceDesc (map (\(name, Generic { genericDesc = desc }) ->
                                      (name, desc)) opts)
    }

struct1 :: (String, Generic a)
        -> Generic a
struct1 (fname, Generic { genericEncode = innerenc1,
                          genericDecode = innerdec1,
                          genericDesc = innerdesc1 }) =
  Generic { genericEncode = \enc -> encodeStruct1 (innerenc1 enc) enc,
            genericDecode = \enc -> decodeStruct1 (innerdec1 enc) enc,
            genericDesc = structDesc [(fname, innerdesc1)] }

struct2 :: (String, Generic a)
        -> (String, Generic b)
        -> Generic (a, b)
struct2 (fname1, Generic { genericEncode = innerenc1,
                           genericDecode = innerdec1,
                           genericDesc = innerdesc1 })
        (fname2, Generic { genericEncode = innerenc2,
                           genericDecode = innerdec2,
                           genericDesc = innerdesc2 }) =
  Generic { genericEncode = \enc -> encodeStruct2 (innerenc1 enc)
                                                  (innerenc2 enc) enc,
            genericDecode = \enc -> decodeStruct2 (innerdec1 enc)
                                                  (innerdec2 enc)enc,
            genericDesc = structDesc [(fname1, innerdesc1),
                                      (fname2, innerdesc2)] }

struct3 :: (String, Generic a)
        -> (String, Generic b)
        -> (String, Generic c)
        -> Generic (a, b, c)
struct3 (fname1, Generic { genericEncode = innerenc1,
                           genericDecode = innerdec1,
                           genericDesc = innerdesc1 })
        (fname2, Generic { genericEncode = innerenc2,
                           genericDecode = innerdec2,
                           genericDesc = innerdesc2 })
        (fname3, Generic { genericEncode = innerenc3,
                           genericDecode = innerdec3,
                           genericDesc = innerdesc3 }) =
  Generic { genericEncode = \enc -> encodeStruct3 (innerenc1 enc)
                                                  (innerenc2 enc)
                                                  (innerenc3 enc) enc,
            genericDecode = \enc -> decodeStruct3 (innerdec1 enc)
                                                  (innerdec2 enc)
                                                  (innerdec3 enc) enc,
            genericDesc = structDesc [(fname1, innerdesc1),
                                      (fname2, innerdesc2),
                                      (fname3, innerdesc3)] }

struct4 :: (String, Generic a)
        -> (String, Generic b)
        -> (String, Generic c)
        -> (String, Generic d)
        -> Generic (a, b, c, d)
struct4 (fname1, Generic { genericEncode = innerenc1,
                           genericDecode = innerdec1,
                           genericDesc = innerdesc1 })
        (fname2, Generic { genericEncode = innerenc2,
                           genericDecode = innerdec2,
                           genericDesc = innerdesc2 })
        (fname3, Generic { genericEncode = innerenc3,
                           genericDecode = innerdec3,
                           genericDesc = innerdesc3 })
        (fname4, Generic { genericEncode = innerenc4,
                           genericDecode = innerdec4,
                           genericDesc = innerdesc4 }) =
  Generic { genericEncode = \enc -> encodeStruct4 (innerenc1 enc)
                                                  (innerenc2 enc)
                                                  (innerenc3 enc)
                                                  (innerenc4 enc) enc,
            genericDecode = \enc -> decodeStruct4 (innerdec1 enc)
                                                  (innerdec2 enc)
                                                  (innerdec3 enc)
                                                  (innerdec4 enc) enc,
            genericDesc = structDesc [(fname1, innerdesc1),
                                      (fname2, innerdesc2),
                                      (fname3, innerdesc3),
                                      (fname4, innerdesc4)] }

struct5 :: (String, Generic a)
        -> (String, Generic b)
        -> (String, Generic c)
        -> (String, Generic d)
        -> (String, Generic e)
        -> Generic (a, b, c, d, e)
struct5 (fname1, Generic { genericEncode = innerenc1,
                           genericDecode = innerdec1,
                           genericDesc = innerdesc1 })
        (fname2, Generic { genericEncode = innerenc2,
                           genericDecode = innerdec2,
                           genericDesc = innerdesc2 })
        (fname3, Generic { genericEncode = innerenc3,
                           genericDecode = innerdec3,
                           genericDesc = innerdesc3 })
        (fname4, Generic { genericEncode = innerenc4,
                           genericDecode = innerdec4,
                           genericDesc = innerdesc4 })
        (fname5, Generic { genericEncode = innerenc5,
                           genericDecode = innerdec5,
                           genericDesc = innerdesc5 }) =
  Generic { genericEncode = \enc -> encodeStruct5 (innerenc1 enc)
                                                  (innerenc2 enc)
                                                  (innerenc3 enc)
                                                  (innerenc4 enc)
                                                  (innerenc5 enc) enc,
            genericDecode = \enc -> decodeStruct5 (innerdec1 enc)
                                                  (innerdec2 enc)
                                                  (innerdec3 enc)
                                                  (innerdec4 enc)
                                                  (innerdec5 enc) enc,
            genericDesc = structDesc [(fname1, innerdesc1),
                                      (fname2, innerdesc2),
                                      (fname3, innerdesc3),
                                      (fname4, innerdesc4),
                                      (fname5, innerdesc5)] }

struct6 :: (String, Generic a)
        -> (String, Generic b)
        -> (String, Generic c)
        -> (String, Generic d)
        -> (String, Generic e)
        -> (String, Generic f)
        -> Generic (a, b, c, d, e, f)
struct6 (fname1, Generic { genericEncode = innerenc1,
                           genericDecode = innerdec1,
                           genericDesc = innerdesc1 })
        (fname2, Generic { genericEncode = innerenc2,
                           genericDecode = innerdec2,
                           genericDesc = innerdesc2 })
        (fname3, Generic { genericEncode = innerenc3,
                           genericDecode = innerdec3,
                           genericDesc = innerdesc3 })
        (fname4, Generic { genericEncode = innerenc4,
                           genericDecode = innerdec4,
                           genericDesc = innerdesc4 })
        (fname5, Generic { genericEncode = innerenc5,
                           genericDecode = innerdec5,
                           genericDesc = innerdesc5 })
        (fname6, Generic { genericEncode = innerenc6,
                           genericDecode = innerdec6,
                           genericDesc = innerdesc6 }) =
  Generic { genericEncode = \enc -> encodeStruct6 (innerenc1 enc)
                                                  (innerenc2 enc)
                                                  (innerenc3 enc)
                                                  (innerenc4 enc)
                                                  (innerenc5 enc)
                                                  (innerenc6 enc) enc,
            genericDecode = \enc -> decodeStruct6 (innerdec1 enc)
                                                  (innerdec2 enc)
                                                  (innerdec3 enc)
                                                  (innerdec4 enc)
                                                  (innerdec5 enc)
                                                  (innerdec6 enc) enc,
            genericDesc = structDesc [(fname1, innerdesc1),
                                      (fname2, innerdesc2),
                                      (fname3, innerdesc3),
                                      (fname4, innerdesc4),
                                      (fname5, innerdesc5),
                                      (fname6, innerdesc6)] }

struct7 :: (String, Generic a)
        -> (String, Generic b)
        -> (String, Generic c)
        -> (String, Generic d)
        -> (String, Generic e)
        -> (String, Generic f)
        -> (String, Generic g)
        -> Generic (a, b, c, d, e, f, g)
struct7 (fname1, Generic { genericEncode = innerenc1,
                           genericDecode = innerdec1,
                           genericDesc = innerdesc1 })
        (fname2, Generic { genericEncode = innerenc2,
                           genericDecode = innerdec2,
                           genericDesc = innerdesc2 })
        (fname3, Generic { genericEncode = innerenc3,
                           genericDecode = innerdec3,
                           genericDesc = innerdesc3 })
        (fname4, Generic { genericEncode = innerenc4,
                           genericDecode = innerdec4,
                           genericDesc = innerdesc4 })
        (fname5, Generic { genericEncode = innerenc5,
                           genericDecode = innerdec5,
                           genericDesc = innerdesc5 })
        (fname6, Generic { genericEncode = innerenc6,
                           genericDecode = innerdec6,
                           genericDesc = innerdesc6 })
        (fname7, Generic { genericEncode = innerenc7,
                           genericDecode = innerdec7,
                           genericDesc = innerdesc7 }) =
  Generic { genericEncode = \enc -> encodeStruct7 (innerenc1 enc)
                                                  (innerenc2 enc)
                                                  (innerenc3 enc)
                                                  (innerenc4 enc)
                                                  (innerenc5 enc)
                                                  (innerenc6 enc)
                                                  (innerenc7 enc) enc,
            genericDecode = \enc -> decodeStruct7 (innerdec1 enc)
                                                  (innerdec2 enc)
                                                  (innerdec3 enc)
                                                  (innerdec4 enc)
                                                  (innerdec5 enc)
                                                  (innerdec6 enc)
                                                  (innerdec7 enc) enc,
            genericDesc = structDesc [(fname1, innerdesc1),
                                      (fname2, innerdesc2),
                                      (fname3, innerdesc3),
                                      (fname4, innerdesc4),
                                      (fname5, innerdesc5),
                                      (fname6, innerdesc6),
                                      (fname7, innerdesc7)] }

struct8 :: (String, Generic a)
        -> (String, Generic b)
        -> (String, Generic c)
        -> (String, Generic d)
        -> (String, Generic e)
        -> (String, Generic f)
        -> (String, Generic g)
        -> (String, Generic h)
        -> Generic (a, b, c, d, e, f, g, h)
struct8 (fname1, Generic { genericEncode = innerenc1,
                           genericDecode = innerdec1,
                           genericDesc = innerdesc1 })
        (fname2, Generic { genericEncode = innerenc2,
                           genericDecode = innerdec2,
                           genericDesc = innerdesc2 })
        (fname3, Generic { genericEncode = innerenc3,
                           genericDecode = innerdec3,
                           genericDesc = innerdesc3 })
        (fname4, Generic { genericEncode = innerenc4,
                           genericDecode = innerdec4,
                           genericDesc = innerdesc4 })
        (fname5, Generic { genericEncode = innerenc5,
                           genericDecode = innerdec5,
                           genericDesc = innerdesc5 })
        (fname6, Generic { genericEncode = innerenc6,
                           genericDecode = innerdec6,
                           genericDesc = innerdesc6 })
        (fname7, Generic { genericEncode = innerenc7,
                           genericDecode = innerdec7,
                           genericDesc = innerdesc7 })
        (fname8, Generic { genericEncode = innerenc8,
                           genericDecode = innerdec8,
                           genericDesc = innerdesc8 }) =
  Generic { genericEncode = \enc -> encodeStruct8 (innerenc1 enc)
                                                  (innerenc2 enc)
                                                  (innerenc3 enc)
                                                  (innerenc4 enc)
                                                  (innerenc5 enc)
                                                  (innerenc6 enc)
                                                  (innerenc7 enc)
                                                  (innerenc8 enc) enc,
            genericDecode = \enc -> decodeStruct8 (innerdec1 enc)
                                                  (innerdec2 enc)
                                                  (innerdec3 enc)
                                                  (innerdec4 enc)
                                                  (innerdec5 enc)
                                                  (innerdec6 enc)
                                                  (innerdec7 enc)
                                                  (innerdec8 enc) enc,
            genericDesc = structDesc [(fname1, innerdesc1),
                                      (fname2, innerdesc2),
                                      (fname3, innerdesc3),
                                      (fname4, innerdesc4),
                                      (fname5, innerdesc5),
                                      (fname6, innerdesc6),
                                      (fname7, innerdesc7),
                                      (fname8, innerdesc8)] }

struct9 :: (String, Generic a)
        -> (String, Generic b)
        -> (String, Generic c)
        -> (String, Generic d)
        -> (String, Generic e)
        -> (String, Generic f)
        -> (String, Generic g)
        -> (String, Generic h)
        -> (String, Generic i)
        -> Generic (a, b, c, d, e, f, g, h, i)
struct9 (fname1, Generic { genericEncode = innerenc1,
                           genericDecode = innerdec1,
                           genericDesc = innerdesc1 })
        (fname2, Generic { genericEncode = innerenc2,
                           genericDecode = innerdec2,
                           genericDesc = innerdesc2 })
        (fname3, Generic { genericEncode = innerenc3,
                           genericDecode = innerdec3,
                           genericDesc = innerdesc3 })
        (fname4, Generic { genericEncode = innerenc4,
                           genericDecode = innerdec4,
                           genericDesc = innerdesc4 })
        (fname5, Generic { genericEncode = innerenc5,
                           genericDecode = innerdec5,
                           genericDesc = innerdesc5 })
        (fname6, Generic { genericEncode = innerenc6,
                           genericDecode = innerdec6,
                           genericDesc = innerdesc6 })
        (fname7, Generic { genericEncode = innerenc7,
                           genericDecode = innerdec7,
                           genericDesc = innerdesc7 })
        (fname8, Generic { genericEncode = innerenc8,
                           genericDecode = innerdec8,
                           genericDesc = innerdesc8 })
        (fname9, Generic { genericEncode = innerenc9,
                           genericDecode = innerdec9,
                           genericDesc = innerdesc9 }) =
  Generic { genericEncode = \enc -> encodeStruct9 (innerenc1 enc)
                                                  (innerenc2 enc)
                                                  (innerenc3 enc)
                                                  (innerenc4 enc)
                                                  (innerenc5 enc)
                                                  (innerenc6 enc)
                                                  (innerenc7 enc)
                                                  (innerenc8 enc)
                                                  (innerenc9 enc) enc,
            genericDecode = \enc -> decodeStruct9 (innerdec1 enc)
                                                  (innerdec2 enc)
                                                  (innerdec3 enc)
                                                  (innerdec4 enc)
                                                  (innerdec5 enc)
                                                  (innerdec6 enc)
                                                  (innerdec7 enc)
                                                  (innerdec8 enc)
                                                  (innerdec9 enc) enc,
            genericDesc = structDesc [(fname1, innerdesc1),
                                      (fname2, innerdesc2),
                                      (fname3, innerdesc3),
                                      (fname4, innerdesc4),
                                      (fname5, innerdesc5),
                                      (fname6, innerdesc6),
                                      (fname7, innerdesc7),
                                      (fname8, innerdesc8),
                                      (fname9, innerdesc9)] }

struct10 :: (String, Generic a)
         -> (String, Generic b)
         -> (String, Generic c)
         -> (String, Generic d)
         -> (String, Generic e)
         -> (String, Generic f)
         -> (String, Generic g)
         -> (String, Generic h)
         -> (String, Generic i)
         -> (String, Generic j)
         -> Generic (a, b, c, d, e, f, g, h, i, j)
struct10 (fname1, Generic { genericEncode = innerenc1,
                            genericDecode = innerdec1,
                            genericDesc = innerdesc1 })
         (fname2, Generic { genericEncode = innerenc2,
                            genericDecode = innerdec2,
                            genericDesc = innerdesc2 })
         (fname3, Generic { genericEncode = innerenc3,
                            genericDecode = innerdec3,
                            genericDesc = innerdesc3 })
         (fname4, Generic { genericEncode = innerenc4,
                            genericDecode = innerdec4,
                            genericDesc = innerdesc4 })
         (fname5, Generic { genericEncode = innerenc5,
                            genericDecode = innerdec5,
                            genericDesc = innerdesc5 })
         (fname6, Generic { genericEncode = innerenc6,
                            genericDecode = innerdec6,
                            genericDesc = innerdesc6 })
         (fname7, Generic { genericEncode = innerenc7,
                            genericDecode = innerdec7,
                            genericDesc = innerdesc7 })
         (fname8, Generic { genericEncode = innerenc8,
                            genericDecode = innerdec8,
                            genericDesc = innerdesc8 })
         (fname9, Generic { genericEncode = innerenc9,
                            genericDecode = innerdec9,
                            genericDesc = innerdesc9 })
         (fname10, Generic { genericEncode = innerenc10,
                             genericDecode = innerdec10,
                             genericDesc = innerdesc10 }) =
  Generic { genericEncode = \enc -> encodeStruct10 (innerenc1 enc)
                                                   (innerenc2 enc)
                                                   (innerenc3 enc)
                                                   (innerenc4 enc)
                                                   (innerenc5 enc)
                                                   (innerenc6 enc)
                                                   (innerenc7 enc)
                                                   (innerenc8 enc)
                                                   (innerenc9 enc)
                                                   (innerenc10 enc) enc,
            genericDecode = \enc -> decodeStruct10 (innerdec1 enc)
                                                   (innerdec2 enc)
                                                   (innerdec3 enc)
                                                   (innerdec4 enc)
                                                   (innerdec5 enc)
                                                   (innerdec6 enc)
                                                   (innerdec7 enc)
                                                   (innerdec8 enc)
                                                   (innerdec9 enc)
                                                   (innerdec10 enc) enc,
            genericDesc = structDesc [(fname1, innerdesc1),
                                      (fname2, innerdesc2),
                                      (fname3, innerdesc3),
                                      (fname4, innerdesc4),
                                      (fname5, innerdesc5),
                                      (fname6, innerdesc6),
                                      (fname7, innerdesc7),
                                      (fname8, innerdesc8),
                                      (fname9, innerdesc9),
                                      (fname10, innerdesc10)] }

struct11 :: (String, Generic a)
         -> (String, Generic b)
         -> (String, Generic c)
         -> (String, Generic d)
         -> (String, Generic e)
         -> (String, Generic f)
         -> (String, Generic g)
         -> (String, Generic h)
         -> (String, Generic i)
         -> (String, Generic j)
         -> (String, Generic k)
         -> Generic (a, b, c, d, e, f, g, h, i, j, k)
struct11 (fname1, Generic { genericEncode = innerenc1,
                            genericDecode = innerdec1,
                            genericDesc = innerdesc1 })
         (fname2, Generic { genericEncode = innerenc2,
                            genericDecode = innerdec2,
                            genericDesc = innerdesc2 })
         (fname3, Generic { genericEncode = innerenc3,
                            genericDecode = innerdec3,
                            genericDesc = innerdesc3 })
         (fname4, Generic { genericEncode = innerenc4,
                            genericDecode = innerdec4,
                            genericDesc = innerdesc4 })
         (fname5, Generic { genericEncode = innerenc5,
                            genericDecode = innerdec5,
                            genericDesc = innerdesc5 })
         (fname6, Generic { genericEncode = innerenc6,
                            genericDecode = innerdec6,
                            genericDesc = innerdesc6 })
         (fname7, Generic { genericEncode = innerenc7,
                            genericDecode = innerdec7,
                            genericDesc = innerdesc7 })
         (fname8, Generic { genericEncode = innerenc8,
                            genericDecode = innerdec8,
                            genericDesc = innerdesc8 })
         (fname9, Generic { genericEncode = innerenc9,
                            genericDecode = innerdec9,
                            genericDesc = innerdesc9 })
         (fname10, Generic { genericEncode = innerenc10,
                             genericDecode = innerdec10,
                             genericDesc = innerdesc10 })
         (fname11, Generic { genericEncode = innerenc11,
                             genericDecode = innerdec11,
                             genericDesc = innerdesc11 }) =
  Generic { genericEncode = \enc -> encodeStruct11 (innerenc1 enc)
                                                   (innerenc2 enc)
                                                   (innerenc3 enc)
                                                   (innerenc4 enc)
                                                   (innerenc5 enc)
                                                   (innerenc6 enc)
                                                   (innerenc7 enc)
                                                   (innerenc8 enc)
                                                   (innerenc9 enc)
                                                   (innerenc10 enc)
                                                   (innerenc11 enc) enc,
            genericDecode = \enc -> decodeStruct11 (innerdec1 enc)
                                                   (innerdec2 enc)
                                                   (innerdec3 enc)
                                                   (innerdec4 enc)
                                                   (innerdec5 enc)
                                                   (innerdec6 enc)
                                                   (innerdec7 enc)
                                                   (innerdec8 enc)
                                                   (innerdec9 enc)
                                                   (innerdec10 enc)
                                                   (innerdec11 enc) enc,
            genericDesc = structDesc [(fname1, innerdesc1),
                                      (fname2, innerdesc2),
                                      (fname3, innerdesc3),
                                      (fname4, innerdesc4),
                                      (fname5, innerdesc5),
                                      (fname6, innerdesc6),
                                      (fname7, innerdesc7),
                                      (fname8, innerdesc8),
                                      (fname9, innerdesc9),
                                      (fname10, innerdesc10),
                                      (fname11, innerdesc11)] }

struct12 :: (String, Generic a)
         -> (String, Generic b)
         -> (String, Generic c)
         -> (String, Generic d)
         -> (String, Generic e)
         -> (String, Generic f)
         -> (String, Generic g)
         -> (String, Generic h)
         -> (String, Generic i)
         -> (String, Generic j)
         -> (String, Generic k)
         -> (String, Generic l)
         -> Generic (a, b, c, d, e, f, g, h, i, j, k, l)
struct12 (fname1, Generic { genericEncode = innerenc1,
                            genericDecode = innerdec1,
                            genericDesc = innerdesc1 })
         (fname2, Generic { genericEncode = innerenc2,
                            genericDecode = innerdec2,
                            genericDesc = innerdesc2 })
         (fname3, Generic { genericEncode = innerenc3,
                            genericDecode = innerdec3,
                            genericDesc = innerdesc3 })
         (fname4, Generic { genericEncode = innerenc4,
                            genericDecode = innerdec4,
                            genericDesc = innerdesc4 })
         (fname5, Generic { genericEncode = innerenc5,
                            genericDecode = innerdec5,
                            genericDesc = innerdesc5 })
         (fname6, Generic { genericEncode = innerenc6,
                            genericDecode = innerdec6,
                            genericDesc = innerdesc6 })
         (fname7, Generic { genericEncode = innerenc7,
                            genericDecode = innerdec7,
                            genericDesc = innerdesc7 })
         (fname8, Generic { genericEncode = innerenc8,
                            genericDecode = innerdec8,
                            genericDesc = innerdesc8 })
         (fname9, Generic { genericEncode = innerenc9,
                            genericDecode = innerdec9,
                            genericDesc = innerdesc9 })
         (fname10, Generic { genericEncode = innerenc10,
                             genericDecode = innerdec10,
                             genericDesc = innerdesc10 })
         (fname11, Generic { genericEncode = innerenc11,
                             genericDecode = innerdec11,
                             genericDesc = innerdesc11 })
         (fname12, Generic { genericEncode = innerenc12,
                             genericDecode = innerdec12,
                             genericDesc = innerdesc12 }) =
  Generic { genericEncode = \enc -> encodeStruct12 (innerenc1 enc)
                                                   (innerenc2 enc)
                                                   (innerenc3 enc)
                                                   (innerenc4 enc)
                                                   (innerenc5 enc)
                                                   (innerenc6 enc)
                                                   (innerenc7 enc)
                                                   (innerenc8 enc)
                                                   (innerenc9 enc)
                                                   (innerenc10 enc)
                                                   (innerenc11 enc)
                                                   (innerenc12 enc) enc,
            genericDecode = \enc -> decodeStruct12 (innerdec1 enc)
                                                   (innerdec2 enc)
                                                   (innerdec3 enc)
                                                   (innerdec4 enc)
                                                   (innerdec5 enc)
                                                   (innerdec6 enc)
                                                   (innerdec7 enc)
                                                   (innerdec8 enc)
                                                   (innerdec9 enc)
                                                   (innerdec10 enc)
                                                   (innerdec11 enc)
                                                   (innerdec12 enc) enc,
            genericDesc = structDesc [(fname1, innerdesc1),
                                      (fname2, innerdesc2),
                                      (fname3, innerdesc3),
                                      (fname4, innerdesc4),
                                      (fname5, innerdesc5),
                                      (fname6, innerdesc6),
                                      (fname7, innerdesc7),
                                      (fname8, innerdesc8),
                                      (fname9, innerdesc9),
                                      (fname10, innerdesc10),
                                      (fname11, innerdesc11),
                                      (fname12, innerdesc12)] }

struct13 :: (String, Generic a)
         -> (String, Generic b)
         -> (String, Generic c)
         -> (String, Generic d)
         -> (String, Generic e)
         -> (String, Generic f)
         -> (String, Generic g)
         -> (String, Generic h)
         -> (String, Generic i)
         -> (String, Generic j)
         -> (String, Generic k)
         -> (String, Generic l)
         -> (String, Generic m)
         -> Generic (a, b, c, d, e, f, g, h, i, j, k, l, m)
struct13 (fname1, Generic { genericEncode = innerenc1,
                            genericDecode = innerdec1,
                            genericDesc = innerdesc1 })
         (fname2, Generic { genericEncode = innerenc2,
                            genericDecode = innerdec2,
                            genericDesc = innerdesc2 })
         (fname3, Generic { genericEncode = innerenc3,
                            genericDecode = innerdec3,
                            genericDesc = innerdesc3 })
         (fname4, Generic { genericEncode = innerenc4,
                            genericDecode = innerdec4,
                            genericDesc = innerdesc4 })
         (fname5, Generic { genericEncode = innerenc5,
                            genericDecode = innerdec5,
                            genericDesc = innerdesc5 })
         (fname6, Generic { genericEncode = innerenc6,
                            genericDecode = innerdec6,
                            genericDesc = innerdesc6 })
         (fname7, Generic { genericEncode = innerenc7,
                            genericDecode = innerdec7,
                            genericDesc = innerdesc7 })
         (fname8, Generic { genericEncode = innerenc8,
                            genericDecode = innerdec8,
                            genericDesc = innerdesc8 })
         (fname9, Generic { genericEncode = innerenc9,
                            genericDecode = innerdec9,
                            genericDesc = innerdesc9 })
         (fname10, Generic { genericEncode = innerenc10,
                             genericDecode = innerdec10,
                             genericDesc = innerdesc10 })
         (fname11, Generic { genericEncode = innerenc11,
                             genericDecode = innerdec11,
                             genericDesc = innerdesc11 })
         (fname12, Generic { genericEncode = innerenc12,
                             genericDecode = innerdec12,
                             genericDesc = innerdesc12 })
         (fname13, Generic { genericEncode = innerenc13,
                             genericDecode = innerdec13,
                             genericDesc = innerdesc13 }) =
  Generic { genericEncode = \enc -> encodeStruct13 (innerenc1 enc)
                                                   (innerenc2 enc)
                                                   (innerenc3 enc)
                                                   (innerenc4 enc)
                                                   (innerenc5 enc)
                                                   (innerenc6 enc)
                                                   (innerenc7 enc)
                                                   (innerenc8 enc)
                                                   (innerenc9 enc)
                                                   (innerenc10 enc)
                                                   (innerenc11 enc)
                                                   (innerenc12 enc)
                                                   (innerenc13 enc) enc,
            genericDecode = \enc -> decodeStruct13 (innerdec1 enc)
                                                   (innerdec2 enc)
                                                   (innerdec3 enc)
                                                   (innerdec4 enc)
                                                   (innerdec5 enc)
                                                   (innerdec6 enc)
                                                   (innerdec7 enc)
                                                   (innerdec8 enc)
                                                   (innerdec9 enc)
                                                   (innerdec10 enc)
                                                   (innerdec11 enc)
                                                   (innerdec12 enc)
                                                   (innerdec13 enc) enc,
            genericDesc = structDesc [(fname1, innerdesc1),
                                      (fname2, innerdesc2),
                                      (fname3, innerdesc3),
                                      (fname4, innerdesc4),
                                      (fname5, innerdesc5),
                                      (fname6, innerdesc6),
                                      (fname7, innerdesc7),
                                      (fname8, innerdesc8),
                                      (fname9, innerdesc9),
                                      (fname10, innerdesc10),
                                      (fname11, innerdesc11),
                                      (fname12, innerdesc12),
                                      (fname13, innerdesc13)] }

struct14 :: (String, Generic a)
         -> (String, Generic b)
         -> (String, Generic c)
         -> (String, Generic d)
         -> (String, Generic e)
         -> (String, Generic f)
         -> (String, Generic g)
         -> (String, Generic h)
         -> (String, Generic i)
         -> (String, Generic j)
         -> (String, Generic k)
         -> (String, Generic l)
         -> (String, Generic m)
         -> (String, Generic n)
         -> Generic (a, b, c, d, e, f, g, h, i, j, k, l, m, n)
struct14 (fname1, Generic { genericEncode = innerenc1,
                            genericDecode = innerdec1,
                            genericDesc = innerdesc1 })
         (fname2, Generic { genericEncode = innerenc2,
                            genericDecode = innerdec2,
                            genericDesc = innerdesc2 })
         (fname3, Generic { genericEncode = innerenc3,
                            genericDecode = innerdec3,
                            genericDesc = innerdesc3 })
         (fname4, Generic { genericEncode = innerenc4,
                            genericDecode = innerdec4,
                            genericDesc = innerdesc4 })
         (fname5, Generic { genericEncode = innerenc5,
                            genericDecode = innerdec5,
                            genericDesc = innerdesc5 })
         (fname6, Generic { genericEncode = innerenc6,
                            genericDecode = innerdec6,
                            genericDesc = innerdesc6 })
         (fname7, Generic { genericEncode = innerenc7,
                            genericDecode = innerdec7,
                            genericDesc = innerdesc7 })
         (fname8, Generic { genericEncode = innerenc8,
                            genericDecode = innerdec8,
                            genericDesc = innerdesc8 })
         (fname9, Generic { genericEncode = innerenc9,
                            genericDecode = innerdec9,
                            genericDesc = innerdesc9 })
         (fname10, Generic { genericEncode = innerenc10,
                             genericDecode = innerdec10,
                             genericDesc = innerdesc10 })
         (fname11, Generic { genericEncode = innerenc11,
                             genericDecode = innerdec11,
                             genericDesc = innerdesc11 })
         (fname12, Generic { genericEncode = innerenc12,
                             genericDecode = innerdec12,
                             genericDesc = innerdesc12 })
         (fname13, Generic { genericEncode = innerenc13,
                             genericDecode = innerdec13,
                             genericDesc = innerdesc13 })
         (fname14, Generic { genericEncode = innerenc14,
                             genericDecode = innerdec14,
                             genericDesc = innerdesc14 }) =
  Generic { genericEncode = \enc -> encodeStruct14 (innerenc1 enc)
                                                   (innerenc2 enc)
                                                   (innerenc3 enc)
                                                   (innerenc4 enc)
                                                   (innerenc5 enc)
                                                   (innerenc6 enc)
                                                   (innerenc7 enc)
                                                   (innerenc8 enc)
                                                   (innerenc9 enc)
                                                   (innerenc10 enc)
                                                   (innerenc11 enc)
                                                   (innerenc12 enc)
                                                   (innerenc13 enc)
                                                   (innerenc14 enc) enc,
            genericDecode = \enc -> decodeStruct14 (innerdec1 enc)
                                                   (innerdec2 enc)
                                                   (innerdec3 enc)
                                                   (innerdec4 enc)
                                                   (innerdec5 enc)
                                                   (innerdec6 enc)
                                                   (innerdec7 enc)
                                                   (innerdec8 enc)
                                                   (innerdec9 enc)
                                                   (innerdec10 enc)
                                                   (innerdec11 enc)
                                                   (innerdec12 enc)
                                                   (innerdec13 enc)
                                                   (innerdec14 enc) enc,
            genericDesc = structDesc [(fname1, innerdesc1),
                                      (fname2, innerdesc2),
                                      (fname3, innerdesc3),
                                      (fname4, innerdesc4),
                                      (fname5, innerdesc5),
                                      (fname6, innerdesc6),
                                      (fname7, innerdesc7),
                                      (fname8, innerdesc8),
                                      (fname9, innerdesc9),
                                      (fname10, innerdesc10),
                                      (fname11, innerdesc11),
                                      (fname12, innerdesc12),
                                      (fname13, innerdesc13),
                                      (fname14, innerdesc14)] }

struct15 :: (String, Generic a)
         -> (String, Generic b)
         -> (String, Generic c)
         -> (String, Generic d)
         -> (String, Generic e)
         -> (String, Generic f)
         -> (String, Generic g)
         -> (String, Generic h)
         -> (String, Generic i)
         -> (String, Generic j)
         -> (String, Generic k)
         -> (String, Generic l)
         -> (String, Generic m)
         -> (String, Generic n)
         -> (String, Generic o)
         -> Generic (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o)
struct15 (fname1, Generic { genericEncode = innerenc1,
                            genericDecode = innerdec1,
                            genericDesc = innerdesc1 })
         (fname2, Generic { genericEncode = innerenc2,
                            genericDecode = innerdec2,
                            genericDesc = innerdesc2 })
         (fname3, Generic { genericEncode = innerenc3,
                            genericDecode = innerdec3,
                            genericDesc = innerdesc3 })
         (fname4, Generic { genericEncode = innerenc4,
                            genericDecode = innerdec4,
                            genericDesc = innerdesc4 })
         (fname5, Generic { genericEncode = innerenc5,
                            genericDecode = innerdec5,
                            genericDesc = innerdesc5 })
         (fname6, Generic { genericEncode = innerenc6,
                            genericDecode = innerdec6,
                            genericDesc = innerdesc6 })
         (fname7, Generic { genericEncode = innerenc7,
                            genericDecode = innerdec7,
                            genericDesc = innerdesc7 })
         (fname8, Generic { genericEncode = innerenc8,
                            genericDecode = innerdec8,
                            genericDesc = innerdesc8 })
         (fname9, Generic { genericEncode = innerenc9,
                            genericDecode = innerdec9,
                            genericDesc = innerdesc9 })
         (fname10, Generic { genericEncode = innerenc10,
                             genericDecode = innerdec10,
                             genericDesc = innerdesc10 })
         (fname11, Generic { genericEncode = innerenc11,
                             genericDecode = innerdec11,
                             genericDesc = innerdesc11 })
         (fname12, Generic { genericEncode = innerenc12,
                             genericDecode = innerdec12,
                             genericDesc = innerdesc12 })
         (fname13, Generic { genericEncode = innerenc13,
                             genericDecode = innerdec13,
                             genericDesc = innerdesc13 })
         (fname14, Generic { genericEncode = innerenc14,
                             genericDecode = innerdec14,
                             genericDesc = innerdesc14 })
         (fname15, Generic { genericEncode = innerenc15,
                             genericDecode = innerdec15,
                             genericDesc = innerdesc15 }) =
  Generic { genericEncode = \enc -> encodeStruct15 (innerenc1 enc)
                                                   (innerenc2 enc)
                                                   (innerenc3 enc)
                                                   (innerenc4 enc)
                                                   (innerenc5 enc)
                                                   (innerenc6 enc)
                                                   (innerenc7 enc)
                                                   (innerenc8 enc)
                                                   (innerenc9 enc)
                                                   (innerenc10 enc)
                                                   (innerenc11 enc)
                                                   (innerenc12 enc)
                                                   (innerenc13 enc)
                                                   (innerenc14 enc)
                                                   (innerenc15 enc) enc,
            genericDecode = \enc -> decodeStruct15 (innerdec1 enc)
                                                   (innerdec2 enc)
                                                   (innerdec3 enc)
                                                   (innerdec4 enc)
                                                   (innerdec5 enc)
                                                   (innerdec6 enc)
                                                   (innerdec7 enc)
                                                   (innerdec8 enc)
                                                   (innerdec9 enc)
                                                   (innerdec10 enc)
                                                   (innerdec11 enc)
                                                   (innerdec12 enc)
                                                   (innerdec13 enc)
                                                   (innerdec14 enc)
                                                   (innerdec15 enc) enc,
            genericDesc = structDesc [(fname1, innerdesc1),
                                      (fname2, innerdesc2),
                                      (fname3, innerdesc3),
                                      (fname4, innerdesc4),
                                      (fname5, innerdesc5),
                                      (fname6, innerdesc6),
                                      (fname7, innerdesc7),
                                      (fname8, innerdesc8),
                                      (fname9, innerdesc9),
                                      (fname10, innerdesc10),
                                      (fname11, innerdesc11),
                                      (fname12, innerdesc12),
                                      (fname13, innerdesc13),
                                      (fname14, innerdesc14),
                                      (fname15, innerdesc15)] }

struct16 :: (String, Generic a)
         -> (String, Generic b)
         -> (String, Generic c)
         -> (String, Generic d)
         -> (String, Generic e)
         -> (String, Generic f)
         -> (String, Generic g)
         -> (String, Generic h)
         -> (String, Generic i)
         -> (String, Generic j)
         -> (String, Generic k)
         -> (String, Generic l)
         -> (String, Generic m)
         -> (String, Generic n)
         -> (String, Generic o)
         -> (String, Generic p)
         -> Generic (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p)
struct16 (fname1, Generic { genericEncode = innerenc1,
                            genericDecode = innerdec1,
                            genericDesc = innerdesc1 })
         (fname2, Generic { genericEncode = innerenc2,
                            genericDecode = innerdec2,
                            genericDesc = innerdesc2 })
         (fname3, Generic { genericEncode = innerenc3,
                            genericDecode = innerdec3,
                            genericDesc = innerdesc3 })
         (fname4, Generic { genericEncode = innerenc4,
                            genericDecode = innerdec4,
                            genericDesc = innerdesc4 })
         (fname5, Generic { genericEncode = innerenc5,
                            genericDecode = innerdec5,
                            genericDesc = innerdesc5 })
         (fname6, Generic { genericEncode = innerenc6,
                            genericDecode = innerdec6,
                            genericDesc = innerdesc6 })
         (fname7, Generic { genericEncode = innerenc7,
                            genericDecode = innerdec7,
                            genericDesc = innerdesc7 })
         (fname8, Generic { genericEncode = innerenc8,
                            genericDecode = innerdec8,
                            genericDesc = innerdesc8 })
         (fname9, Generic { genericEncode = innerenc9,
                            genericDecode = innerdec9,
                            genericDesc = innerdesc9 })
         (fname10, Generic { genericEncode = innerenc10,
                             genericDecode = innerdec10,
                             genericDesc = innerdesc10 })
         (fname11, Generic { genericEncode = innerenc11,
                             genericDecode = innerdec11,
                             genericDesc = innerdesc11 })
         (fname12, Generic { genericEncode = innerenc12,
                             genericDecode = innerdec12,
                             genericDesc = innerdesc12 })
         (fname13, Generic { genericEncode = innerenc13,
                             genericDecode = innerdec13,
                             genericDesc = innerdesc13 })
         (fname14, Generic { genericEncode = innerenc14,
                             genericDecode = innerdec14,
                             genericDesc = innerdesc14 })
         (fname15, Generic { genericEncode = innerenc15,
                             genericDecode = innerdec15,
                             genericDesc = innerdesc15 })
         (fname16, Generic { genericEncode = innerenc16,
                             genericDecode = innerdec16,
                             genericDesc = innerdesc16 }) =
  Generic { genericEncode = \enc -> encodeStruct16 (innerenc1 enc)
                                                   (innerenc2 enc)
                                                   (innerenc3 enc)
                                                   (innerenc4 enc)
                                                   (innerenc5 enc)
                                                   (innerenc6 enc)
                                                   (innerenc7 enc)
                                                   (innerenc8 enc)
                                                   (innerenc9 enc)
                                                   (innerenc10 enc)
                                                   (innerenc11 enc)
                                                   (innerenc12 enc)
                                                   (innerenc13 enc)
                                                   (innerenc14 enc)
                                                   (innerenc15 enc)
                                                   (innerenc16 enc) enc,
            genericDecode = \enc -> decodeStruct16 (innerdec1 enc)
                                                   (innerdec2 enc)
                                                   (innerdec3 enc)
                                                   (innerdec4 enc)
                                                   (innerdec5 enc)
                                                   (innerdec6 enc)
                                                   (innerdec7 enc)
                                                   (innerdec8 enc)
                                                   (innerdec9 enc)
                                                   (innerdec10 enc)
                                                   (innerdec11 enc)
                                                   (innerdec12 enc)
                                                   (innerdec13 enc)
                                                   (innerdec14 enc)
                                                   (innerdec15 enc)
                                                   (innerdec16 enc) enc,
            genericDesc = structDesc [(fname1, innerdesc1),
                                      (fname2, innerdesc2),
                                      (fname3, innerdesc3),
                                      (fname4, innerdesc4),
                                      (fname5, innerdesc5),
                                      (fname6, innerdesc6),
                                      (fname7, innerdesc7),
                                      (fname8, innerdesc8),
                                      (fname9, innerdesc9),
                                      (fname10, innerdesc10),
                                      (fname11, innerdesc11),
                                      (fname12, innerdesc12),
                                      (fname13, innerdesc13),
                                      (fname14, innerdesc14),
                                      (fname15, innerdesc15),
                                      (fname16, innerdesc16)] }

struct17 :: (String, Generic a)
         -> (String, Generic b)
         -> (String, Generic c)
         -> (String, Generic d)
         -> (String, Generic e)
         -> (String, Generic f)
         -> (String, Generic g)
         -> (String, Generic h)
         -> (String, Generic i)
         -> (String, Generic j)
         -> (String, Generic k)
         -> (String, Generic l)
         -> (String, Generic m)
         -> (String, Generic n)
         -> (String, Generic o)
         -> (String, Generic p)
         -> (String, Generic q)
         -> Generic (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q)
struct17 (fname1, Generic { genericEncode = innerenc1,
                            genericDecode = innerdec1,
                            genericDesc = innerdesc1 })
         (fname2, Generic { genericEncode = innerenc2,
                            genericDecode = innerdec2,
                            genericDesc = innerdesc2 })
         (fname3, Generic { genericEncode = innerenc3,
                            genericDecode = innerdec3,
                            genericDesc = innerdesc3 })
         (fname4, Generic { genericEncode = innerenc4,
                            genericDecode = innerdec4,
                            genericDesc = innerdesc4 })
         (fname5, Generic { genericEncode = innerenc5,
                            genericDecode = innerdec5,
                            genericDesc = innerdesc5 })
         (fname6, Generic { genericEncode = innerenc6,
                            genericDecode = innerdec6,
                            genericDesc = innerdesc6 })
         (fname7, Generic { genericEncode = innerenc7,
                            genericDecode = innerdec7,
                            genericDesc = innerdesc7 })
         (fname8, Generic { genericEncode = innerenc8,
                            genericDecode = innerdec8,
                            genericDesc = innerdesc8 })
         (fname9, Generic { genericEncode = innerenc9,
                            genericDecode = innerdec9,
                            genericDesc = innerdesc9 })
         (fname10, Generic { genericEncode = innerenc10,
                             genericDecode = innerdec10,
                             genericDesc = innerdesc10 })
         (fname11, Generic { genericEncode = innerenc11,
                             genericDecode = innerdec11,
                             genericDesc = innerdesc11 })
         (fname12, Generic { genericEncode = innerenc12,
                             genericDecode = innerdec12,
                             genericDesc = innerdesc12 })
         (fname13, Generic { genericEncode = innerenc13,
                             genericDecode = innerdec13,
                             genericDesc = innerdesc13 })
         (fname14, Generic { genericEncode = innerenc14,
                             genericDecode = innerdec14,
                             genericDesc = innerdesc14 })
         (fname15, Generic { genericEncode = innerenc15,
                             genericDecode = innerdec15,
                             genericDesc = innerdesc15 })
         (fname16, Generic { genericEncode = innerenc16,
                             genericDecode = innerdec16,
                             genericDesc = innerdesc16 })
         (fname17, Generic { genericEncode = innerenc17,
                             genericDecode = innerdec17,
                             genericDesc = innerdesc17 }) =
  Generic { genericEncode = \enc -> encodeStruct17 (innerenc1 enc)
                                                   (innerenc2 enc)
                                                   (innerenc3 enc)
                                                   (innerenc4 enc)
                                                   (innerenc5 enc)
                                                   (innerenc6 enc)
                                                   (innerenc7 enc)
                                                   (innerenc8 enc)
                                                   (innerenc9 enc)
                                                   (innerenc10 enc)
                                                   (innerenc11 enc)
                                                   (innerenc12 enc)
                                                   (innerenc13 enc)
                                                   (innerenc14 enc)
                                                   (innerenc15 enc)
                                                   (innerenc16 enc)
                                                   (innerenc17 enc) enc,
            genericDecode = \enc -> decodeStruct17 (innerdec1 enc)
                                                   (innerdec2 enc)
                                                   (innerdec3 enc)
                                                   (innerdec4 enc)
                                                   (innerdec5 enc)
                                                   (innerdec6 enc)
                                                   (innerdec7 enc)
                                                   (innerdec8 enc)
                                                   (innerdec9 enc)
                                                   (innerdec10 enc)
                                                   (innerdec11 enc)
                                                   (innerdec12 enc)
                                                   (innerdec13 enc)
                                                   (innerdec14 enc)
                                                   (innerdec15 enc)
                                                   (innerdec16 enc)
                                                   (innerdec17 enc) enc,
            genericDesc = structDesc [(fname1, innerdesc1),
                                      (fname2, innerdesc2),
                                      (fname3, innerdesc3),
                                      (fname4, innerdesc4),
                                      (fname5, innerdesc5),
                                      (fname6, innerdesc6),
                                      (fname7, innerdesc7),
                                      (fname8, innerdesc8),
                                      (fname9, innerdesc9),
                                      (fname10, innerdesc10),
                                      (fname11, innerdesc11),
                                      (fname12, innerdesc12),
                                      (fname13, innerdesc13),
                                      (fname14, innerdesc14),
                                      (fname15, innerdesc15),
                                      (fname16, innerdesc16),
                                      (fname17, innerdesc17)] }

struct18 :: (String, Generic a)
         -> (String, Generic b)
         -> (String, Generic c)
         -> (String, Generic d)
         -> (String, Generic e)
         -> (String, Generic f)
         -> (String, Generic g)
         -> (String, Generic h)
         -> (String, Generic i)
         -> (String, Generic j)
         -> (String, Generic k)
         -> (String, Generic l)
         -> (String, Generic m)
         -> (String, Generic n)
         -> (String, Generic o)
         -> (String, Generic p)
         -> (String, Generic q)
         -> (String, Generic r)
         -> Generic (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r)
struct18 (fname1, Generic { genericEncode = innerenc1,
                            genericDecode = innerdec1,
                            genericDesc = innerdesc1 })
         (fname2, Generic { genericEncode = innerenc2,
                            genericDecode = innerdec2,
                            genericDesc = innerdesc2 })
         (fname3, Generic { genericEncode = innerenc3,
                            genericDecode = innerdec3,
                            genericDesc = innerdesc3 })
         (fname4, Generic { genericEncode = innerenc4,
                            genericDecode = innerdec4,
                            genericDesc = innerdesc4 })
         (fname5, Generic { genericEncode = innerenc5,
                            genericDecode = innerdec5,
                            genericDesc = innerdesc5 })
         (fname6, Generic { genericEncode = innerenc6,
                            genericDecode = innerdec6,
                            genericDesc = innerdesc6 })
         (fname7, Generic { genericEncode = innerenc7,
                            genericDecode = innerdec7,
                            genericDesc = innerdesc7 })
         (fname8, Generic { genericEncode = innerenc8,
                            genericDecode = innerdec8,
                            genericDesc = innerdesc8 })
         (fname9, Generic { genericEncode = innerenc9,
                            genericDecode = innerdec9,
                            genericDesc = innerdesc9 })
         (fname10, Generic { genericEncode = innerenc10,
                             genericDecode = innerdec10,
                             genericDesc = innerdesc10 })
         (fname11, Generic { genericEncode = innerenc11,
                             genericDecode = innerdec11,
                             genericDesc = innerdesc11 })
         (fname12, Generic { genericEncode = innerenc12,
                             genericDecode = innerdec12,
                             genericDesc = innerdesc12 })
         (fname13, Generic { genericEncode = innerenc13,
                             genericDecode = innerdec13,
                             genericDesc = innerdesc13 })
         (fname14, Generic { genericEncode = innerenc14,
                             genericDecode = innerdec14,
                             genericDesc = innerdesc14 })
         (fname15, Generic { genericEncode = innerenc15,
                             genericDecode = innerdec15,
                             genericDesc = innerdesc15 })
         (fname16, Generic { genericEncode = innerenc16,
                             genericDecode = innerdec16,
                             genericDesc = innerdesc16 })
         (fname17, Generic { genericEncode = innerenc17,
                             genericDecode = innerdec17,
                             genericDesc = innerdesc17 })
         (fname18, Generic { genericEncode = innerenc18,
                             genericDecode = innerdec18,
                             genericDesc = innerdesc18 }) =
  Generic { genericEncode = \enc -> encodeStruct18 (innerenc1 enc)
                                                   (innerenc2 enc)
                                                   (innerenc3 enc)
                                                   (innerenc4 enc)
                                                   (innerenc5 enc)
                                                   (innerenc6 enc)
                                                   (innerenc7 enc)
                                                   (innerenc8 enc)
                                                   (innerenc9 enc)
                                                   (innerenc10 enc)
                                                   (innerenc11 enc)
                                                   (innerenc12 enc)
                                                   (innerenc13 enc)
                                                   (innerenc14 enc)
                                                   (innerenc15 enc)
                                                   (innerenc16 enc)
                                                   (innerenc17 enc)
                                                   (innerenc18 enc) enc,
            genericDecode = \enc -> decodeStruct18 (innerdec1 enc)
                                                   (innerdec2 enc)
                                                   (innerdec3 enc)
                                                   (innerdec4 enc)
                                                   (innerdec5 enc)
                                                   (innerdec6 enc)
                                                   (innerdec7 enc)
                                                   (innerdec8 enc)
                                                   (innerdec9 enc)
                                                   (innerdec10 enc)
                                                   (innerdec11 enc)
                                                   (innerdec12 enc)
                                                   (innerdec13 enc)
                                                   (innerdec14 enc)
                                                   (innerdec15 enc)
                                                   (innerdec16 enc)
                                                   (innerdec17 enc)
                                                   (innerdec18 enc) enc,
            genericDesc = structDesc [(fname1, innerdesc1),
                                      (fname2, innerdesc2),
                                      (fname3, innerdesc3),
                                      (fname4, innerdesc4),
                                      (fname5, innerdesc5),
                                      (fname6, innerdesc6),
                                      (fname7, innerdesc7),
                                      (fname8, innerdesc8),
                                      (fname9, innerdesc9),
                                      (fname10, innerdesc10),
                                      (fname11, innerdesc11),
                                      (fname12, innerdesc12),
                                      (fname13, innerdesc13),
                                      (fname14, innerdesc14),
                                      (fname15, innerdesc15),
                                      (fname16, innerdesc16),
                                      (fname17, innerdesc17),
                                      (fname18, innerdesc18)] }

struct19 :: (String, Generic a)
         -> (String, Generic b)
         -> (String, Generic c)
         -> (String, Generic d)
         -> (String, Generic e)
         -> (String, Generic f)
         -> (String, Generic g)
         -> (String, Generic h)
         -> (String, Generic i)
         -> (String, Generic j)
         -> (String, Generic k)
         -> (String, Generic l)
         -> (String, Generic m)
         -> (String, Generic n)
         -> (String, Generic o)
         -> (String, Generic p)
         -> (String, Generic q)
         -> (String, Generic r)
         -> (String, Generic s)
         -> Generic (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s)
struct19 (fname1, Generic { genericEncode = innerenc1,
                            genericDecode = innerdec1,
                            genericDesc = innerdesc1 })
         (fname2, Generic { genericEncode = innerenc2,
                            genericDecode = innerdec2,
                            genericDesc = innerdesc2 })
         (fname3, Generic { genericEncode = innerenc3,
                            genericDecode = innerdec3,
                            genericDesc = innerdesc3 })
         (fname4, Generic { genericEncode = innerenc4,
                            genericDecode = innerdec4,
                            genericDesc = innerdesc4 })
         (fname5, Generic { genericEncode = innerenc5,
                            genericDecode = innerdec5,
                            genericDesc = innerdesc5 })
         (fname6, Generic { genericEncode = innerenc6,
                            genericDecode = innerdec6,
                            genericDesc = innerdesc6 })
         (fname7, Generic { genericEncode = innerenc7,
                            genericDecode = innerdec7,
                            genericDesc = innerdesc7 })
         (fname8, Generic { genericEncode = innerenc8,
                            genericDecode = innerdec8,
                            genericDesc = innerdesc8 })
         (fname9, Generic { genericEncode = innerenc9,
                            genericDecode = innerdec9,
                            genericDesc = innerdesc9 })
         (fname10, Generic { genericEncode = innerenc10,
                             genericDecode = innerdec10,
                             genericDesc = innerdesc10 })
         (fname11, Generic { genericEncode = innerenc11,
                             genericDecode = innerdec11,
                             genericDesc = innerdesc11 })
         (fname12, Generic { genericEncode = innerenc12,
                             genericDecode = innerdec12,
                             genericDesc = innerdesc12 })
         (fname13, Generic { genericEncode = innerenc13,
                             genericDecode = innerdec13,
                             genericDesc = innerdesc13 })
         (fname14, Generic { genericEncode = innerenc14,
                             genericDecode = innerdec14,
                             genericDesc = innerdesc14 })
         (fname15, Generic { genericEncode = innerenc15,
                             genericDecode = innerdec15,
                             genericDesc = innerdesc15 })
         (fname16, Generic { genericEncode = innerenc16,
                             genericDecode = innerdec16,
                             genericDesc = innerdesc16 })
         (fname17, Generic { genericEncode = innerenc17,
                             genericDecode = innerdec17,
                             genericDesc = innerdesc17 })
         (fname18, Generic { genericEncode = innerenc18,
                             genericDecode = innerdec18,
                             genericDesc = innerdesc18 })
         (fname19, Generic { genericEncode = innerenc19,
                             genericDecode = innerdec19,
                             genericDesc = innerdesc19 }) =
  Generic { genericEncode = \enc -> encodeStruct19 (innerenc1 enc)
                                                   (innerenc2 enc)
                                                   (innerenc3 enc)
                                                   (innerenc4 enc)
                                                   (innerenc5 enc)
                                                   (innerenc6 enc)
                                                   (innerenc7 enc)
                                                   (innerenc8 enc)
                                                   (innerenc9 enc)
                                                   (innerenc10 enc)
                                                   (innerenc11 enc)
                                                   (innerenc12 enc)
                                                   (innerenc13 enc)
                                                   (innerenc14 enc)
                                                   (innerenc15 enc)
                                                   (innerenc16 enc)
                                                   (innerenc17 enc)
                                                   (innerenc18 enc)
                                                   (innerenc19 enc) enc,
            genericDecode = \enc -> decodeStruct19 (innerdec1 enc)
                                                   (innerdec2 enc)
                                                   (innerdec3 enc)
                                                   (innerdec4 enc)
                                                   (innerdec5 enc)
                                                   (innerdec6 enc)
                                                   (innerdec7 enc)
                                                   (innerdec8 enc)
                                                   (innerdec9 enc)
                                                   (innerdec10 enc)
                                                   (innerdec11 enc)
                                                   (innerdec12 enc)
                                                   (innerdec13 enc)
                                                   (innerdec14 enc)
                                                   (innerdec15 enc)
                                                   (innerdec16 enc)
                                                   (innerdec17 enc)
                                                   (innerdec18 enc)
                                                   (innerdec19 enc) enc,
            genericDesc = structDesc [(fname1, innerdesc1),
                                      (fname2, innerdesc2),
                                      (fname3, innerdesc3),
                                      (fname4, innerdesc4),
                                      (fname5, innerdesc5),
                                      (fname6, innerdesc6),
                                      (fname7, innerdesc7),
                                      (fname8, innerdesc8),
                                      (fname9, innerdesc9),
                                      (fname10, innerdesc10),
                                      (fname11, innerdesc11),
                                      (fname12, innerdesc12),
                                      (fname13, innerdesc13),
                                      (fname14, innerdesc14),
                                      (fname15, innerdesc15),
                                      (fname16, innerdesc16),
                                      (fname17, innerdesc17),
                                      (fname18, innerdesc18),
                                      (fname19, innerdesc19)] }

struct20 :: (String, Generic a)
         -> (String, Generic b)
         -> (String, Generic c)
         -> (String, Generic d)
         -> (String, Generic e)
         -> (String, Generic f)
         -> (String, Generic g)
         -> (String, Generic h)
         -> (String, Generic i)
         -> (String, Generic j)
         -> (String, Generic k)
         -> (String, Generic l)
         -> (String, Generic m)
         -> (String, Generic n)
         -> (String, Generic o)
         -> (String, Generic p)
         -> (String, Generic q)
         -> (String, Generic r)
         -> (String, Generic s)
         -> (String, Generic t)
         -> Generic (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t)
struct20 (fname1, Generic { genericEncode = innerenc1,
                            genericDecode = innerdec1,
                            genericDesc = innerdesc1 })
         (fname2, Generic { genericEncode = innerenc2,
                            genericDecode = innerdec2,
                            genericDesc = innerdesc2 })
         (fname3, Generic { genericEncode = innerenc3,
                            genericDecode = innerdec3,
                            genericDesc = innerdesc3 })
         (fname4, Generic { genericEncode = innerenc4,
                            genericDecode = innerdec4,
                            genericDesc = innerdesc4 })
         (fname5, Generic { genericEncode = innerenc5,
                            genericDecode = innerdec5,
                            genericDesc = innerdesc5 })
         (fname6, Generic { genericEncode = innerenc6,
                            genericDecode = innerdec6,
                            genericDesc = innerdesc6 })
         (fname7, Generic { genericEncode = innerenc7,
                            genericDecode = innerdec7,
                            genericDesc = innerdesc7 })
         (fname8, Generic { genericEncode = innerenc8,
                            genericDecode = innerdec8,
                            genericDesc = innerdesc8 })
         (fname9, Generic { genericEncode = innerenc9,
                            genericDecode = innerdec9,
                            genericDesc = innerdesc9 })
         (fname10, Generic { genericEncode = innerenc10,
                             genericDecode = innerdec10,
                             genericDesc = innerdesc10 })
         (fname11, Generic { genericEncode = innerenc11,
                             genericDecode = innerdec11,
                             genericDesc = innerdesc11 })
         (fname12, Generic { genericEncode = innerenc12,
                             genericDecode = innerdec12,
                             genericDesc = innerdesc12 })
         (fname13, Generic { genericEncode = innerenc13,
                             genericDecode = innerdec13,
                             genericDesc = innerdesc13 })
         (fname14, Generic { genericEncode = innerenc14,
                             genericDecode = innerdec14,
                             genericDesc = innerdesc14 })
         (fname15, Generic { genericEncode = innerenc15,
                             genericDecode = innerdec15,
                             genericDesc = innerdesc15 })
         (fname16, Generic { genericEncode = innerenc16,
                             genericDecode = innerdec16,
                             genericDesc = innerdesc16 })
         (fname17, Generic { genericEncode = innerenc17,
                             genericDecode = innerdec17,
                             genericDesc = innerdesc17 })
         (fname18, Generic { genericEncode = innerenc18,
                             genericDecode = innerdec18,
                             genericDesc = innerdesc18 })
         (fname19, Generic { genericEncode = innerenc19,
                             genericDecode = innerdec19,
                             genericDesc = innerdesc19 })
         (fname20, Generic { genericEncode = innerenc20,
                             genericDecode = innerdec20,
                             genericDesc = innerdesc20 }) =
  Generic { genericEncode = \enc -> encodeStruct20 (innerenc1 enc)
                                                   (innerenc2 enc)
                                                   (innerenc3 enc)
                                                   (innerenc4 enc)
                                                   (innerenc5 enc)
                                                   (innerenc6 enc)
                                                   (innerenc7 enc)
                                                   (innerenc8 enc)
                                                   (innerenc9 enc)
                                                   (innerenc10 enc)
                                                   (innerenc11 enc)
                                                   (innerenc12 enc)
                                                   (innerenc13 enc)
                                                   (innerenc14 enc)
                                                   (innerenc15 enc)
                                                   (innerenc16 enc)
                                                   (innerenc17 enc)
                                                   (innerenc18 enc)
                                                   (innerenc19 enc)
                                                   (innerenc20 enc) enc,
            genericDecode = \enc -> decodeStruct20 (innerdec1 enc)
                                                   (innerdec2 enc)
                                                   (innerdec3 enc)
                                                   (innerdec4 enc)
                                                   (innerdec5 enc)
                                                   (innerdec6 enc)
                                                   (innerdec7 enc)
                                                   (innerdec8 enc)
                                                   (innerdec9 enc)
                                                   (innerdec10 enc)
                                                   (innerdec11 enc)
                                                   (innerdec12 enc)
                                                   (innerdec13 enc)
                                                   (innerdec14 enc)
                                                   (innerdec15 enc)
                                                   (innerdec16 enc)
                                                   (innerdec17 enc)
                                                   (innerdec18 enc)
                                                   (innerdec19 enc)
                                                   (innerdec20 enc) enc,
            genericDesc = structDesc [(fname1, innerdesc1),
                                      (fname2, innerdesc2),
                                      (fname3, innerdesc3),
                                      (fname4, innerdesc4),
                                      (fname5, innerdesc5),
                                      (fname6, innerdesc6),
                                      (fname7, innerdesc7),
                                      (fname8, innerdesc8),
                                      (fname9, innerdesc9),
                                      (fname10, innerdesc10),
                                      (fname11, innerdesc11),
                                      (fname12, innerdesc12),
                                      (fname13, innerdesc13),
                                      (fname14, innerdesc14),
                                      (fname15, innerdesc15),
                                      (fname16, innerdesc16),
                                      (fname17, innerdesc17),
                                      (fname18, innerdesc18),
                                      (fname19, innerdesc19),
                                      (fname20, innerdesc20)] }

set2 :: (String, Generic a)
     -> (String, Generic b)
     -> Generic (a, b)
set2 (fname1, Generic { genericEncode = innerenc1,
                        genericDecode = innerdec1,
                        genericDesc = innerdesc1 })
     (fname2, Generic { genericEncode = innerenc2,
                        genericDecode = innerdec2,
                        genericDesc = innerdesc2 }) =
  Generic { genericEncode = \enc -> encodeSet2 (innerenc1 enc)
                                               (innerenc2 enc) enc,
            genericDecode = \enc -> decodeSet2 (innerdec1 enc)
                                               (innerdec2 enc)enc,
            genericDesc = setDesc [(fname1, innerdesc1),
                                   (fname2, innerdesc2)] }

set3 :: (String, Generic a)
     -> (String, Generic b)
     -> (String, Generic c)
     -> Generic (a, b, c)
set3 (fname1, Generic { genericEncode = innerenc1,
                        genericDecode = innerdec1,
                        genericDesc = innerdesc1 })
     (fname2, Generic { genericEncode = innerenc2,
                        genericDecode = innerdec2,
                        genericDesc = innerdesc2 })
     (fname3, Generic { genericEncode = innerenc3,
                        genericDecode = innerdec3,
                        genericDesc = innerdesc3 }) =
  Generic { genericEncode = \enc -> encodeSet3 (innerenc1 enc)
                                               (innerenc2 enc)
                                               (innerenc3 enc) enc,
            genericDecode = \enc -> decodeSet3 (innerdec1 enc)
                                               (innerdec2 enc)
                                               (innerdec3 enc) enc,
            genericDesc = setDesc [(fname1, innerdesc1),
                                   (fname2, innerdesc2),
                                   (fname3, innerdesc3)] }

set4 :: (String, Generic a)
     -> (String, Generic b)
     -> (String, Generic c)
     -> (String, Generic d)
     -> Generic (a, b, c, d)
set4 (fname1, Generic { genericEncode = innerenc1,
                        genericDecode = innerdec1,
                        genericDesc = innerdesc1 })
     (fname2, Generic { genericEncode = innerenc2,
                        genericDecode = innerdec2,
                        genericDesc = innerdesc2 })
     (fname3, Generic { genericEncode = innerenc3,
                        genericDecode = innerdec3,
                        genericDesc = innerdesc3 })
     (fname4, Generic { genericEncode = innerenc4,
                        genericDecode = innerdec4,
                        genericDesc = innerdesc4 }) =
  Generic { genericEncode = \enc -> encodeSet4 (innerenc1 enc)
                                               (innerenc2 enc)
                                               (innerenc3 enc)
                                               (innerenc4 enc) enc,
            genericDecode = \enc -> decodeSet4 (innerdec1 enc)
                                               (innerdec2 enc)
                                               (innerdec3 enc)
                                               (innerdec4 enc) enc,
            genericDesc = setDesc [(fname1, innerdesc1),
                                   (fname2, innerdesc2),
                                   (fname3, innerdesc3),
                                   (fname4, innerdesc4)] }

set5 :: (String, Generic a)
     -> (String, Generic b)
     -> (String, Generic c)
     -> (String, Generic d)
     -> (String, Generic e)
     -> Generic (a, b, c, d, e)
set5 (fname1, Generic { genericEncode = innerenc1,
                        genericDecode = innerdec1,
                        genericDesc = innerdesc1 })
     (fname2, Generic { genericEncode = innerenc2,
                        genericDecode = innerdec2,
                        genericDesc = innerdesc2 })
     (fname3, Generic { genericEncode = innerenc3,
                        genericDecode = innerdec3,
                        genericDesc = innerdesc3 })
     (fname4, Generic { genericEncode = innerenc4,
                        genericDecode = innerdec4,
                        genericDesc = innerdesc4 })
     (fname5, Generic { genericEncode = innerenc5,
                        genericDecode = innerdec5,
                        genericDesc = innerdesc5 }) =
  Generic { genericEncode = \enc -> encodeSet5 (innerenc1 enc)
                                               (innerenc2 enc)
                                               (innerenc3 enc)
                                               (innerenc4 enc)
                                               (innerenc5 enc) enc,
            genericDecode = \enc -> decodeSet5 (innerdec1 enc)
                                               (innerdec2 enc)
                                               (innerdec3 enc)
                                               (innerdec4 enc)
                                               (innerdec5 enc) enc,
            genericDesc = setDesc [(fname1, innerdesc1),
                                   (fname2, innerdesc2),
                                   (fname3, innerdesc3),
                                   (fname4, innerdesc4),
                                   (fname5, innerdesc5)] }

set6 :: (String, Generic a)
     -> (String, Generic b)
     -> (String, Generic c)
     -> (String, Generic d)
     -> (String, Generic e)
     -> (String, Generic f)
     -> Generic (a, b, c, d, e, f)
set6 (fname1, Generic { genericEncode = innerenc1,
                        genericDecode = innerdec1,
                        genericDesc = innerdesc1 })
     (fname2, Generic { genericEncode = innerenc2,
                        genericDecode = innerdec2,
                        genericDesc = innerdesc2 })
     (fname3, Generic { genericEncode = innerenc3,
                        genericDecode = innerdec3,
                        genericDesc = innerdesc3 })
     (fname4, Generic { genericEncode = innerenc4,
                        genericDecode = innerdec4,
                        genericDesc = innerdesc4 })
     (fname5, Generic { genericEncode = innerenc5,
                        genericDecode = innerdec5,
                        genericDesc = innerdesc5 })
     (fname6, Generic { genericEncode = innerenc6,
                        genericDecode = innerdec6,
                        genericDesc = innerdesc6 }) =
  Generic { genericEncode = \enc -> encodeSet6 (innerenc1 enc)
                                               (innerenc2 enc)
                                               (innerenc3 enc)
                                               (innerenc4 enc)
                                               (innerenc5 enc)
                                               (innerenc6 enc) enc,
            genericDecode = \enc -> decodeSet6 (innerdec1 enc)
                                               (innerdec2 enc)
                                               (innerdec3 enc)
                                               (innerdec4 enc)
                                               (innerdec5 enc)
                                               (innerdec6 enc) enc,
            genericDesc = setDesc [(fname1, innerdesc1),
                                   (fname2, innerdesc2),
                                   (fname3, innerdesc3),
                                   (fname4, innerdesc4),
                                   (fname5, innerdesc5),
                                   (fname6, innerdesc6)] }

set7 :: (String, Generic a)
     -> (String, Generic b)
     -> (String, Generic c)
     -> (String, Generic d)
     -> (String, Generic e)
     -> (String, Generic f)
     -> (String, Generic g)
     -> Generic (a, b, c, d, e, f, g)
set7 (fname1, Generic { genericEncode = innerenc1,
                        genericDecode = innerdec1,
                        genericDesc = innerdesc1 })
     (fname2, Generic { genericEncode = innerenc2,
                        genericDecode = innerdec2,
                        genericDesc = innerdesc2 })
     (fname3, Generic { genericEncode = innerenc3,
                        genericDecode = innerdec3,
                        genericDesc = innerdesc3 })
     (fname4, Generic { genericEncode = innerenc4,
                        genericDecode = innerdec4,
                        genericDesc = innerdesc4 })
     (fname5, Generic { genericEncode = innerenc5,
                        genericDecode = innerdec5,
                        genericDesc = innerdesc5 })
     (fname6, Generic { genericEncode = innerenc6,
                        genericDecode = innerdec6,
                        genericDesc = innerdesc6 })
     (fname7, Generic { genericEncode = innerenc7,
                        genericDecode = innerdec7,
                        genericDesc = innerdesc7 }) =
  Generic { genericEncode = \enc -> encodeSet7 (innerenc1 enc)
                                               (innerenc2 enc)
                                               (innerenc3 enc)
                                               (innerenc4 enc)
                                               (innerenc5 enc)
                                               (innerenc6 enc)
                                               (innerenc7 enc) enc,
            genericDecode = \enc -> decodeSet7 (innerdec1 enc)
                                               (innerdec2 enc)
                                               (innerdec3 enc)
                                               (innerdec4 enc)
                                               (innerdec5 enc)
                                               (innerdec6 enc)
                                               (innerdec7 enc) enc,
            genericDesc = setDesc [(fname1, innerdesc1),
                                   (fname2, innerdesc2),
                                   (fname3, innerdesc3),
                                   (fname4, innerdesc4),
                                   (fname5, innerdesc5),
                                   (fname6, innerdesc6),
                                   (fname7, innerdesc7)] }

set8 :: (String, Generic a)
     -> (String, Generic b)
     -> (String, Generic c)
     -> (String, Generic d)
     -> (String, Generic e)
     -> (String, Generic f)
     -> (String, Generic g)
     -> (String, Generic h)
     -> Generic (a, b, c, d, e, f, g, h)
set8 (fname1, Generic { genericEncode = innerenc1,
                        genericDecode = innerdec1,
                        genericDesc = innerdesc1 })
     (fname2, Generic { genericEncode = innerenc2,
                        genericDecode = innerdec2,
                        genericDesc = innerdesc2 })
     (fname3, Generic { genericEncode = innerenc3,
                        genericDecode = innerdec3,
                        genericDesc = innerdesc3 })
     (fname4, Generic { genericEncode = innerenc4,
                        genericDecode = innerdec4,
                        genericDesc = innerdesc4 })
     (fname5, Generic { genericEncode = innerenc5,
                        genericDecode = innerdec5,
                        genericDesc = innerdesc5 })
     (fname6, Generic { genericEncode = innerenc6,
                        genericDecode = innerdec6,
                        genericDesc = innerdesc6 })
     (fname7, Generic { genericEncode = innerenc7,
                        genericDecode = innerdec7,
                        genericDesc = innerdesc7 })
     (fname8, Generic { genericEncode = innerenc8,
                        genericDecode = innerdec8,
                        genericDesc = innerdesc8 }) =
  Generic { genericEncode = \enc -> encodeSet8 (innerenc1 enc)
                                               (innerenc2 enc)
                                               (innerenc3 enc)
                                               (innerenc4 enc)
                                               (innerenc5 enc)
                                               (innerenc6 enc)
                                               (innerenc7 enc)
                                               (innerenc8 enc) enc,
            genericDecode = \enc -> decodeSet8 (innerdec1 enc)
                                               (innerdec2 enc)
                                               (innerdec3 enc)
                                               (innerdec4 enc)
                                               (innerdec5 enc)
                                               (innerdec6 enc)
                                               (innerdec7 enc)
                                               (innerdec8 enc) enc,
            genericDesc = setDesc [(fname1, innerdesc1),
                                   (fname2, innerdesc2),
                                   (fname3, innerdesc3),
                                   (fname4, innerdesc4),
                                   (fname5, innerdesc5),
                                   (fname6, innerdesc6),
                                   (fname7, innerdesc7),
                                   (fname8, innerdesc8)] }

set9 :: (String, Generic a)
     -> (String, Generic b)
     -> (String, Generic c)
     -> (String, Generic d)
     -> (String, Generic e)
     -> (String, Generic f)
     -> (String, Generic g)
     -> (String, Generic h)
     -> (String, Generic i)
     -> Generic (a, b, c, d, e, f, g, h, i)
set9 (fname1, Generic { genericEncode = innerenc1,
                        genericDecode = innerdec1,
                        genericDesc = innerdesc1 })
     (fname2, Generic { genericEncode = innerenc2,
                        genericDecode = innerdec2,
                        genericDesc = innerdesc2 })
     (fname3, Generic { genericEncode = innerenc3,
                        genericDecode = innerdec3,
                        genericDesc = innerdesc3 })
     (fname4, Generic { genericEncode = innerenc4,
                        genericDecode = innerdec4,
                        genericDesc = innerdesc4 })
     (fname5, Generic { genericEncode = innerenc5,
                        genericDecode = innerdec5,
                        genericDesc = innerdesc5 })
     (fname6, Generic { genericEncode = innerenc6,
                        genericDecode = innerdec6,
                        genericDesc = innerdesc6 })
     (fname7, Generic { genericEncode = innerenc7,
                        genericDecode = innerdec7,
                        genericDesc = innerdesc7 })
     (fname8, Generic { genericEncode = innerenc8,
                        genericDecode = innerdec8,
                        genericDesc = innerdesc8 })
     (fname9, Generic { genericEncode = innerenc9,
                        genericDecode = innerdec9,
                        genericDesc = innerdesc9 }) =
  Generic { genericEncode = \enc -> encodeSet9 (innerenc1 enc)
                                               (innerenc2 enc)
                                               (innerenc3 enc)
                                               (innerenc4 enc)
                                               (innerenc5 enc)
                                               (innerenc6 enc)
                                               (innerenc7 enc)
                                               (innerenc8 enc)
                                               (innerenc9 enc) enc,
            genericDecode = \enc -> decodeSet9 (innerdec1 enc)
                                               (innerdec2 enc)
                                               (innerdec3 enc)
                                               (innerdec4 enc)
                                               (innerdec5 enc)
                                               (innerdec6 enc)
                                               (innerdec7 enc)
                                               (innerdec8 enc)
                                               (innerdec9 enc) enc,
            genericDesc = setDesc [(fname1, innerdesc1),
                                   (fname2, innerdesc2),
                                   (fname3, innerdesc3),
                                   (fname4, innerdesc4),
                                   (fname5, innerdesc5),
                                   (fname6, innerdesc6),
                                   (fname7, innerdesc7),
                                   (fname8, innerdesc8),
                                   (fname9, innerdesc9)] }

set10 :: (String, Generic a)
      -> (String, Generic b)
      -> (String, Generic c)
      -> (String, Generic d)
      -> (String, Generic e)
      -> (String, Generic f)
      -> (String, Generic g)
      -> (String, Generic h)
      -> (String, Generic i)
      -> (String, Generic j)
      -> Generic (a, b, c, d, e, f, g, h, i, j)
set10 (fname1, Generic { genericEncode = innerenc1,
                         genericDecode = innerdec1,
                         genericDesc = innerdesc1 })
      (fname2, Generic { genericEncode = innerenc2,
                         genericDecode = innerdec2,
                         genericDesc = innerdesc2 })
      (fname3, Generic { genericEncode = innerenc3,
                         genericDecode = innerdec3,
                         genericDesc = innerdesc3 })
      (fname4, Generic { genericEncode = innerenc4,
                         genericDecode = innerdec4,
                         genericDesc = innerdesc4 })
      (fname5, Generic { genericEncode = innerenc5,
                         genericDecode = innerdec5,
                         genericDesc = innerdesc5 })
      (fname6, Generic { genericEncode = innerenc6,
                         genericDecode = innerdec6,
                         genericDesc = innerdesc6 })
      (fname7, Generic { genericEncode = innerenc7,
                         genericDecode = innerdec7,
                         genericDesc = innerdesc7 })
      (fname8, Generic { genericEncode = innerenc8,
                         genericDecode = innerdec8,
                         genericDesc = innerdesc8 })
      (fname9, Generic { genericEncode = innerenc9,
                         genericDecode = innerdec9,
                         genericDesc = innerdesc9 })
      (fname10, Generic { genericEncode = innerenc10,
                          genericDecode = innerdec10,
                          genericDesc = innerdesc10 }) =
  Generic { genericEncode = \enc -> encodeSet10 (innerenc1 enc)
                                                (innerenc2 enc)
                                                (innerenc3 enc)
                                                (innerenc4 enc)
                                                (innerenc5 enc)
                                                (innerenc6 enc)
                                                (innerenc7 enc)
                                                (innerenc8 enc)
                                                (innerenc9 enc)
                                                (innerenc10 enc) enc,
            genericDecode = \enc -> decodeSet10 (innerdec1 enc)
                                                (innerdec2 enc)
                                                (innerdec3 enc)
                                                (innerdec4 enc)
                                                (innerdec5 enc)
                                                (innerdec6 enc)
                                                (innerdec7 enc)
                                                (innerdec8 enc)
                                                (innerdec9 enc)
                                                (innerdec10 enc) enc,
            genericDesc = setDesc [(fname1, innerdesc1),
                                   (fname2, innerdesc2),
                                   (fname3, innerdesc3),
                                   (fname4, innerdesc4),
                                   (fname5, innerdesc5),
                                   (fname6, innerdesc6),
                                   (fname7, innerdesc7),
                                   (fname8, innerdesc8),
                                   (fname9, innerdesc9),
                                   (fname10, innerdesc10)] }

set11 :: (String, Generic a)
      -> (String, Generic b)
      -> (String, Generic c)
      -> (String, Generic d)
      -> (String, Generic e)
      -> (String, Generic f)
      -> (String, Generic g)
      -> (String, Generic h)
      -> (String, Generic i)
      -> (String, Generic j)
      -> (String, Generic k)
      -> Generic (a, b, c, d, e, f, g, h, i, j, k)
set11 (fname1, Generic { genericEncode = innerenc1,
                         genericDecode = innerdec1,
                         genericDesc = innerdesc1 })
      (fname2, Generic { genericEncode = innerenc2,
                         genericDecode = innerdec2,
                         genericDesc = innerdesc2 })
      (fname3, Generic { genericEncode = innerenc3,
                         genericDecode = innerdec3,
                         genericDesc = innerdesc3 })
      (fname4, Generic { genericEncode = innerenc4,
                         genericDecode = innerdec4,
                         genericDesc = innerdesc4 })
      (fname5, Generic { genericEncode = innerenc5,
                         genericDecode = innerdec5,
                         genericDesc = innerdesc5 })
      (fname6, Generic { genericEncode = innerenc6,
                         genericDecode = innerdec6,
                         genericDesc = innerdesc6 })
      (fname7, Generic { genericEncode = innerenc7,
                         genericDecode = innerdec7,
                         genericDesc = innerdesc7 })
      (fname8, Generic { genericEncode = innerenc8,
                         genericDecode = innerdec8,
                         genericDesc = innerdesc8 })
      (fname9, Generic { genericEncode = innerenc9,
                         genericDecode = innerdec9,
                         genericDesc = innerdesc9 })
      (fname10, Generic { genericEncode = innerenc10,
                          genericDecode = innerdec10,
                          genericDesc = innerdesc10 })
      (fname11, Generic { genericEncode = innerenc11,
                          genericDecode = innerdec11,
                          genericDesc = innerdesc11 }) =
  Generic { genericEncode = \enc -> encodeSet11 (innerenc1 enc)
                                                (innerenc2 enc)
                                                (innerenc3 enc)
                                                (innerenc4 enc)
                                                (innerenc5 enc)
                                                (innerenc6 enc)
                                                (innerenc7 enc)
                                                (innerenc8 enc)
                                                (innerenc9 enc)
                                                (innerenc10 enc)
                                                (innerenc11 enc) enc,
            genericDecode = \enc -> decodeSet11 (innerdec1 enc)
                                                (innerdec2 enc)
                                                (innerdec3 enc)
                                                (innerdec4 enc)
                                                (innerdec5 enc)
                                                (innerdec6 enc)
                                                (innerdec7 enc)
                                                (innerdec8 enc)
                                                (innerdec9 enc)
                                                (innerdec10 enc)
                                                (innerdec11 enc) enc,
            genericDesc = setDesc [(fname1, innerdesc1),
                                   (fname2, innerdesc2),
                                   (fname3, innerdesc3),
                                   (fname4, innerdesc4),
                                   (fname5, innerdesc5),
                                   (fname6, innerdesc6),
                                   (fname7, innerdesc7),
                                   (fname8, innerdesc8),
                                   (fname9, innerdesc9),
                                   (fname10, innerdesc10),
                                   (fname11, innerdesc11)] }

set12 :: (String, Generic a)
      -> (String, Generic b)
      -> (String, Generic c)
      -> (String, Generic d)
      -> (String, Generic e)
      -> (String, Generic f)
      -> (String, Generic g)
      -> (String, Generic h)
      -> (String, Generic i)
      -> (String, Generic j)
      -> (String, Generic k)
      -> (String, Generic l)
      -> Generic (a, b, c, d, e, f, g, h, i, j, k, l)
set12 (fname1, Generic { genericEncode = innerenc1,
                         genericDecode = innerdec1,
                         genericDesc = innerdesc1 })
      (fname2, Generic { genericEncode = innerenc2,
                         genericDecode = innerdec2,
                         genericDesc = innerdesc2 })
      (fname3, Generic { genericEncode = innerenc3,
                         genericDecode = innerdec3,
                         genericDesc = innerdesc3 })
      (fname4, Generic { genericEncode = innerenc4,
                         genericDecode = innerdec4,
                         genericDesc = innerdesc4 })
      (fname5, Generic { genericEncode = innerenc5,
                         genericDecode = innerdec5,
                         genericDesc = innerdesc5 })
      (fname6, Generic { genericEncode = innerenc6,
                         genericDecode = innerdec6,
                         genericDesc = innerdesc6 })
      (fname7, Generic { genericEncode = innerenc7,
                         genericDecode = innerdec7,
                         genericDesc = innerdesc7 })
      (fname8, Generic { genericEncode = innerenc8,
                         genericDecode = innerdec8,
                         genericDesc = innerdesc8 })
      (fname9, Generic { genericEncode = innerenc9,
                         genericDecode = innerdec9,
                         genericDesc = innerdesc9 })
      (fname10, Generic { genericEncode = innerenc10,
                          genericDecode = innerdec10,
                          genericDesc = innerdesc10 })
      (fname11, Generic { genericEncode = innerenc11,
                          genericDecode = innerdec11,
                          genericDesc = innerdesc11 })
      (fname12, Generic { genericEncode = innerenc12,
                          genericDecode = innerdec12,
                          genericDesc = innerdesc12 }) =
  Generic { genericEncode = \enc -> encodeSet12 (innerenc1 enc)
                                                (innerenc2 enc)
                                                (innerenc3 enc)
                                                (innerenc4 enc)
                                                (innerenc5 enc)
                                                (innerenc6 enc)
                                                (innerenc7 enc)
                                                (innerenc8 enc)
                                                (innerenc9 enc)
                                                (innerenc10 enc)
                                                (innerenc11 enc)
                                                (innerenc12 enc) enc,
            genericDecode = \enc -> decodeSet12 (innerdec1 enc)
                                                (innerdec2 enc)
                                                (innerdec3 enc)
                                                (innerdec4 enc)
                                                (innerdec5 enc)
                                                (innerdec6 enc)
                                                (innerdec7 enc)
                                                (innerdec8 enc)
                                                (innerdec9 enc)
                                                (innerdec10 enc)
                                                (innerdec11 enc)
                                                (innerdec12 enc) enc,
            genericDesc = setDesc [(fname1, innerdesc1),
                                   (fname2, innerdesc2),
                                   (fname3, innerdesc3),
                                   (fname4, innerdesc4),
                                   (fname5, innerdesc5),
                                   (fname6, innerdesc6),
                                   (fname7, innerdesc7),
                                   (fname8, innerdesc8),
                                   (fname9, innerdesc9),
                                   (fname10, innerdesc10),
                                   (fname11, innerdesc11),
                                   (fname12, innerdesc12)] }

set13 :: (String, Generic a)
      -> (String, Generic b)
      -> (String, Generic c)
      -> (String, Generic d)
      -> (String, Generic e)
      -> (String, Generic f)
      -> (String, Generic g)
      -> (String, Generic h)
      -> (String, Generic i)
      -> (String, Generic j)
      -> (String, Generic k)
      -> (String, Generic l)
      -> (String, Generic m)
      -> Generic (a, b, c, d, e, f, g, h, i, j, k, l, m)
set13 (fname1, Generic { genericEncode = innerenc1,
                         genericDecode = innerdec1,
                         genericDesc = innerdesc1 })
      (fname2, Generic { genericEncode = innerenc2,
                         genericDecode = innerdec2,
                         genericDesc = innerdesc2 })
      (fname3, Generic { genericEncode = innerenc3,
                         genericDecode = innerdec3,
                         genericDesc = innerdesc3 })
      (fname4, Generic { genericEncode = innerenc4,
                         genericDecode = innerdec4,
                         genericDesc = innerdesc4 })
      (fname5, Generic { genericEncode = innerenc5,
                         genericDecode = innerdec5,
                         genericDesc = innerdesc5 })
      (fname6, Generic { genericEncode = innerenc6,
                         genericDecode = innerdec6,
                         genericDesc = innerdesc6 })
      (fname7, Generic { genericEncode = innerenc7,
                         genericDecode = innerdec7,
                         genericDesc = innerdesc7 })
      (fname8, Generic { genericEncode = innerenc8,
                         genericDecode = innerdec8,
                         genericDesc = innerdesc8 })
      (fname9, Generic { genericEncode = innerenc9,
                         genericDecode = innerdec9,
                         genericDesc = innerdesc9 })
      (fname10, Generic { genericEncode = innerenc10,
                          genericDecode = innerdec10,
                          genericDesc = innerdesc10 })
      (fname11, Generic { genericEncode = innerenc11,
                          genericDecode = innerdec11,
                          genericDesc = innerdesc11 })
      (fname12, Generic { genericEncode = innerenc12,
                          genericDecode = innerdec12,
                          genericDesc = innerdesc12 })
      (fname13, Generic { genericEncode = innerenc13,
                          genericDecode = innerdec13,
                          genericDesc = innerdesc13 }) =
  Generic { genericEncode = \enc -> encodeSet13 (innerenc1 enc)
                                                (innerenc2 enc)
                                                (innerenc3 enc)
                                                (innerenc4 enc)
                                                (innerenc5 enc)
                                                (innerenc6 enc)
                                                (innerenc7 enc)
                                                (innerenc8 enc)
                                                (innerenc9 enc)
                                                (innerenc10 enc)
                                                (innerenc11 enc)
                                                (innerenc12 enc)
                                                (innerenc13 enc) enc,
            genericDecode = \enc -> decodeSet13 (innerdec1 enc)
                                                (innerdec2 enc)
                                                (innerdec3 enc)
                                                (innerdec4 enc)
                                                (innerdec5 enc)
                                                (innerdec6 enc)
                                                (innerdec7 enc)
                                                (innerdec8 enc)
                                                (innerdec9 enc)
                                                (innerdec10 enc)
                                                (innerdec11 enc)
                                                (innerdec12 enc)
                                                (innerdec13 enc) enc,
            genericDesc = setDesc [(fname1, innerdesc1),
                                   (fname2, innerdesc2),
                                   (fname3, innerdesc3),
                                   (fname4, innerdesc4),
                                   (fname5, innerdesc5),
                                   (fname6, innerdesc6),
                                   (fname7, innerdesc7),
                                   (fname8, innerdesc8),
                                   (fname9, innerdesc9),
                                   (fname10, innerdesc10),
                                   (fname11, innerdesc11),
                                   (fname12, innerdesc12),
                                   (fname13, innerdesc13)] }

set14 :: (String, Generic a)
      -> (String, Generic b)
      -> (String, Generic c)
      -> (String, Generic d)
      -> (String, Generic e)
      -> (String, Generic f)
      -> (String, Generic g)
      -> (String, Generic h)
      -> (String, Generic i)
      -> (String, Generic j)
      -> (String, Generic k)
      -> (String, Generic l)
      -> (String, Generic m)
      -> (String, Generic n)
      -> Generic (a, b, c, d, e, f, g, h, i, j, k, l, m, n)
set14 (fname1, Generic { genericEncode = innerenc1,
                         genericDecode = innerdec1,
                         genericDesc = innerdesc1 })
      (fname2, Generic { genericEncode = innerenc2,
                         genericDecode = innerdec2,
                         genericDesc = innerdesc2 })
      (fname3, Generic { genericEncode = innerenc3,
                         genericDecode = innerdec3,
                         genericDesc = innerdesc3 })
      (fname4, Generic { genericEncode = innerenc4,
                         genericDecode = innerdec4,
                         genericDesc = innerdesc4 })
      (fname5, Generic { genericEncode = innerenc5,
                         genericDecode = innerdec5,
                         genericDesc = innerdesc5 })
      (fname6, Generic { genericEncode = innerenc6,
                         genericDecode = innerdec6,
                         genericDesc = innerdesc6 })
      (fname7, Generic { genericEncode = innerenc7,
                         genericDecode = innerdec7,
                         genericDesc = innerdesc7 })
      (fname8, Generic { genericEncode = innerenc8,
                         genericDecode = innerdec8,
                         genericDesc = innerdesc8 })
      (fname9, Generic { genericEncode = innerenc9,
                         genericDecode = innerdec9,
                         genericDesc = innerdesc9 })
      (fname10, Generic { genericEncode = innerenc10,
                          genericDecode = innerdec10,
                          genericDesc = innerdesc10 })
      (fname11, Generic { genericEncode = innerenc11,
                          genericDecode = innerdec11,
                          genericDesc = innerdesc11 })
      (fname12, Generic { genericEncode = innerenc12,
                          genericDecode = innerdec12,
                          genericDesc = innerdesc12 })
      (fname13, Generic { genericEncode = innerenc13,
                          genericDecode = innerdec13,
                          genericDesc = innerdesc13 })
      (fname14, Generic { genericEncode = innerenc14,
                          genericDecode = innerdec14,
                          genericDesc = innerdesc14 }) =
  Generic { genericEncode = \enc -> encodeSet14 (innerenc1 enc)
                                                (innerenc2 enc)
                                                (innerenc3 enc)
                                                (innerenc4 enc)
                                                (innerenc5 enc)
                                                (innerenc6 enc)
                                                (innerenc7 enc)
                                                (innerenc8 enc)
                                                (innerenc9 enc)
                                                (innerenc10 enc)
                                                (innerenc11 enc)
                                                (innerenc12 enc)
                                                (innerenc13 enc)
                                                (innerenc14 enc) enc,
            genericDecode = \enc -> decodeSet14 (innerdec1 enc)
                                                (innerdec2 enc)
                                                (innerdec3 enc)
                                                (innerdec4 enc)
                                                (innerdec5 enc)
                                                (innerdec6 enc)
                                                (innerdec7 enc)
                                                (innerdec8 enc)
                                                (innerdec9 enc)
                                                (innerdec10 enc)
                                                (innerdec11 enc)
                                                (innerdec12 enc)
                                                (innerdec13 enc)
                                                (innerdec14 enc) enc,
            genericDesc = setDesc [(fname1, innerdesc1),
                                   (fname2, innerdesc2),
                                   (fname3, innerdesc3),
                                   (fname4, innerdesc4),
                                   (fname5, innerdesc5),
                                   (fname6, innerdesc6),
                                   (fname7, innerdesc7),
                                   (fname8, innerdesc8),
                                   (fname9, innerdesc9),
                                   (fname10, innerdesc10),
                                   (fname11, innerdesc11),
                                   (fname12, innerdesc12),
                                   (fname13, innerdesc13),
                                   (fname14, innerdesc14)] }

set15 :: (String, Generic a)
      -> (String, Generic b)
      -> (String, Generic c)
      -> (String, Generic d)
      -> (String, Generic e)
      -> (String, Generic f)
      -> (String, Generic g)
      -> (String, Generic h)
      -> (String, Generic i)
      -> (String, Generic j)
      -> (String, Generic k)
      -> (String, Generic l)
      -> (String, Generic m)
      -> (String, Generic n)
      -> (String, Generic o)
      -> Generic (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o)
set15 (fname1, Generic { genericEncode = innerenc1,
                         genericDecode = innerdec1,
                         genericDesc = innerdesc1 })
      (fname2, Generic { genericEncode = innerenc2,
                         genericDecode = innerdec2,
                         genericDesc = innerdesc2 })
      (fname3, Generic { genericEncode = innerenc3,
                         genericDecode = innerdec3,
                         genericDesc = innerdesc3 })
      (fname4, Generic { genericEncode = innerenc4,
                         genericDecode = innerdec4,
                         genericDesc = innerdesc4 })
      (fname5, Generic { genericEncode = innerenc5,
                         genericDecode = innerdec5,
                         genericDesc = innerdesc5 })
      (fname6, Generic { genericEncode = innerenc6,
                         genericDecode = innerdec6,
                         genericDesc = innerdesc6 })
      (fname7, Generic { genericEncode = innerenc7,
                         genericDecode = innerdec7,
                         genericDesc = innerdesc7 })
      (fname8, Generic { genericEncode = innerenc8,
                         genericDecode = innerdec8,
                         genericDesc = innerdesc8 })
      (fname9, Generic { genericEncode = innerenc9,
                         genericDecode = innerdec9,
                         genericDesc = innerdesc9 })
      (fname10, Generic { genericEncode = innerenc10,
                          genericDecode = innerdec10,
                          genericDesc = innerdesc10 })
      (fname11, Generic { genericEncode = innerenc11,
                          genericDecode = innerdec11,
                          genericDesc = innerdesc11 })
      (fname12, Generic { genericEncode = innerenc12,
                          genericDecode = innerdec12,
                          genericDesc = innerdesc12 })
      (fname13, Generic { genericEncode = innerenc13,
                          genericDecode = innerdec13,
                          genericDesc = innerdesc13 })
      (fname14, Generic { genericEncode = innerenc14,
                          genericDecode = innerdec14,
                          genericDesc = innerdesc14 })
      (fname15, Generic { genericEncode = innerenc15,
                          genericDecode = innerdec15,
                          genericDesc = innerdesc15 }) =
  Generic { genericEncode = \enc -> encodeSet15 (innerenc1 enc)
                                                (innerenc2 enc)
                                                (innerenc3 enc)
                                                (innerenc4 enc)
                                                (innerenc5 enc)
                                                (innerenc6 enc)
                                                (innerenc7 enc)
                                                (innerenc8 enc)
                                                (innerenc9 enc)
                                                (innerenc10 enc)
                                                (innerenc11 enc)
                                                (innerenc12 enc)
                                                (innerenc13 enc)
                                                (innerenc14 enc)
                                                (innerenc15 enc) enc,
            genericDecode = \enc -> decodeSet15 (innerdec1 enc)
                                                (innerdec2 enc)
                                                (innerdec3 enc)
                                                (innerdec4 enc)
                                                (innerdec5 enc)
                                                (innerdec6 enc)
                                                (innerdec7 enc)
                                                (innerdec8 enc)
                                                (innerdec9 enc)
                                                (innerdec10 enc)
                                                (innerdec11 enc)
                                                (innerdec12 enc)
                                                (innerdec13 enc)
                                                (innerdec14 enc)
                                                (innerdec15 enc) enc,
            genericDesc = setDesc [(fname1, innerdesc1),
                                   (fname2, innerdesc2),
                                   (fname3, innerdesc3),
                                   (fname4, innerdesc4),
                                   (fname5, innerdesc5),
                                   (fname6, innerdesc6),
                                   (fname7, innerdesc7),
                                   (fname8, innerdesc8),
                                   (fname9, innerdesc9),
                                   (fname10, innerdesc10),
                                   (fname11, innerdesc11),
                                   (fname12, innerdesc12),
                                   (fname13, innerdesc13),
                                   (fname14, innerdesc14),
                                   (fname15, innerdesc15)] }

set16 :: (String, Generic a)
      -> (String, Generic b)
      -> (String, Generic c)
      -> (String, Generic d)
      -> (String, Generic e)
      -> (String, Generic f)
      -> (String, Generic g)
      -> (String, Generic h)
      -> (String, Generic i)
      -> (String, Generic j)
      -> (String, Generic k)
      -> (String, Generic l)
      -> (String, Generic m)
      -> (String, Generic n)
      -> (String, Generic o)
      -> (String, Generic p)
      -> Generic (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p)
set16 (fname1, Generic { genericEncode = innerenc1,
                         genericDecode = innerdec1,
                         genericDesc = innerdesc1 })
      (fname2, Generic { genericEncode = innerenc2,
                         genericDecode = innerdec2,
                         genericDesc = innerdesc2 })
      (fname3, Generic { genericEncode = innerenc3,
                         genericDecode = innerdec3,
                         genericDesc = innerdesc3 })
      (fname4, Generic { genericEncode = innerenc4,
                         genericDecode = innerdec4,
                         genericDesc = innerdesc4 })
      (fname5, Generic { genericEncode = innerenc5,
                         genericDecode = innerdec5,
                         genericDesc = innerdesc5 })
      (fname6, Generic { genericEncode = innerenc6,
                         genericDecode = innerdec6,
                         genericDesc = innerdesc6 })
      (fname7, Generic { genericEncode = innerenc7,
                         genericDecode = innerdec7,
                         genericDesc = innerdesc7 })
      (fname8, Generic { genericEncode = innerenc8,
                         genericDecode = innerdec8,
                         genericDesc = innerdesc8 })
      (fname9, Generic { genericEncode = innerenc9,
                         genericDecode = innerdec9,
                         genericDesc = innerdesc9 })
      (fname10, Generic { genericEncode = innerenc10,
                          genericDecode = innerdec10,
                          genericDesc = innerdesc10 })
      (fname11, Generic { genericEncode = innerenc11,
                          genericDecode = innerdec11,
                          genericDesc = innerdesc11 })
      (fname12, Generic { genericEncode = innerenc12,
                          genericDecode = innerdec12,
                          genericDesc = innerdesc12 })
      (fname13, Generic { genericEncode = innerenc13,
                          genericDecode = innerdec13,
                          genericDesc = innerdesc13 })
      (fname14, Generic { genericEncode = innerenc14,
                          genericDecode = innerdec14,
                          genericDesc = innerdesc14 })
      (fname15, Generic { genericEncode = innerenc15,
                          genericDecode = innerdec15,
                          genericDesc = innerdesc15 })
      (fname16, Generic { genericEncode = innerenc16,
                          genericDecode = innerdec16,
                          genericDesc = innerdesc16 }) =
  Generic { genericEncode = \enc -> encodeSet16 (innerenc1 enc)
                                                (innerenc2 enc)
                                                (innerenc3 enc)
                                                (innerenc4 enc)
                                                (innerenc5 enc)
                                                (innerenc6 enc)
                                                (innerenc7 enc)
                                                (innerenc8 enc)
                                                (innerenc9 enc)
                                                (innerenc10 enc)
                                                (innerenc11 enc)
                                                (innerenc12 enc)
                                                (innerenc13 enc)
                                                (innerenc14 enc)
                                                (innerenc15 enc)
                                                (innerenc16 enc) enc,
            genericDecode = \enc -> decodeSet16 (innerdec1 enc)
                                                (innerdec2 enc)
                                                (innerdec3 enc)
                                                (innerdec4 enc)
                                                (innerdec5 enc)
                                                (innerdec6 enc)
                                                (innerdec7 enc)
                                                (innerdec8 enc)
                                                (innerdec9 enc)
                                                (innerdec10 enc)
                                                (innerdec11 enc)
                                                (innerdec12 enc)
                                                (innerdec13 enc)
                                                (innerdec14 enc)
                                                (innerdec15 enc)
                                                (innerdec16 enc) enc,
            genericDesc = setDesc [(fname1, innerdesc1),
                                   (fname2, innerdesc2),
                                   (fname3, innerdesc3),
                                   (fname4, innerdesc4),
                                   (fname5, innerdesc5),
                                   (fname6, innerdesc6),
                                   (fname7, innerdesc7),
                                   (fname8, innerdesc8),
                                   (fname9, innerdesc9),
                                   (fname10, innerdesc10),
                                   (fname11, innerdesc11),
                                   (fname12, innerdesc12),
                                   (fname13, innerdesc13),
                                   (fname14, innerdesc14),
                                   (fname15, innerdesc15),
                                   (fname16, innerdesc16)] }

set17 :: (String, Generic a)
      -> (String, Generic b)
      -> (String, Generic c)
      -> (String, Generic d)
      -> (String, Generic e)
      -> (String, Generic f)
      -> (String, Generic g)
      -> (String, Generic h)
      -> (String, Generic i)
      -> (String, Generic j)
      -> (String, Generic k)
      -> (String, Generic l)
      -> (String, Generic m)
      -> (String, Generic n)
      -> (String, Generic o)
      -> (String, Generic p)
      -> (String, Generic q)
      -> Generic (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q)
set17 (fname1, Generic { genericEncode = innerenc1,
                         genericDecode = innerdec1,
                         genericDesc = innerdesc1 })
      (fname2, Generic { genericEncode = innerenc2,
                         genericDecode = innerdec2,
                         genericDesc = innerdesc2 })
      (fname3, Generic { genericEncode = innerenc3,
                         genericDecode = innerdec3,
                         genericDesc = innerdesc3 })
      (fname4, Generic { genericEncode = innerenc4,
                         genericDecode = innerdec4,
                         genericDesc = innerdesc4 })
      (fname5, Generic { genericEncode = innerenc5,
                         genericDecode = innerdec5,
                         genericDesc = innerdesc5 })
      (fname6, Generic { genericEncode = innerenc6,
                         genericDecode = innerdec6,
                         genericDesc = innerdesc6 })
      (fname7, Generic { genericEncode = innerenc7,
                         genericDecode = innerdec7,
                         genericDesc = innerdesc7 })
      (fname8, Generic { genericEncode = innerenc8,
                         genericDecode = innerdec8,
                         genericDesc = innerdesc8 })
      (fname9, Generic { genericEncode = innerenc9,
                         genericDecode = innerdec9,
                         genericDesc = innerdesc9 })
      (fname10, Generic { genericEncode = innerenc10,
                          genericDecode = innerdec10,
                          genericDesc = innerdesc10 })
      (fname11, Generic { genericEncode = innerenc11,
                          genericDecode = innerdec11,
                          genericDesc = innerdesc11 })
      (fname12, Generic { genericEncode = innerenc12,
                          genericDecode = innerdec12,
                          genericDesc = innerdesc12 })
      (fname13, Generic { genericEncode = innerenc13,
                          genericDecode = innerdec13,
                          genericDesc = innerdesc13 })
      (fname14, Generic { genericEncode = innerenc14,
                          genericDecode = innerdec14,
                          genericDesc = innerdesc14 })
      (fname15, Generic { genericEncode = innerenc15,
                          genericDecode = innerdec15,
                          genericDesc = innerdesc15 })
      (fname16, Generic { genericEncode = innerenc16,
                          genericDecode = innerdec16,
                          genericDesc = innerdesc16 })
      (fname17, Generic { genericEncode = innerenc17,
                          genericDecode = innerdec17,
                          genericDesc = innerdesc17 }) =
  Generic { genericEncode = \enc -> encodeSet17 (innerenc1 enc)
                                                (innerenc2 enc)
                                                (innerenc3 enc)
                                                (innerenc4 enc)
                                                (innerenc5 enc)
                                                (innerenc6 enc)
                                                (innerenc7 enc)
                                                (innerenc8 enc)
                                                (innerenc9 enc)
                                                (innerenc10 enc)
                                                (innerenc11 enc)
                                                (innerenc12 enc)
                                                (innerenc13 enc)
                                                (innerenc14 enc)
                                                (innerenc15 enc)
                                                (innerenc16 enc)
                                                (innerenc17 enc) enc,
            genericDecode = \enc -> decodeSet17 (innerdec1 enc)
                                                (innerdec2 enc)
                                                (innerdec3 enc)
                                                (innerdec4 enc)
                                                (innerdec5 enc)
                                                (innerdec6 enc)
                                                (innerdec7 enc)
                                                (innerdec8 enc)
                                                (innerdec9 enc)
                                                (innerdec10 enc)
                                                (innerdec11 enc)
                                                (innerdec12 enc)
                                                (innerdec13 enc)
                                                (innerdec14 enc)
                                                (innerdec15 enc)
                                                (innerdec16 enc)
                                                (innerdec17 enc) enc,
            genericDesc = setDesc [(fname1, innerdesc1),
                                   (fname2, innerdesc2),
                                   (fname3, innerdesc3),
                                   (fname4, innerdesc4),
                                   (fname5, innerdesc5),
                                   (fname6, innerdesc6),
                                   (fname7, innerdesc7),
                                   (fname8, innerdesc8),
                                   (fname9, innerdesc9),
                                   (fname10, innerdesc10),
                                   (fname11, innerdesc11),
                                   (fname12, innerdesc12),
                                   (fname13, innerdesc13),
                                   (fname14, innerdesc14),
                                   (fname15, innerdesc15),
                                   (fname16, innerdesc16),
                                   (fname17, innerdesc17)] }

set18 :: (String, Generic a)
      -> (String, Generic b)
      -> (String, Generic c)
      -> (String, Generic d)
      -> (String, Generic e)
      -> (String, Generic f)
      -> (String, Generic g)
      -> (String, Generic h)
      -> (String, Generic i)
      -> (String, Generic j)
      -> (String, Generic k)
      -> (String, Generic l)
      -> (String, Generic m)
      -> (String, Generic n)
      -> (String, Generic o)
      -> (String, Generic p)
      -> (String, Generic q)
      -> (String, Generic r)
      -> Generic (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r)
set18 (fname1, Generic { genericEncode = innerenc1,
                         genericDecode = innerdec1,
                         genericDesc = innerdesc1 })
      (fname2, Generic { genericEncode = innerenc2,
                         genericDecode = innerdec2,
                         genericDesc = innerdesc2 })
      (fname3, Generic { genericEncode = innerenc3,
                         genericDecode = innerdec3,
                         genericDesc = innerdesc3 })
      (fname4, Generic { genericEncode = innerenc4,
                         genericDecode = innerdec4,
                         genericDesc = innerdesc4 })
      (fname5, Generic { genericEncode = innerenc5,
                         genericDecode = innerdec5,
                         genericDesc = innerdesc5 })
      (fname6, Generic { genericEncode = innerenc6,
                         genericDecode = innerdec6,
                         genericDesc = innerdesc6 })
      (fname7, Generic { genericEncode = innerenc7,
                         genericDecode = innerdec7,
                         genericDesc = innerdesc7 })
      (fname8, Generic { genericEncode = innerenc8,
                         genericDecode = innerdec8,
                         genericDesc = innerdesc8 })
      (fname9, Generic { genericEncode = innerenc9,
                         genericDecode = innerdec9,
                         genericDesc = innerdesc9 })
      (fname10, Generic { genericEncode = innerenc10,
                          genericDecode = innerdec10,
                          genericDesc = innerdesc10 })
      (fname11, Generic { genericEncode = innerenc11,
                          genericDecode = innerdec11,
                          genericDesc = innerdesc11 })
      (fname12, Generic { genericEncode = innerenc12,
                          genericDecode = innerdec12,
                          genericDesc = innerdesc12 })
      (fname13, Generic { genericEncode = innerenc13,
                          genericDecode = innerdec13,
                          genericDesc = innerdesc13 })
      (fname14, Generic { genericEncode = innerenc14,
                          genericDecode = innerdec14,
                          genericDesc = innerdesc14 })
      (fname15, Generic { genericEncode = innerenc15,
                          genericDecode = innerdec15,
                          genericDesc = innerdesc15 })
      (fname16, Generic { genericEncode = innerenc16,
                          genericDecode = innerdec16,
                          genericDesc = innerdesc16 })
      (fname17, Generic { genericEncode = innerenc17,
                          genericDecode = innerdec17,
                          genericDesc = innerdesc17 })
      (fname18, Generic { genericEncode = innerenc18,
                          genericDecode = innerdec18,
                          genericDesc = innerdesc18 }) =
  Generic { genericEncode = \enc -> encodeSet18 (innerenc1 enc)
                                                (innerenc2 enc)
                                                (innerenc3 enc)
                                                (innerenc4 enc)
                                                (innerenc5 enc)
                                                (innerenc6 enc)
                                                (innerenc7 enc)
                                                (innerenc8 enc)
                                                (innerenc9 enc)
                                                (innerenc10 enc)
                                                (innerenc11 enc)
                                                (innerenc12 enc)
                                                (innerenc13 enc)
                                                (innerenc14 enc)
                                                (innerenc15 enc)
                                                (innerenc16 enc)
                                                (innerenc17 enc)
                                                (innerenc18 enc) enc,
            genericDecode = \enc -> decodeSet18 (innerdec1 enc)
                                                (innerdec2 enc)
                                                (innerdec3 enc)
                                                (innerdec4 enc)
                                                (innerdec5 enc)
                                                (innerdec6 enc)
                                                (innerdec7 enc)
                                                (innerdec8 enc)
                                                (innerdec9 enc)
                                                (innerdec10 enc)
                                                (innerdec11 enc)
                                                (innerdec12 enc)
                                                (innerdec13 enc)
                                                (innerdec14 enc)
                                                (innerdec15 enc)
                                                (innerdec16 enc)
                                                (innerdec17 enc)
                                                (innerdec18 enc) enc,
            genericDesc = setDesc [(fname1, innerdesc1),
                                   (fname2, innerdesc2),
                                   (fname3, innerdesc3),
                                   (fname4, innerdesc4),
                                   (fname5, innerdesc5),
                                   (fname6, innerdesc6),
                                   (fname7, innerdesc7),
                                   (fname8, innerdesc8),
                                   (fname9, innerdesc9),
                                   (fname10, innerdesc10),
                                   (fname11, innerdesc11),
                                   (fname12, innerdesc12),
                                   (fname13, innerdesc13),
                                   (fname14, innerdesc14),
                                   (fname15, innerdesc15),
                                   (fname16, innerdesc16),
                                   (fname17, innerdesc17),
                                   (fname18, innerdesc18)] }

set19 :: (String, Generic a)
      -> (String, Generic b)
      -> (String, Generic c)
      -> (String, Generic d)
      -> (String, Generic e)
      -> (String, Generic f)
      -> (String, Generic g)
      -> (String, Generic h)
      -> (String, Generic i)
      -> (String, Generic j)
      -> (String, Generic k)
      -> (String, Generic l)
      -> (String, Generic m)
      -> (String, Generic n)
      -> (String, Generic o)
      -> (String, Generic p)
      -> (String, Generic q)
      -> (String, Generic r)
      -> (String, Generic s)
      -> Generic (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s)
set19 (fname1, Generic { genericEncode = innerenc1,
                         genericDecode = innerdec1,
                         genericDesc = innerdesc1 })
      (fname2, Generic { genericEncode = innerenc2,
                         genericDecode = innerdec2,
                         genericDesc = innerdesc2 })
      (fname3, Generic { genericEncode = innerenc3,
                         genericDecode = innerdec3,
                         genericDesc = innerdesc3 })
      (fname4, Generic { genericEncode = innerenc4,
                         genericDecode = innerdec4,
                         genericDesc = innerdesc4 })
      (fname5, Generic { genericEncode = innerenc5,
                         genericDecode = innerdec5,
                         genericDesc = innerdesc5 })
      (fname6, Generic { genericEncode = innerenc6,
                         genericDecode = innerdec6,
                         genericDesc = innerdesc6 })
      (fname7, Generic { genericEncode = innerenc7,
                         genericDecode = innerdec7,
                         genericDesc = innerdesc7 })
      (fname8, Generic { genericEncode = innerenc8,
                         genericDecode = innerdec8,
                         genericDesc = innerdesc8 })
      (fname9, Generic { genericEncode = innerenc9,
                         genericDecode = innerdec9,
                         genericDesc = innerdesc9 })
      (fname10, Generic { genericEncode = innerenc10,
                          genericDecode = innerdec10,
                          genericDesc = innerdesc10 })
      (fname11, Generic { genericEncode = innerenc11,
                          genericDecode = innerdec11,
                          genericDesc = innerdesc11 })
      (fname12, Generic { genericEncode = innerenc12,
                          genericDecode = innerdec12,
                          genericDesc = innerdesc12 })
      (fname13, Generic { genericEncode = innerenc13,
                          genericDecode = innerdec13,
                          genericDesc = innerdesc13 })
      (fname14, Generic { genericEncode = innerenc14,
                          genericDecode = innerdec14,
                          genericDesc = innerdesc14 })
      (fname15, Generic { genericEncode = innerenc15,
                          genericDecode = innerdec15,
                          genericDesc = innerdesc15 })
      (fname16, Generic { genericEncode = innerenc16,
                          genericDecode = innerdec16,
                          genericDesc = innerdesc16 })
      (fname17, Generic { genericEncode = innerenc17,
                          genericDecode = innerdec17,
                          genericDesc = innerdesc17 })
      (fname18, Generic { genericEncode = innerenc18,
                          genericDecode = innerdec18,
                          genericDesc = innerdesc18 })
      (fname19, Generic { genericEncode = innerenc19,
                          genericDecode = innerdec19,
                          genericDesc = innerdesc19 }) =
  Generic { genericEncode = \enc -> encodeSet19 (innerenc1 enc)
                                                (innerenc2 enc)
                                                (innerenc3 enc)
                                                (innerenc4 enc)
                                                (innerenc5 enc)
                                                (innerenc6 enc)
                                                (innerenc7 enc)
                                                (innerenc8 enc)
                                                (innerenc9 enc)
                                                (innerenc10 enc)
                                                (innerenc11 enc)
                                                (innerenc12 enc)
                                                (innerenc13 enc)
                                                (innerenc14 enc)
                                                (innerenc15 enc)
                                                (innerenc16 enc)
                                                (innerenc17 enc)
                                                (innerenc18 enc)
                                                (innerenc19 enc) enc,
            genericDecode = \enc -> decodeSet19 (innerdec1 enc)
                                                (innerdec2 enc)
                                                (innerdec3 enc)
                                                (innerdec4 enc)
                                                (innerdec5 enc)
                                                (innerdec6 enc)
                                                (innerdec7 enc)
                                                (innerdec8 enc)
                                                (innerdec9 enc)
                                                (innerdec10 enc)
                                                (innerdec11 enc)
                                                (innerdec12 enc)
                                                (innerdec13 enc)
                                                (innerdec14 enc)
                                                (innerdec15 enc)
                                                (innerdec16 enc)
                                                (innerdec17 enc)
                                                (innerdec18 enc)
                                                (innerdec19 enc) enc,
            genericDesc = setDesc [(fname1, innerdesc1),
                                   (fname2, innerdesc2),
                                   (fname3, innerdesc3),
                                   (fname4, innerdesc4),
                                   (fname5, innerdesc5),
                                   (fname6, innerdesc6),
                                   (fname7, innerdesc7),
                                   (fname8, innerdesc8),
                                   (fname9, innerdesc9),
                                   (fname10, innerdesc10),
                                   (fname11, innerdesc11),
                                   (fname12, innerdesc12),
                                   (fname13, innerdesc13),
                                   (fname14, innerdesc14),
                                   (fname15, innerdesc15),
                                   (fname16, innerdesc16),
                                   (fname17, innerdesc17),
                                   (fname18, innerdesc18),
                                   (fname19, innerdesc19)] }

set20 :: (String, Generic a)
      -> (String, Generic b)
      -> (String, Generic c)
      -> (String, Generic d)
      -> (String, Generic e)
      -> (String, Generic f)
      -> (String, Generic g)
      -> (String, Generic h)
      -> (String, Generic i)
      -> (String, Generic j)
      -> (String, Generic k)
      -> (String, Generic l)
      -> (String, Generic m)
      -> (String, Generic n)
      -> (String, Generic o)
      -> (String, Generic p)
      -> (String, Generic q)
      -> (String, Generic r)
      -> (String, Generic s)
      -> (String, Generic t)
      -> Generic (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t)
set20 (fname1, Generic { genericEncode = innerenc1,
                         genericDecode = innerdec1,
                         genericDesc = innerdesc1 })
      (fname2, Generic { genericEncode = innerenc2,
                         genericDecode = innerdec2,
                         genericDesc = innerdesc2 })
      (fname3, Generic { genericEncode = innerenc3,
                         genericDecode = innerdec3,
                         genericDesc = innerdesc3 })
      (fname4, Generic { genericEncode = innerenc4,
                         genericDecode = innerdec4,
                         genericDesc = innerdesc4 })
      (fname5, Generic { genericEncode = innerenc5,
                         genericDecode = innerdec5,
                         genericDesc = innerdesc5 })
      (fname6, Generic { genericEncode = innerenc6,
                         genericDecode = innerdec6,
                         genericDesc = innerdesc6 })
      (fname7, Generic { genericEncode = innerenc7,
                         genericDecode = innerdec7,
                         genericDesc = innerdesc7 })
      (fname8, Generic { genericEncode = innerenc8,
                         genericDecode = innerdec8,
                         genericDesc = innerdesc8 })
      (fname9, Generic { genericEncode = innerenc9,
                         genericDecode = innerdec9,
                         genericDesc = innerdesc9 })
      (fname10, Generic { genericEncode = innerenc10,
                          genericDecode = innerdec10,
                          genericDesc = innerdesc10 })
      (fname11, Generic { genericEncode = innerenc11,
                          genericDecode = innerdec11,
                          genericDesc = innerdesc11 })
      (fname12, Generic { genericEncode = innerenc12,
                          genericDecode = innerdec12,
                          genericDesc = innerdesc12 })
      (fname13, Generic { genericEncode = innerenc13,
                          genericDecode = innerdec13,
                          genericDesc = innerdesc13 })
      (fname14, Generic { genericEncode = innerenc14,
                          genericDecode = innerdec14,
                          genericDesc = innerdesc14 })
      (fname15, Generic { genericEncode = innerenc15,
                          genericDecode = innerdec15,
                          genericDesc = innerdesc15 })
      (fname16, Generic { genericEncode = innerenc16,
                          genericDecode = innerdec16,
                          genericDesc = innerdesc16 })
      (fname17, Generic { genericEncode = innerenc17,
                          genericDecode = innerdec17,
                          genericDesc = innerdesc17 })
      (fname18, Generic { genericEncode = innerenc18,
                          genericDecode = innerdec18,
                          genericDesc = innerdesc18 })
      (fname19, Generic { genericEncode = innerenc19,
                          genericDecode = innerdec19,
                          genericDesc = innerdesc19 })
      (fname20, Generic { genericEncode = innerenc20,
                          genericDecode = innerdec20,
                          genericDesc = innerdesc20 }) =
  Generic { genericEncode = \enc -> encodeSet20 (innerenc1 enc)
                                                (innerenc2 enc)
                                                (innerenc3 enc)
                                                (innerenc4 enc)
                                                (innerenc5 enc)
                                                (innerenc6 enc)
                                                (innerenc7 enc)
                                                (innerenc8 enc)
                                                (innerenc9 enc)
                                                (innerenc10 enc)
                                                (innerenc11 enc)
                                                (innerenc12 enc)
                                                (innerenc13 enc)
                                                (innerenc14 enc)
                                                (innerenc15 enc)
                                                (innerenc16 enc)
                                                (innerenc17 enc)
                                                (innerenc18 enc)
                                                (innerenc19 enc)
                                                (innerenc20 enc) enc,
            genericDecode = \enc -> decodeSet20 (innerdec1 enc)
                                                (innerdec2 enc)
                                                (innerdec3 enc)
                                                (innerdec4 enc)
                                                (innerdec5 enc)
                                                (innerdec6 enc)
                                                (innerdec7 enc)
                                                (innerdec8 enc)
                                                (innerdec9 enc)
                                                (innerdec10 enc)
                                                (innerdec11 enc)
                                                (innerdec12 enc)
                                                (innerdec13 enc)
                                                (innerdec14 enc)
                                                (innerdec15 enc)
                                                (innerdec16 enc)
                                                (innerdec17 enc)
                                                (innerdec18 enc)
                                                (innerdec19 enc)
                                                (innerdec20 enc) enc,
            genericDesc = setDesc [(fname1, innerdesc1),
                                   (fname2, innerdesc2),
                                   (fname3, innerdesc3),
                                   (fname4, innerdesc4),
                                   (fname5, innerdesc5),
                                   (fname6, innerdesc6),
                                   (fname7, innerdesc7),
                                   (fname8, innerdesc8),
                                   (fname9, innerdesc9),
                                   (fname10, innerdesc10),
                                   (fname11, innerdesc11),
                                   (fname12, innerdesc12),
                                   (fname13, innerdesc13),
                                   (fname14, innerdesc14),
                                   (fname15, innerdesc15),
                                   (fname16, innerdesc16),
                                   (fname17, innerdesc17),
                                   (fname18, innerdesc18),
                                   (fname19, innerdesc19),
                                   (fname20, innerdesc20)] }

instance (Alternative decodety, GenericEncoding decodety encodedty encty) =>
         Schema decodety encodedty encty Generic where
  encode enc Generic { genericEncode = func } = func enc
  decode enc Generic { genericDecode = func } = func enc
