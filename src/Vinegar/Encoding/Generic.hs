-- Copyright (c) 2018 Eric McCorkle.  All rights reserved.
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

module Vinegar.Encoding.Generic(
       GenericEncoding(..),
       GenericEncodingDesc(..)
       ) where

import Vinegar.Encoding.Class
import Vinegar.Encoding.Desc.Class

import qualified Data.ByteString as Strict
import qualified Data.ByteString.Lazy as Lazy

class Encoding decodety encodedty encty =>
      GenericEncoding decodety encodedty encty |
        encty -> decodety  where
  encodeUnit :: encty
             -> encodedty

  encodeBool :: encty
             -> Bool
             -> encodedty

  encodeUnboundedIntegral :: (Integral n) =>
                             encty
                          -> n
                          -> encodedty

  encodeBoundedIntegral :: (Integral n) =>
                           n
                        -- ^ The lower bound on the number.
                        -> n
                        -- ^ The upper bound on the number.
                        -> encty
                        -> n
                        -> encodedty

  encodeVariableBytes :: encty
                      -> Strict.ByteString
                      -> encodedty

  encodeVariableLazyBytes :: encty
                          -> Lazy.ByteString
                          -> encodedty

  encodeFixedBytes :: (Integral n) =>
                      n
                   -- ^ The length of the bytestring
                   -> encty
                   -> Strict.ByteString
                   -> encodedty

  encodeFixedLazyBytes :: (Integral n) =>
                          n
                       -- ^ The length of the bytestring
                       -> encty
                       -> Lazy.ByteString
                       -> encodedty

  encodeSeqOf :: (a -> encodedty)
              -- ^ The encoding function for the inner type
              -> encty
              -> [a]
              -> encodedty

  encodeSetOf :: (a -> encodedty)
              -- ^ The encoding function for the inner type
              -> encty
              -> [a]
              -> encodedty

  encodeOptional :: (a -> encodedty)
                 -> encty
                 -> Maybe a
                 -> encodedty

  encodeChoice :: (a -> encodedty)
               -> encty
               -> a
               -> encodedty

  encodeStruct1 :: (a -> encodedty)
                -> encty
                -> a
                -> encodedty

  encodeStruct2 :: (a -> encodedty)
                -> (b -> encodedty)
                -> encty
                -> (a, b)
                -> encodedty

  encodeStruct3 :: (a -> encodedty)
                -> (b -> encodedty)
                -> (c -> encodedty)
                -> encty
                -> (a, b, c)
                -> encodedty

  encodeStruct4 :: (a -> encodedty)
                -> (b -> encodedty)
                -> (c -> encodedty)
                -> (d -> encodedty)
                -> encty
                -> (a, b, c, d)
                -> encodedty

  encodeStruct5 :: (a -> encodedty)
                -> (b -> encodedty)
                -> (c -> encodedty)
                -> (d -> encodedty)
                -> (e -> encodedty)
                -> encty
                -> (a, b, c, d, e)
                -> encodedty

  encodeStruct6 :: (a -> encodedty)
                -> (b -> encodedty)
                -> (c -> encodedty)
                -> (d -> encodedty)
                -> (e -> encodedty)
                -> (f -> encodedty)
                -> encty
                -> (a, b, c, d, e, f)
                -> encodedty

  encodeStruct7 :: (a -> encodedty)
                -> (b -> encodedty)
                -> (c -> encodedty)
                -> (d -> encodedty)
                -> (e -> encodedty)
                -> (f -> encodedty)
                -> (g -> encodedty)
                -> encty
                -> (a, b, c, d, e, f, g)
                -> encodedty

  encodeStruct8 :: (a -> encodedty)
                -> (b -> encodedty)
                -> (c -> encodedty)
                -> (d -> encodedty)
                -> (e -> encodedty)
                -> (f -> encodedty)
                -> (g -> encodedty)
                -> (h -> encodedty)
                -> encty
                -> (a, b, c, d, e, f, g, h)
                -> encodedty

  encodeStruct9 :: (a -> encodedty)
                -> (b -> encodedty)
                -> (c -> encodedty)
                -> (d -> encodedty)
                -> (e -> encodedty)
                -> (f -> encodedty)
                -> (g -> encodedty)
                -> (h -> encodedty)
                -> (i -> encodedty)
                -> encty
                -> (a, b, c, d, e, f, g, h, i)
                -> encodedty

  encodeStruct10 :: (a -> encodedty)
                 -> (b -> encodedty)
                 -> (c -> encodedty)
                 -> (d -> encodedty)
                 -> (e -> encodedty)
                 -> (f -> encodedty)
                 -> (g -> encodedty)
                 -> (h -> encodedty)
                 -> (i -> encodedty)
                 -> (j -> encodedty)
                 -> encty
                 -> (a, b, c, d, e, f, g, h, i, j)
                 -> encodedty

  encodeStruct11 :: (a -> encodedty)
                 -> (b -> encodedty)
                 -> (c -> encodedty)
                 -> (d -> encodedty)
                 -> (e -> encodedty)
                 -> (f -> encodedty)
                 -> (g -> encodedty)
                 -> (h -> encodedty)
                 -> (i -> encodedty)
                 -> (j -> encodedty)
                 -> (k -> encodedty)
                 -> encty
                 -> (a, b, c, d, e, f, g, h, i, j, k)
                 -> encodedty

  encodeStruct12 :: (a -> encodedty)
                 -> (b -> encodedty)
                 -> (c -> encodedty)
                 -> (d -> encodedty)
                 -> (e -> encodedty)
                 -> (f -> encodedty)
                 -> (g -> encodedty)
                 -> (h -> encodedty)
                 -> (i -> encodedty)
                 -> (j -> encodedty)
                 -> (k -> encodedty)
                 -> (l -> encodedty)
                 -> encty
                 -> (a, b, c, d, e, f, g, h, i, j, k, l)
                 -> encodedty

  encodeStruct13 :: (a -> encodedty)
                 -> (b -> encodedty)
                 -> (c -> encodedty)
                 -> (d -> encodedty)
                 -> (e -> encodedty)
                 -> (f -> encodedty)
                 -> (g -> encodedty)
                 -> (h -> encodedty)
                 -> (i -> encodedty)
                 -> (j -> encodedty)
                 -> (k -> encodedty)
                 -> (l -> encodedty)
                 -> (m -> encodedty)
                 -> encty
                 -> (a, b, c, d, e, f, g, h, i, j, k, l, m)
                 -> encodedty

  encodeStruct14 :: (a -> encodedty)
                 -> (b -> encodedty)
                 -> (c -> encodedty)
                 -> (d -> encodedty)
                 -> (e -> encodedty)
                 -> (f -> encodedty)
                 -> (g -> encodedty)
                 -> (h -> encodedty)
                 -> (i -> encodedty)
                 -> (j -> encodedty)
                 -> (k -> encodedty)
                 -> (l -> encodedty)
                 -> (m -> encodedty)
                 -> (n -> encodedty)
                 -> encty
                 -> (a, b, c, d, e, f, g, h, i, j, k, l, m, n)
                 -> encodedty

  encodeStruct15 :: (a -> encodedty)
                 -> (b -> encodedty)
                 -> (c -> encodedty)
                 -> (d -> encodedty)
                 -> (e -> encodedty)
                 -> (f -> encodedty)
                 -> (g -> encodedty)
                 -> (h -> encodedty)
                 -> (i -> encodedty)
                 -> (j -> encodedty)
                 -> (k -> encodedty)
                 -> (l -> encodedty)
                 -> (m -> encodedty)
                 -> (n -> encodedty)
                 -> (o -> encodedty)
                 -> encty
                 -> (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o)
                 -> encodedty

  encodeStruct16 :: (a -> encodedty)
                 -> (b -> encodedty)
                 -> (c -> encodedty)
                 -> (d -> encodedty)
                 -> (e -> encodedty)
                 -> (f -> encodedty)
                 -> (g -> encodedty)
                 -> (h -> encodedty)
                 -> (i -> encodedty)
                 -> (j -> encodedty)
                 -> (k -> encodedty)
                 -> (l -> encodedty)
                 -> (m -> encodedty)
                 -> (n -> encodedty)
                 -> (o -> encodedty)
                 -> (p -> encodedty)
                 -> encty
                 -> (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p)
                 -> encodedty

  encodeStruct17 :: (a -> encodedty)
                 -> (b -> encodedty)
                 -> (c -> encodedty)
                 -> (d -> encodedty)
                 -> (e -> encodedty)
                 -> (f -> encodedty)
                 -> (g -> encodedty)
                 -> (h -> encodedty)
                 -> (i -> encodedty)
                 -> (j -> encodedty)
                 -> (k -> encodedty)
                 -> (l -> encodedty)
                 -> (m -> encodedty)
                 -> (n -> encodedty)
                 -> (o -> encodedty)
                 -> (p -> encodedty)
                 -> (q -> encodedty)
                 -> encty
                 -> (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q)
                 -> encodedty

  encodeStruct18 :: (a -> encodedty)
                 -> (b -> encodedty)
                 -> (c -> encodedty)
                 -> (d -> encodedty)
                 -> (e -> encodedty)
                 -> (f -> encodedty)
                 -> (g -> encodedty)
                 -> (h -> encodedty)
                 -> (i -> encodedty)
                 -> (j -> encodedty)
                 -> (k -> encodedty)
                 -> (l -> encodedty)
                 -> (m -> encodedty)
                 -> (n -> encodedty)
                 -> (o -> encodedty)
                 -> (p -> encodedty)
                 -> (q -> encodedty)
                 -> (r -> encodedty)
                 -> encty
                 -> (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r)
                 -> encodedty

  encodeStruct19 :: (a -> encodedty)
                 -> (b -> encodedty)
                 -> (c -> encodedty)
                 -> (d -> encodedty)
                 -> (e -> encodedty)
                 -> (f -> encodedty)
                 -> (g -> encodedty)
                 -> (h -> encodedty)
                 -> (i -> encodedty)
                 -> (j -> encodedty)
                 -> (k -> encodedty)
                 -> (l -> encodedty)
                 -> (m -> encodedty)
                 -> (n -> encodedty)
                 -> (o -> encodedty)
                 -> (p -> encodedty)
                 -> (q -> encodedty)
                 -> (r -> encodedty)
                 -> (s -> encodedty)
                 -> encty
                 -> (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s)
                 -> encodedty

  encodeStruct20 :: (a -> encodedty)
                 -> (b -> encodedty)
                 -> (c -> encodedty)
                 -> (d -> encodedty)
                 -> (e -> encodedty)
                 -> (f -> encodedty)
                 -> (g -> encodedty)
                 -> (h -> encodedty)
                 -> (i -> encodedty)
                 -> (j -> encodedty)
                 -> (k -> encodedty)
                 -> (l -> encodedty)
                 -> (m -> encodedty)
                 -> (n -> encodedty)
                 -> (o -> encodedty)
                 -> (p -> encodedty)
                 -> (q -> encodedty)
                 -> (r -> encodedty)
                 -> (s -> encodedty)
                 -> (t -> encodedty)
                 -> encty
                 -> (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t)
                 -> encodedty

  encodeSet2 :: (a -> encodedty)
             -> (b -> encodedty)
             -> encty
             -> (a, b)
             -> encodedty

  encodeSet3 :: (a -> encodedty)
             -> (b -> encodedty)
             -> (c -> encodedty)
             -> encty
             -> (a, b, c)
             -> encodedty

  encodeSet4 :: (a -> encodedty)
             -> (b -> encodedty)
             -> (c -> encodedty)
             -> (d -> encodedty)
             -> encty
             -> (a, b, c, d)
             -> encodedty

  encodeSet5 :: (a -> encodedty)
             -> (b -> encodedty)
             -> (c -> encodedty)
             -> (d -> encodedty)
             -> (e -> encodedty)
             -> encty
             -> (a, b, c, d, e)
             -> encodedty

  encodeSet6 :: (a -> encodedty)
             -> (b -> encodedty)
             -> (c -> encodedty)
             -> (d -> encodedty)
             -> (e -> encodedty)
             -> (f -> encodedty)
             -> encty
             -> (a, b, c, d, e, f)
             -> encodedty

  encodeSet7 :: (a -> encodedty)
             -> (b -> encodedty)
             -> (c -> encodedty)
             -> (d -> encodedty)
             -> (e -> encodedty)
             -> (f -> encodedty)
             -> (g -> encodedty)
             -> encty
             -> (a, b, c, d, e, f, g)
             -> encodedty

  encodeSet8 :: (a -> encodedty)
             -> (b -> encodedty)
             -> (c -> encodedty)
             -> (d -> encodedty)
             -> (e -> encodedty)
             -> (f -> encodedty)
             -> (g -> encodedty)
             -> (h -> encodedty)
             -> encty
             -> (a, b, c, d, e, f, g, h)
             -> encodedty

  encodeSet9 :: (a -> encodedty)
             -> (b -> encodedty)
             -> (c -> encodedty)
             -> (d -> encodedty)
             -> (e -> encodedty)
             -> (f -> encodedty)
             -> (g -> encodedty)
             -> (h -> encodedty)
             -> (i -> encodedty)
             -> encty
             -> (a, b, c, d, e, f, g, h, i)
             -> encodedty

  encodeSet10 :: (a -> encodedty)
              -> (b -> encodedty)
              -> (c -> encodedty)
              -> (d -> encodedty)
              -> (e -> encodedty)
              -> (f -> encodedty)
              -> (g -> encodedty)
              -> (h -> encodedty)
              -> (i -> encodedty)
              -> (j -> encodedty)
              -> encty
              -> (a, b, c, d, e, f, g, h, i, j)
              -> encodedty

  encodeSet11 :: (a -> encodedty)
              -> (b -> encodedty)
              -> (c -> encodedty)
              -> (d -> encodedty)
              -> (e -> encodedty)
              -> (f -> encodedty)
              -> (g -> encodedty)
              -> (h -> encodedty)
              -> (i -> encodedty)
              -> (j -> encodedty)
              -> (k -> encodedty)
              -> encty
              -> (a, b, c, d, e, f, g, h, i, j, k)
              -> encodedty

  encodeSet12 :: (a -> encodedty)
              -> (b -> encodedty)
              -> (c -> encodedty)
              -> (d -> encodedty)
              -> (e -> encodedty)
              -> (f -> encodedty)
              -> (g -> encodedty)
              -> (h -> encodedty)
              -> (i -> encodedty)
              -> (j -> encodedty)
              -> (k -> encodedty)
              -> (l -> encodedty)
              -> encty
              -> (a, b, c, d, e, f, g, h, i, j, k, l)
              -> encodedty

  encodeSet13 :: (a -> encodedty)
              -> (b -> encodedty)
              -> (c -> encodedty)
              -> (d -> encodedty)
              -> (e -> encodedty)
              -> (f -> encodedty)
              -> (g -> encodedty)
              -> (h -> encodedty)
              -> (i -> encodedty)
              -> (j -> encodedty)
              -> (k -> encodedty)
              -> (l -> encodedty)
              -> (m -> encodedty)
              -> encty
              -> (a, b, c, d, e, f, g, h, i, j, k, l, m)
              -> encodedty

  encodeSet14 :: (a -> encodedty)
              -> (b -> encodedty)
              -> (c -> encodedty)
              -> (d -> encodedty)
              -> (e -> encodedty)
              -> (f -> encodedty)
              -> (g -> encodedty)
              -> (h -> encodedty)
              -> (i -> encodedty)
              -> (j -> encodedty)
              -> (k -> encodedty)
              -> (l -> encodedty)
              -> (m -> encodedty)
              -> (n -> encodedty)
              -> encty
              -> (a, b, c, d, e, f, g, h, i, j, k, l, m, n)
              -> encodedty

  encodeSet15 :: (a -> encodedty)
              -> (b -> encodedty)
              -> (c -> encodedty)
              -> (d -> encodedty)
              -> (e -> encodedty)
              -> (f -> encodedty)
              -> (g -> encodedty)
              -> (h -> encodedty)
              -> (i -> encodedty)
              -> (j -> encodedty)
              -> (k -> encodedty)
              -> (l -> encodedty)
              -> (m -> encodedty)
              -> (n -> encodedty)
              -> (o -> encodedty)
              -> encty
              -> (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o)
              -> encodedty

  encodeSet16 :: (a -> encodedty)
              -> (b -> encodedty)
              -> (c -> encodedty)
              -> (d -> encodedty)
              -> (e -> encodedty)
              -> (f -> encodedty)
              -> (g -> encodedty)
              -> (h -> encodedty)
              -> (i -> encodedty)
              -> (j -> encodedty)
              -> (k -> encodedty)
              -> (l -> encodedty)
              -> (m -> encodedty)
              -> (n -> encodedty)
              -> (o -> encodedty)
              -> (p -> encodedty)
              -> encty
              -> (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p)
              -> encodedty

  encodeSet17 :: (a -> encodedty)
              -> (b -> encodedty)
              -> (c -> encodedty)
              -> (d -> encodedty)
              -> (e -> encodedty)
              -> (f -> encodedty)
              -> (g -> encodedty)
              -> (h -> encodedty)
              -> (i -> encodedty)
              -> (j -> encodedty)
              -> (k -> encodedty)
              -> (l -> encodedty)
              -> (m -> encodedty)
              -> (n -> encodedty)
              -> (o -> encodedty)
              -> (p -> encodedty)
              -> (q -> encodedty)
              -> encty
              -> (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q)
              -> encodedty

  encodeSet18 :: (a -> encodedty)
              -> (b -> encodedty)
              -> (c -> encodedty)
              -> (d -> encodedty)
              -> (e -> encodedty)
              -> (f -> encodedty)
              -> (g -> encodedty)
              -> (h -> encodedty)
              -> (i -> encodedty)
              -> (j -> encodedty)
              -> (k -> encodedty)
              -> (l -> encodedty)
              -> (m -> encodedty)
              -> (n -> encodedty)
              -> (o -> encodedty)
              -> (p -> encodedty)
              -> (q -> encodedty)
              -> (r -> encodedty)
              -> encty
              -> (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r)
              -> encodedty

  encodeSet19 :: (a -> encodedty)
              -> (b -> encodedty)
              -> (c -> encodedty)
              -> (d -> encodedty)
              -> (e -> encodedty)
              -> (f -> encodedty)
              -> (g -> encodedty)
              -> (h -> encodedty)
              -> (i -> encodedty)
              -> (j -> encodedty)
              -> (k -> encodedty)
              -> (l -> encodedty)
              -> (m -> encodedty)
              -> (n -> encodedty)
              -> (o -> encodedty)
              -> (p -> encodedty)
              -> (q -> encodedty)
              -> (r -> encodedty)
              -> (s -> encodedty)
              -> encty
              -> (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s)
              -> encodedty

  encodeSet20 :: (a -> encodedty)
              -> (b -> encodedty)
              -> (c -> encodedty)
              -> (d -> encodedty)
              -> (e -> encodedty)
              -> (f -> encodedty)
              -> (g -> encodedty)
              -> (h -> encodedty)
              -> (i -> encodedty)
              -> (j -> encodedty)
              -> (k -> encodedty)
              -> (l -> encodedty)
              -> (m -> encodedty)
              -> (n -> encodedty)
              -> (o -> encodedty)
              -> (p -> encodedty)
              -> (q -> encodedty)
              -> (r -> encodedty)
              -> (s -> encodedty)
              -> (t -> encodedty)
              -> encty
              -> (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t)
              -> encodedty

  decodeBool :: encty
             -> encodedty
             -> decodedty Bool

  decodeUnboundedIntegral :: (Integral n) =>
                             encty
                          -> encodedty
                          -> decodedty n

  decodeBoundedIntegral :: (Integral n) =>
                           n
                        -- ^ The lower bound on the number.
                        -> n
                        -- ^ The upper bound on the number.
                        -> encty
                        -> encodedty
                        -> decodedty n

  decodeVariableBytes :: encty
                      -> encodedty
                      -> decodedty Strict.ByteString

  decodeVariableLazyBytes :: encty
                          -> encodedty
                          -> decodedty Lazy.ByteString

  decodeFixedBytes :: (Integral n) =>
                      n
                   -- ^ The length of the bytestring
                   -> encty
                   -> encodedty
                   -> decodedty Strict.ByteString

  decodeFixedLazyBytes :: (Integral n) =>
                          n
                       -- ^ The length of the bytestring
                       -> encty
                       -> encodedty
                       -> decodedty Lazy.ByteString

  decodeSeqOf :: (encodedty -> decodety a)
              -- ^ The encoding function for the inner type
              -> encty
              -> encodedty
              -> decodedty [a]

  decodeSetOf :: (encodedty -> decodety a)
              -- ^ The encoding function for the inner type
              -> encty
              -> encodedty
              -> decodedty [a]

  decodeOptional :: (encodedty -> decodety a)
                 -> encty
                 -> encodedty
                 -> decodedty (Maybe a)

  decodeChoice :: (encodedty -> decodedty a)
               -> encty
               -> encodedty
               -> decodedty a

  decodeStruct1 :: (encodedty -> decodedty a)
                -> encty
                -> encodedty
                -> decodedty a

  decodeStruct2 :: (encodedty -> decodedty a)
                -> (encodedty -> decodedty b)
                -> encty
                -> encodedty
                -> decodedty (a, b)

  decodeStruct3 :: (encodedty -> decodedty a)
                -> (encodedty -> decodedty b)
                -> (encodedty -> decodedty c)
                -> encty
                -> encodedty
                -> decodedty (a, b, c)

  decodeStruct4 :: (encodedty -> decodedty a)
                -> (encodedty -> decodedty b)
                -> (encodedty -> decodedty c)
                -> (encodedty -> decodedty d)
                -> encty
                -> encodedty
                -> decodedty (a, b, c, d)

  decodeStruct5 :: (encodedty -> decodedty a)
                -> (encodedty -> decodedty b)
                -> (encodedty -> decodedty c)
                -> (encodedty -> decodedty d)
                -> (encodedty -> decodedty e)
                -> encty
                -> encodedty
                -> decodedty (a, b, c, d, e)

  decodeStruct6 :: (encodedty -> decodedty a)
                -> (encodedty -> decodedty b)
                -> (encodedty -> decodedty c)
                -> (encodedty -> decodedty d)
                -> (encodedty -> decodedty e)
                -> (encodedty -> decodedty f)
                -> encty
                -> encodedty
                -> decodedty (a, b, c, d, e, f)

  decodeStruct7 :: (encodedty -> decodedty a)
                -> (encodedty -> decodedty b)
                -> (encodedty -> decodedty c)
                -> (encodedty -> decodedty d)
                -> (encodedty -> decodedty e)
                -> (encodedty -> decodedty f)
                -> (encodedty -> decodedty g)
                -> encty
                -> encodedty
                -> decodedty (a, b, c, d, e, f, g)

  decodeStruct8 :: (encodedty -> decodedty a)
                -> (encodedty -> decodedty b)
                -> (encodedty -> decodedty c)
                -> (encodedty -> decodedty d)
                -> (encodedty -> decodedty e)
                -> (encodedty -> decodedty f)
                -> (encodedty -> decodedty g)
                -> (encodedty -> decodedty h)
                -> encty
                -> encodedty
                -> decodedty (a, b, c, d, e, f, g, h)

  decodeStruct9 :: (encodedty -> decodedty a)
                -> (encodedty -> decodedty b)
                -> (encodedty -> decodedty c)
                -> (encodedty -> decodedty d)
                -> (encodedty -> decodedty e)
                -> (encodedty -> decodedty f)
                -> (encodedty -> decodedty g)
                -> (encodedty -> decodedty h)
                -> (encodedty -> decodedty i)
                -> encty
                -> encodedty
                -> decodedty (a, b, c, d, e, f, g, h, i)

  decodeStruct10 :: (encodedty -> decodedty a)
                 -> (encodedty -> decodedty b)
                 -> (encodedty -> decodedty c)
                 -> (encodedty -> decodedty d)
                 -> (encodedty -> decodedty e)
                 -> (encodedty -> decodedty f)
                 -> (encodedty -> decodedty g)
                 -> (encodedty -> decodedty h)
                 -> (encodedty -> decodedty i)
                 -> (encodedty -> decodedty j)
                 -> encty
                 -> encodedty
                 -> decodedty (a, b, c, d, e, f, g, h, i, j)

  decodeStruct11 :: (encodedty -> decodedty a)
                 -> (encodedty -> decodedty b)
                 -> (encodedty -> decodedty c)
                 -> (encodedty -> decodedty d)
                 -> (encodedty -> decodedty e)
                 -> (encodedty -> decodedty f)
                 -> (encodedty -> decodedty g)
                 -> (encodedty -> decodedty h)
                 -> (encodedty -> decodedty i)
                 -> (encodedty -> decodedty j)
                 -> (encodedty -> decodedty k)
                 -> encty
                 -> encodedty
                 -> decodedty (a, b, c, d, e, f, g, h, i, j, k)

  decodeStruct12 :: (encodedty -> decodedty a)
                 -> (encodedty -> decodedty b)
                 -> (encodedty -> decodedty c)
                 -> (encodedty -> decodedty d)
                 -> (encodedty -> decodedty e)
                 -> (encodedty -> decodedty f)
                 -> (encodedty -> decodedty g)
                 -> (encodedty -> decodedty h)
                 -> (encodedty -> decodedty i)
                 -> (encodedty -> decodedty j)
                 -> (encodedty -> decodedty k)
                 -> (encodedty -> decodedty l)
                 -> encty
                 -> encodedty
                 -> decodedty (a, b, c, d, e, f, g, h, i, j, k, l)

  decodeStruct13 :: (encodedty -> decodedty a)
                 -> (encodedty -> decodedty b)
                 -> (encodedty -> decodedty c)
                 -> (encodedty -> decodedty d)
                 -> (encodedty -> decodedty e)
                 -> (encodedty -> decodedty f)
                 -> (encodedty -> decodedty g)
                 -> (encodedty -> decodedty h)
                 -> (encodedty -> decodedty i)
                 -> (encodedty -> decodedty j)
                 -> (encodedty -> decodedty k)
                 -> (encodedty -> decodedty l)
                 -> (encodedty -> decodedty m)
                 -> encty
                 -> encodedty
                 -> decodedty (a, b, c, d, e, f, g, h, i, j, k, l, m)

  decodeStruct14 :: (encodedty -> decodedty a)
                 -> (encodedty -> decodedty b)
                 -> (encodedty -> decodedty c)
                 -> (encodedty -> decodedty d)
                 -> (encodedty -> decodedty e)
                 -> (encodedty -> decodedty f)
                 -> (encodedty -> decodedty g)
                 -> (encodedty -> decodedty h)
                 -> (encodedty -> decodedty i)
                 -> (encodedty -> decodedty j)
                 -> (encodedty -> decodedty k)
                 -> (encodedty -> decodedty l)
                 -> (encodedty -> decodedty m)
                 -> (encodedty -> decodedty n)
                 -> encty
                 -> encodedty
                 -> decodedty (a, b, c, d, e, f, g, h, i, j, k, l, m, n)

  decodeStruct15 :: (encodedty -> decodedty a)
                 -> (encodedty -> decodedty b)
                 -> (encodedty -> decodedty c)
                 -> (encodedty -> decodedty d)
                 -> (encodedty -> decodedty e)
                 -> (encodedty -> decodedty f)
                 -> (encodedty -> decodedty g)
                 -> (encodedty -> decodedty h)
                 -> (encodedty -> decodedty i)
                 -> (encodedty -> decodedty j)
                 -> (encodedty -> decodedty k)
                 -> (encodedty -> decodedty l)
                 -> (encodedty -> decodedty m)
                 -> (encodedty -> decodedty n)
                 -> (encodedty -> decodedty o)
                 -> encty
                 -> encodedty
                 -> decodedty (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o)

  decodeStruct16 :: (encodedty -> decodedty a)
                 -> (encodedty -> decodedty b)
                 -> (encodedty -> decodedty c)
                 -> (encodedty -> decodedty d)
                 -> (encodedty -> decodedty e)
                 -> (encodedty -> decodedty f)
                 -> (encodedty -> decodedty g)
                 -> (encodedty -> decodedty h)
                 -> (encodedty -> decodedty i)
                 -> (encodedty -> decodedty j)
                 -> (encodedty -> decodedty k)
                 -> (encodedty -> decodedty l)
                 -> (encodedty -> decodedty m)
                 -> (encodedty -> decodedty n)
                 -> (encodedty -> decodedty o)
                 -> (encodedty -> decodedty p)
                 -> encty
                 -> encodedty
                 -> decodedty (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p)

  decodeStruct17 :: (encodedty -> decodedty a)
                 -> (encodedty -> decodedty b)
                 -> (encodedty -> decodedty c)
                 -> (encodedty -> decodedty d)
                 -> (encodedty -> decodedty e)
                 -> (encodedty -> decodedty f)
                 -> (encodedty -> decodedty g)
                 -> (encodedty -> decodedty h)
                 -> (encodedty -> decodedty i)
                 -> (encodedty -> decodedty j)
                 -> (encodedty -> decodedty k)
                 -> (encodedty -> decodedty l)
                 -> (encodedty -> decodedty m)
                 -> (encodedty -> decodedty n)
                 -> (encodedty -> decodedty o)
                 -> (encodedty -> decodedty p)
                 -> (encodedty -> decodedty q)
                 -> encty
                 -> encodedty
                 -> decodedty (a, b, c, d, e, f, g, h, i, j,
                               k, l, m, n, o, p, q)

  decodeStruct18 :: (encodedty -> decodedty a)
                 -> (encodedty -> decodedty b)
                 -> (encodedty -> decodedty c)
                 -> (encodedty -> decodedty d)
                 -> (encodedty -> decodedty e)
                 -> (encodedty -> decodedty f)
                 -> (encodedty -> decodedty g)
                 -> (encodedty -> decodedty h)
                 -> (encodedty -> decodedty i)
                 -> (encodedty -> decodedty j)
                 -> (encodedty -> decodedty k)
                 -> (encodedty -> decodedty l)
                 -> (encodedty -> decodedty m)
                 -> (encodedty -> decodedty n)
                 -> (encodedty -> decodedty o)
                 -> (encodedty -> decodedty p)
                 -> (encodedty -> decodedty q)
                 -> (encodedty -> decodedty r)
                 -> encty
                 -> encodedty
                 -> decodedty (a, b, c, d, e, f, g, h, i, j,
                               k, l, m, n, o, p, q, r)

  decodeStruct19 :: (encodedty -> decodedty a)
                 -> (encodedty -> decodedty b)
                 -> (encodedty -> decodedty c)
                 -> (encodedty -> decodedty d)
                 -> (encodedty -> decodedty e)
                 -> (encodedty -> decodedty f)
                 -> (encodedty -> decodedty g)
                 -> (encodedty -> decodedty h)
                 -> (encodedty -> decodedty i)
                 -> (encodedty -> decodedty j)
                 -> (encodedty -> decodedty k)
                 -> (encodedty -> decodedty l)
                 -> (encodedty -> decodedty m)
                 -> (encodedty -> decodedty n)
                 -> (encodedty -> decodedty o)
                 -> (encodedty -> decodedty p)
                 -> (encodedty -> decodedty q)
                 -> (encodedty -> decodedty r)
                 -> (encodedty -> decodedty s)
                 -> encty
                 -> encodedty
                 -> decodedty (a, b, c, d, e, f, g, h, i, j,
                               k, l, m, n, o, p, q, r, s)

  decodeStruct20 :: (encodedty -> decodedty a)
                 -> (encodedty -> decodedty b)
                 -> (encodedty -> decodedty c)
                 -> (encodedty -> decodedty d)
                 -> (encodedty -> decodedty e)
                 -> (encodedty -> decodedty f)
                 -> (encodedty -> decodedty g)
                 -> (encodedty -> decodedty h)
                 -> (encodedty -> decodedty i)
                 -> (encodedty -> decodedty j)
                 -> (encodedty -> decodedty k)
                 -> (encodedty -> decodedty l)
                 -> (encodedty -> decodedty m)
                 -> (encodedty -> decodedty n)
                 -> (encodedty -> decodedty o)
                 -> (encodedty -> decodedty p)
                 -> (encodedty -> decodedty q)
                 -> (encodedty -> decodedty r)
                 -> (encodedty -> decodedty s)
                 -> (encodedty -> decodedty t)
                 -> encty
                 -> encodedty
                 -> decodedty (a, b, c, d, e, f, g, h, i, j,
                               k, l, m, n, o, p, q, r, s, t)

  decodeSet2 :: (encodedty -> decodedty a)
             -> (encodedty -> decodedty b)
             -> encty
             -> encodedty
             -> decodedty (a, b)

  decodeSet3 :: (encodedty -> decodedty a)
             -> (encodedty -> decodedty b)
             -> (encodedty -> decodedty c)
             -> encty
             -> encodedty
             -> decodedty (a, b, c)

  decodeSet4 :: (encodedty -> decodedty a)
             -> (encodedty -> decodedty b)
             -> (encodedty -> decodedty c)
             -> (encodedty -> decodedty d)
             -> encty
             -> encodedty
             -> decodedty (a, b, c, d)

  decodeSet5 :: (encodedty -> decodedty a)
             -> (encodedty -> decodedty b)
             -> (encodedty -> decodedty c)
             -> (encodedty -> decodedty d)
             -> (encodedty -> decodedty e)
             -> encty
             -> encodedty
             -> decodedty (a, b, c, d, e)

  decodeSet6 :: (encodedty -> decodedty a)
             -> (encodedty -> decodedty b)
             -> (encodedty -> decodedty c)
             -> (encodedty -> decodedty d)
             -> (encodedty -> decodedty e)
             -> (encodedty -> decodedty f)
             -> encty
             -> encodedty
             -> decodedty (a, b, c, d, e, f)

  decodeSet7 :: (encodedty -> decodedty a)
             -> (encodedty -> decodedty b)
             -> (encodedty -> decodedty c)
             -> (encodedty -> decodedty d)
             -> (encodedty -> decodedty e)
             -> (encodedty -> decodedty f)
             -> (encodedty -> decodedty g)
             -> encty
             -> encodedty
             -> decodedty (a, b, c, d, e, f, g)

  decodeSet8 :: (encodedty -> decodedty a)
             -> (encodedty -> decodedty b)
             -> (encodedty -> decodedty c)
             -> (encodedty -> decodedty d)
             -> (encodedty -> decodedty e)
             -> (encodedty -> decodedty f)
             -> (encodedty -> decodedty g)
             -> (encodedty -> decodedty h)
             -> encty
             -> encodedty
             -> decodedty (a, b, c, d, e, f, g, h)

  decodeSet9 :: (encodedty -> decodedty a)
             -> (encodedty -> decodedty b)
             -> (encodedty -> decodedty c)
             -> (encodedty -> decodedty d)
             -> (encodedty -> decodedty e)
             -> (encodedty -> decodedty f)
             -> (encodedty -> decodedty g)
             -> (encodedty -> decodedty h)
             -> (encodedty -> decodedty i)
             -> encty
             -> encodedty
             -> decodedty (a, b, c, d, e, f, g, h, i)

  decodeSet10 :: (encodedty -> decodedty a)
              -> (encodedty -> decodedty b)
              -> (encodedty -> decodedty c)
              -> (encodedty -> decodedty d)
              -> (encodedty -> decodedty e)
              -> (encodedty -> decodedty f)
              -> (encodedty -> decodedty g)
              -> (encodedty -> decodedty h)
              -> (encodedty -> decodedty i)
              -> (encodedty -> decodedty j)
              -> encty
              -> encodedty
              -> decodedty (a, b, c, d, e, f, g, h, i, j)

  decodeSet11 :: (encodedty -> decodedty a)
              -> (encodedty -> decodedty b)
              -> (encodedty -> decodedty c)
              -> (encodedty -> decodedty d)
              -> (encodedty -> decodedty e)
              -> (encodedty -> decodedty f)
              -> (encodedty -> decodedty g)
              -> (encodedty -> decodedty h)
              -> (encodedty -> decodedty i)
              -> (encodedty -> decodedty j)
              -> (encodedty -> decodedty k)
              -> encty
              -> encodedty
              -> decodedty (a, b, c, d, e, f, g, h, i, j, k)

  decodeSet12 :: (encodedty -> decodedty a)
              -> (encodedty -> decodedty b)
              -> (encodedty -> decodedty c)
              -> (encodedty -> decodedty d)
              -> (encodedty -> decodedty e)
              -> (encodedty -> decodedty f)
              -> (encodedty -> decodedty g)
              -> (encodedty -> decodedty h)
              -> (encodedty -> decodedty i)
              -> (encodedty -> decodedty j)
              -> (encodedty -> decodedty k)
              -> (encodedty -> decodedty l)
              -> encty
              -> encodedty
              -> decodedty (a, b, c, d, e, f, g, h, i, j, k, l)

  decodeSet13 :: (encodedty -> decodedty a)
              -> (encodedty -> decodedty b)
              -> (encodedty -> decodedty c)
              -> (encodedty -> decodedty d)
              -> (encodedty -> decodedty e)
              -> (encodedty -> decodedty f)
              -> (encodedty -> decodedty g)
              -> (encodedty -> decodedty h)
              -> (encodedty -> decodedty i)
              -> (encodedty -> decodedty j)
              -> (encodedty -> decodedty k)
              -> (encodedty -> decodedty l)
              -> (encodedty -> decodedty m)
              -> encty
              -> encodedty
              -> decodedty (a, b, c, d, e, f, g, h, i, j, k, l, m)

  decodeSet14 :: (encodedty -> decodedty a)
              -> (encodedty -> decodedty b)
              -> (encodedty -> decodedty c)
              -> (encodedty -> decodedty d)
              -> (encodedty -> decodedty e)
              -> (encodedty -> decodedty f)
              -> (encodedty -> decodedty g)
              -> (encodedty -> decodedty h)
              -> (encodedty -> decodedty i)
              -> (encodedty -> decodedty j)
              -> (encodedty -> decodedty k)
              -> (encodedty -> decodedty l)
              -> (encodedty -> decodedty m)
              -> (encodedty -> decodedty n)
              -> encty
              -> encodedty
              -> decodedty (a, b, c, d, e, f, g, h, i, j, k, l, m, n)

  decodeSet15 :: (encodedty -> decodedty a)
              -> (encodedty -> decodedty b)
              -> (encodedty -> decodedty c)
              -> (encodedty -> decodedty d)
              -> (encodedty -> decodedty e)
              -> (encodedty -> decodedty f)
              -> (encodedty -> decodedty g)
              -> (encodedty -> decodedty h)
              -> (encodedty -> decodedty i)
              -> (encodedty -> decodedty j)
              -> (encodedty -> decodedty k)
              -> (encodedty -> decodedty l)
              -> (encodedty -> decodedty m)
              -> (encodedty -> decodedty n)
              -> (encodedty -> decodedty o)
              -> encty
              -> encodedty
              -> decodedty (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o)

  decodeSet16 :: (encodedty -> decodedty a)
              -> (encodedty -> decodedty b)
              -> (encodedty -> decodedty c)
              -> (encodedty -> decodedty d)
              -> (encodedty -> decodedty e)
              -> (encodedty -> decodedty f)
              -> (encodedty -> decodedty g)
              -> (encodedty -> decodedty h)
              -> (encodedty -> decodedty i)
              -> (encodedty -> decodedty j)
              -> (encodedty -> decodedty k)
              -> (encodedty -> decodedty l)
              -> (encodedty -> decodedty m)
              -> (encodedty -> decodedty n)
              -> (encodedty -> decodedty o)
              -> (encodedty -> decodedty p)
              -> encty
              -> encodedty
              -> decodedty (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p)

  decodeSet17 :: (encodedty -> decodedty a)
              -> (encodedty -> decodedty b)
              -> (encodedty -> decodedty c)
              -> (encodedty -> decodedty d)
              -> (encodedty -> decodedty e)
              -> (encodedty -> decodedty f)
              -> (encodedty -> decodedty g)
              -> (encodedty -> decodedty h)
              -> (encodedty -> decodedty i)
              -> (encodedty -> decodedty j)
              -> (encodedty -> decodedty k)
              -> (encodedty -> decodedty l)
              -> (encodedty -> decodedty m)
              -> (encodedty -> decodedty n)
              -> (encodedty -> decodedty o)
              -> (encodedty -> decodedty p)
              -> (encodedty -> decodedty q)
              -> encty
              -> encodedty
              -> decodedty (a, b, c, d, e, f, g, h, i, j,
                            k, l, m, n, o, p, q)

  decodeSet18 :: (encodedty -> decodedty a)
              -> (encodedty -> decodedty b)
              -> (encodedty -> decodedty c)
              -> (encodedty -> decodedty d)
              -> (encodedty -> decodedty e)
              -> (encodedty -> decodedty f)
              -> (encodedty -> decodedty g)
              -> (encodedty -> decodedty h)
              -> (encodedty -> decodedty i)
              -> (encodedty -> decodedty j)
              -> (encodedty -> decodedty k)
              -> (encodedty -> decodedty l)
              -> (encodedty -> decodedty m)
              -> (encodedty -> decodedty n)
              -> (encodedty -> decodedty o)
              -> (encodedty -> decodedty p)
              -> (encodedty -> decodedty q)
              -> (encodedty -> decodedty r)
              -> encty
              -> encodedty
              -> decodedty (a, b, c, d, e, f, g, h, i, j,
                            k, l, m, n, o, p, q, r)

  decodeSet19 :: (encodedty -> decodedty a)
              -> (encodedty -> decodedty b)
              -> (encodedty -> decodedty c)
              -> (encodedty -> decodedty d)
              -> (encodedty -> decodedty e)
              -> (encodedty -> decodedty f)
              -> (encodedty -> decodedty g)
              -> (encodedty -> decodedty h)
              -> (encodedty -> decodedty i)
              -> (encodedty -> decodedty j)
              -> (encodedty -> decodedty k)
              -> (encodedty -> decodedty l)
              -> (encodedty -> decodedty m)
              -> (encodedty -> decodedty n)
              -> (encodedty -> decodedty o)
              -> (encodedty -> decodedty p)
              -> (encodedty -> decodedty q)
              -> (encodedty -> decodedty r)
              -> (encodedty -> decodedty s)
              -> encty
              -> encodedty
              -> decodedty (a, b, c, d, e, f, g, h, i, j,
                            k, l, m, n, o, p, q, r, s)

  decodeSet20 :: (encodedty -> decodedty a)
              -> (encodedty -> decodedty b)
              -> (encodedty -> decodedty c)
              -> (encodedty -> decodedty d)
              -> (encodedty -> decodedty e)
              -> (encodedty -> decodedty f)
              -> (encodedty -> decodedty g)
              -> (encodedty -> decodedty h)
              -> (encodedty -> decodedty i)
              -> (encodedty -> decodedty j)
              -> (encodedty -> decodedty k)
              -> (encodedty -> decodedty l)
              -> (encodedty -> decodedty m)
              -> (encodedty -> decodedty n)
              -> (encodedty -> decodedty o)
              -> (encodedty -> decodedty p)
              -> (encodedty -> decodedty q)
              -> (encodedty -> decodedty r)
              -> (encodedty -> decodedty s)
              -> (encodedty -> decodedty t)
              -> encty
              -> encodedty
              -> decodedty (a, b, c, d, e, f, g, h, i, j,
                            k, l, m, n, o, p, q, r, s, t)

class EncodingDesc descty => GenericEncodingDesc descty where
  unitDesc :: descty

  boolDesc :: descty

  unboundedIntegralDesc :: descty

  boundedIntegralDesc :: (Integral n) =>
                         n
                      -- ^ The lower bound on the number.
                      -> n
                      -- ^ The upper bound on the number.
                      -> descty

  variableBytesDesc :: descty

  fixedBytesDesc :: (Integral n) =>
                    n
                 -- ^ The length of the bytestring.
                 -> descty

  sequenceOfDesc :: descty
                 -- ^ Element descriptor.
                 -> descty

  setOfDesc :: descty
            -- ^ Element descriptor.
            -> descty

  optionalDesc :: descty
               -- ^ Element descriptor.
               -> descty

  choiceDesc :: [(String, descty)]
             -> descty

  structDesc :: [(String, descty)]
             -> descty

  setDesc :: [(String, descty)]
          -> descty
