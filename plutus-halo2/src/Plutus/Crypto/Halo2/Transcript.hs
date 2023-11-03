{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE Strict             #-}

module Plutus.Crypto.Halo2.Transcript 
( Transcript
, squeezeChallange
, addScalarToTranscript
, addPointToTranscript
) where

import PlutusTx.Prelude
    ( BuiltinBLS12_381_G1_Element,
      BuiltinByteString,
      ($),
      (.),
      fst,
      snd,
      blake2b_256,
      byteStringToInteger,
      integerToByteString,
      modulo,
      Semigroup((<>)) ) 
import PlutusTx.Builtins
    ( BuiltinBLS12_381_G1_Element,
      BuiltinByteString,
      blake2b_256,
      byteStringToInteger,
      integerToByteString ) 
import Plutus.Crypto.BlsUtils
    ( Scalar(..),
      bls12_381_field_prime,
      Fp(unFp),
      mkScalar,
      unCompressG1Point )

type Transcript = BuiltinByteString

{-# INLINEABLE squeezeChallange #-}
squeezeChallange :: Transcript -> (Scalar, Transcript)
squeezeChallange bs = (mkScalar (byteStringToInteger (blake2b_256 (bs <> "\x00")) `modulo` bls12_381_field_prime), bs <> "\x00")

{-# INLINEABLE addScalarToTranscript #-}
addScalarToTranscript :: Transcript -> Scalar -> Transcript
addScalarToTranscript bs s = bs <> "\x02" <> integerToByteString (unScalar s)

{-# INLINEABLE addPointToTranscript #-}
addPointToTranscript :: Transcript -> BuiltinBLS12_381_G1_Element -> Transcript
addPointToTranscript bs p = bs <> "\x01" <> (integerToByteString . unFp) x 
                                         <> (integerToByteString . unFp) y
                                         where (x,y) = unCompressG1Point p