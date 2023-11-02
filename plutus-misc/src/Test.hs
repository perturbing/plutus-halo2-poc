{-# LANGUAGE NoImplicitPrelude      #-}
{-# LANGUAGE BinaryLiterals         #-}
{-# LANGUAGE OverloadedStrings      #-}
-- {-# LANGUAGE TemplateHaskell        #-}
-- {-# LANGUAGE ScopedTypeVariables    #-}
-- {-# LANGUAGE MultiParamTypeClasses  #-}
-- {-# LANGUAGE InstanceSigs           #-}
{-# LANGUAGE Strict                 #-}

module Test where

import Plutus.Crypto.BlsUtils
import PlutusTx.Prelude
import PlutusTx.Builtins

-- this is the integer representation of 0x72c22a0af5412672924ce854e121221e3a00715f31a2450c18fa0daa78de0295
transcriptRepr :: Scalar
transcriptRepr = mkScalar (0x72c22a0af5412672924ce854e121221e3a00715f31a2450c18fa0daa78de0295 `modulo` bls12_381_field_prime)

a1 :: BuiltinBLS12_381_G1_Element
a1 = compressG1Point (mkFp 0x034f19ebad2014e91fd16ff2673ceb5d8ed6765be7c692d660fc80aa0639cbdc984d4b8059e83f4191f175d4a06c71bf, mkFp 0x187d2ba8f20c7c41ae04987673b8c79607111c3e765d76e8f34c0e3f4c2bb8f7a88105b45872485889f1dbe3c4d06e02) 

a2 :: BuiltinBLS12_381_G1_Element
a2 = compressG1Point (mkFp 0x0cd04b4b48c69671c127f89081064cdbf49583473472b0902f00425166b20113302c1ebba96b66aa77945a6e3af1e2fd, mkFp 0x0d8038c81212112e65e6ee7cd3ae3ab6aa6e143d8ce15c038486e2c8b344571e5cab2ebcedd1db3c03bc9546a6b7b0ad)

a3 :: BuiltinBLS12_381_G1_Element
a3 = compressG1Point (mkFp 0x1226eb09ec81f1e8d7676594c7f3d5a9ac6d2b5d875634e4b6b7ac8d346cbf6d475c5770a130289d1c58891041b6fd53, mkFp 0x03f5e9542ded4dc45d7b0797a60a2924c520b419cb901f68bf2c6d60a3d4d78646e181cac9e3fa30e29190bee44d184b)

type Transcript = BuiltinByteString

squeezeChallange :: Transcript -> (Scalar, Transcript)
squeezeChallange bs = (mkScalar (byteStringToInteger (blake2b_256 (bs <> "\x00")) `modulo` bls12_381_field_prime), bs <> "\x00")

addScalar :: Transcript -> Scalar -> Transcript
addScalar bs s = bs <> "\x02" <> integerToByteString (unScalar s)

addPoint :: Transcript -> BuiltinBLS12_381_G1_Element -> Transcript
addPoint bs p = bs <> "\x01" <> integerToByteString x 
                             <> integerToByteString y
                where x = unFp . fst . unCompressG1Point $ p
                      y = unFp . snd . unCompressG1Point $ p

transcript0 :: Transcript
transcript0 = emptyByteString

transcript1 :: Transcript
transcript1 = addScalar transcript0 transcriptRepr

transcript2 :: Transcript
transcript2 = addPoint transcript1 a1

transcript3 :: Transcript
transcript3 = addPoint transcript2 a2

transcript4 :: Transcript
transcript4 = addPoint transcript3 a3

(theta,transcript5) = squeezeChallange transcript4
thetaRef = 0x63e29cf3ec919f116af2f52aca45057200c3e162193e13dd538a8a2a65f160f4