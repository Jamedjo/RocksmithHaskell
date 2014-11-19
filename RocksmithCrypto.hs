module RocksmithCrypto (decryptPsarc, encryptPsarc) where

import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Codec.Crypto.AES as AES

psarcKey = B.pack [0xC5, 0x3D, 0xB2, 0x38, 0x70, 0xA1, 0xA2, 0xF7,
            0x1C, 0xAE, 0x64, 0x06, 0x1F, 0xDD, 0x0E, 0x11,
            0x57, 0x30, 0x9D, 0xC8, 0x52, 0x04, 0xD4, 0xC5,
            0xBF, 0xDF, 0x25, 0x09, 0x0D, 0xF2, 0x57, 0x2C]

decryptPsarc :: B.ByteString -> String
decryptPsarc = utf16ToString . rocksmithDecrypt psarcKey
	where utf16ToString = T.unpack . TE.decodeUtf16LE

encryptPsarc :: String -> B.ByteString
encryptPsarc = rocksmithEncrypt psarcKey . stringToUtf16
	where stringToUtf16 = TE.encodeUtf16LE . T.pack


rocksmithDecrypt :: B.ByteString -> B.ByteString -> B.ByteString
rocksmithDecrypt = rocksmithCrypt AES.Decrypt

rocksmithEncrypt :: B.ByteString -> B.ByteString -> B.ByteString
rocksmithEncrypt = rocksmithCrypt AES.Encrypt

rocksmithCrypt :: AES.Direction -> B.ByteString -> B.ByteString -> B.ByteString
rocksmithCrypt direction key = AES.crypt' AES.CFB key iv direction
	where iv = B.pack $ take 16 $ repeat 0x00

