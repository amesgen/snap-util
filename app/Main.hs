module Main (main) where

import Codec.CBOR.Encoding qualified as CBOR (encodeMapLen)
import Codec.CBOR.FlatTerm qualified as CBOR
import Codec.CBOR.Read qualified as CBOR
import Codec.CBOR.Write qualified as CBOR
import Control.Monad.Except
import Data.ByteString.Builder qualified as BB
import Data.ByteString.Lazy qualified as BL
import Data.Word (Word64)
import System.Environment

main :: IO ()
main =
  getArgs >>= \case
    ["check", file] -> do
      bs <- BL.readFile file
      let ctxToks = withCtx $ parseCBORTermTokens bs
      putStrLn $ assess ctxToks
    ["fixup", file, fileOut] -> do
      bs <- BL.readFile file
      let ctxToks = withCtx $ parseCBORTermTokens bs
      BB.writeFile fileOut $ fixup ctxToks
    _ -> fail "unexpected cli args"

assess :: [(Ctx, CBOR.TermToken, a)] -> String
assess ctxToks = case skipToPointers ctxToks of
  [] -> "Not a Conway ledger state"
  (_, CBOR.TkMapLen len, _) : _
    | len > 0 -> "Bad pointer addresses present"
    | otherwise -> "Bad pointer addresses NOT present"
  _ -> error "Unexpected format"
  where
    skipToPointers = dropWhile \(ctx, _, _) -> ctx /= pointerCtx

fixup :: [(Ctx, CBOR.TermToken, BL.ByteString)] -> BB.Builder
fixup = go True
  where
    go writeEmptyMapLen = \case
      [] -> mempty
      (ctx, _, bs) : rest
        | isInPointerCtx ctx,
          let bs' =
                if writeEmptyMapLen
                  then CBOR.toBuilder $ CBOR.encodeMapLen 0
                  else mempty ->
            bs' <> go False rest
        | otherwise -> BB.lazyByteString bs <> go writeEmptyMapLen rest

parseCBORTermTokens :: BL.ByteString -> [(CBOR.TermToken, BL.ByteString)]
parseCBORTermTokens bs
  | BL.null bs = []
  | otherwise = case CBOR.deserialiseFromBytesWithSize CBOR.decodeTermToken bs of
      Left e -> error $ show e
      Right (bs', off, tok) -> (tok, BL.take off bs) : parseCBORTermTokens bs'

pointerCtx :: Ctx
pointerCtx = CtxListN {len = 2, ix = 1, ctx = CtxListN {len = 6, ix = 4, ctx = CtxListN {len = 2, ix = 1, ctx = CtxListN {len = 4, ix = 1, ctx = CtxListN {len = 7, ix = 3, ctx = CtxListN {len = 3, ix = 1, ctx = CtxListN {len = 2, ix = 1, ctx = CtxListN {len = 2, ix = 1, ctx = CtxListN {len = 7, ix = 6, ctx = CtxListN {len = 2, ix = 0, ctx = CtxListN {len = 2, ix = 1, ctx = CtxTop}}}}}}}}}}}

isInPointerCtx :: Ctx -> Bool
isInPointerCtx ctx =
  ctx == pointerCtx || case subCtx ctx of
    Nothing -> False
    Just ctx' -> isInPointerCtx ctx'

subCtx :: Ctx -> Maybe Ctx
subCtx = \case
  CtxTop -> Nothing
  CtxBytes {ctx} -> Just ctx
  CtxString {ctx} -> Just ctx
  CtxListN {ctx} -> Just ctx
  CtxList {ctx} -> Just ctx
  CtxMapNKey {ctx} -> Just ctx
  CtxMapNVal {ctx} -> Just ctx
  CtxMapKey {ctx} -> Just ctx
  CtxMapVal {ctx} -> Just ctx
  CtxTagged {ctx} -> Just ctx

data Ctx
  = CtxTop
  | CtxBytes {ix :: Word64, ctx :: Ctx}
  | CtxString {ix :: Word64, ctx :: Ctx}
  | CtxListN {len, ix :: Word64, ctx :: Ctx}
  | CtxList {ix :: Word64, ctx :: Ctx}
  | CtxMapNKey {len, ix :: Word64, ctx :: Ctx}
  | CtxMapNVal {len, ix :: Word64, ctx :: Ctx}
  | CtxMapKey {ix :: Word64, ctx :: Ctx}
  | CtxMapVal {ix :: Word64, ctx :: Ctx}
  | CtxTagged {tag :: Word64, ctx :: Ctx}
  deriving stock (Show, Eq)

collapseCtx :: Ctx -> Ctx
collapseCtx c = case c of
  CtxTop -> c
  CtxBytes {} -> c
  CtxString {} -> c
  CtxListN {len, ix, ctx}
    | ix + 1 < len -> c
    | otherwise -> collapseCtx ctx
  CtxList {} -> c
  CtxMapNKey {} -> c
  CtxMapNVal {len, ix, ctx}
    | ix + 1 < len -> c
    | otherwise -> collapseCtx ctx
  CtxMapKey {} -> c
  CtxMapVal {} -> c
  CtxTagged {ctx} -> collapseCtx ctx

withCtx ::
  forall a.
  [(CBOR.TermToken, a)] -> [(Ctx, CBOR.TermToken, a)]
withCtx = go CtxTop
  where
    go :: Ctx -> [(CBOR.TermToken, a)] -> [(Ctx, CBOR.TermToken, a)]
    go _ [] = []
    go !ctx ((tok, a) : rest) = either error id do
      let failCtx =
            throwError $ "unexpected context " <> show ctx <> " for " <> show tok

          advanceCtx ctx = case ctx of
            CtxTop -> pure CtxTop
            CtxBytes {} -> failCtx
            CtxString {} -> failCtx
            CtxListN {len, ix, ctx} -> pure CtxListN {len, ix = ix + 1, ctx}
            CtxList {ix, ctx} -> pure CtxList {ix = ix + 1, ctx}
            CtxMapNKey {len, ix, ctx} -> pure CtxMapNVal {len, ix, ctx}
            CtxMapNVal {len, ix, ctx} -> pure CtxMapNKey {len, ix = ix + 1, ctx}
            CtxMapKey {ix, ctx} -> pure CtxMapVal {ix, ctx}
            CtxMapVal {ix, ctx} -> pure CtxMapKey {ix = ix + 1, ctx}
            CtxTagged {} -> pure ctx

      ctx' <- case tok of
        CBOR.TkInt _ -> advanceCtx $ collapseCtx ctx
        CBOR.TkInteger _ -> advanceCtx $ collapseCtx ctx
        CBOR.TkBytes _ -> case ctx of
          CtxBytes {ix, ctx} -> pure CtxBytes {ix = ix + 1, ctx}
          _ -> advanceCtx $ collapseCtx ctx
        CBOR.TkBytesBegin -> pure CtxBytes {ix = 0, ctx}
        CBOR.TkString _ -> case ctx of
          CtxString {ix, ctx} -> pure CtxString {ix = ix + 1, ctx}
          _ -> advanceCtx $ collapseCtx ctx
        CBOR.TkStringBegin -> pure CtxString {ix = 0, ctx}
        CBOR.TkListLen len
          | len == 0 -> advanceCtx $ collapseCtx ctx
          | otherwise -> pure CtxListN {len = wordToWord64 len, ix = 0, ctx}
        CBOR.TkListBegin -> pure CtxList {ix = 0, ctx}
        CBOR.TkMapLen len
          | len == 0 -> advanceCtx $ collapseCtx ctx
          | otherwise -> pure CtxMapNKey {len = wordToWord64 len, ix = 0, ctx}
        CBOR.TkMapBegin -> pure CtxMapKey {ix = 0, ctx}
        CBOR.TkBreak -> case ctx of
          CtxList {ctx} -> advanceCtx $ collapseCtx ctx
          CtxMapKey {ctx} -> advanceCtx $ collapseCtx ctx
          _ -> failCtx
        CBOR.TkTag tag -> pure CtxTagged {tag, ctx}
        CBOR.TkBool _ -> advanceCtx $ collapseCtx ctx
        CBOR.TkNull -> advanceCtx $ collapseCtx ctx
        CBOR.TkSimple _ -> advanceCtx $ collapseCtx ctx
        CBOR.TkFloat16 _ -> advanceCtx $ collapseCtx ctx
        CBOR.TkFloat32 _ -> advanceCtx $ collapseCtx ctx
        CBOR.TkFloat64 _ -> advanceCtx $ collapseCtx ctx

      case ctx' of
        CtxTop -> pure [(ctx, tok, a)]
        _ -> pure $ (ctx, tok, a) : go ctx' rest

wordToWord64 :: Word -> Word64
wordToWord64 = fromIntegral
{-# INLINE wordToWord64 #-}
