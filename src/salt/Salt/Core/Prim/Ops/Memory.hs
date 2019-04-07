
module Salt.Core.Prim.Ops.Memory where
import Salt.Core.Prim.Ops.Base
import qualified Data.Int              as Int
import qualified Data.Word             as Word
import qualified Foreign.Marshal.Alloc as Alloc
import qualified Foreign.Ptr           as Ptr
import qualified Foreign.Storable      as Storable

primSizeOf :: Type a -> Word.Word64
primSizeOf TBool        = fromIntegral $ Storable.sizeOf (undefined :: Bool)
primSizeOf TInt8        = fromIntegral $ Storable.sizeOf (undefined :: Int.Int8)
primSizeOf TInt16       = fromIntegral $ Storable.sizeOf (undefined :: Int.Int16)
primSizeOf TInt32       = fromIntegral $ Storable.sizeOf (undefined :: Int.Int32)
primSizeOf TInt64       = fromIntegral $ Storable.sizeOf (undefined :: Int.Int64)
primSizeOf TWord8       = fromIntegral $ Storable.sizeOf (undefined :: Word.Word8)
primSizeOf TWord16      = fromIntegral $ Storable.sizeOf (undefined :: Word.Word16)
primSizeOf TWord32      = fromIntegral $ Storable.sizeOf (undefined :: Word.Word32)
primSizeOf TWord64      = fromIntegral $ Storable.sizeOf (undefined :: Word.Word64)
primSizeOf TAddr        = fromIntegral $ Storable.sizeOf (undefined :: Ptr.WordPtr)
primSizeOf (TPtr _ _)   = fromIntegral $ Storable.sizeOf (undefined :: Ptr.WordPtr)
primSizeOf (TPrm "Ptr") = fromIntegral $ Storable.sizeOf (undefined :: Ptr.WordPtr)
-- unwrap type annotations
primSizeOf (TAnn _ t)   = primSizeOf t
-- unwrap simple type applications
primSizeOf (TKey TKApp [TGTypes [a], _])    = primSizeOf a
primSizeOf (TPrm name) = error $ "I do not know how to size type: " ++ (show name)
primSizeOf _           = error "primSizeOf unimplemented for requested type"

--           size
primAlloc :: Word.Word64 -> IO Ptr.WordPtr
primAlloc size = do p <- Alloc.mallocBytes (fromIntegral size)
                    let wp = Ptr.ptrToWordPtr p
                    return wp

--               size
primAllocAddr :: Word.Word64 -> IO [Value a]
primAllocAddr s = do wp <- primAlloc s
                     return [VAddr wp]

--              region    type
primAllocPtr :: Type a -> Type a -> IO [Value a]
primAllocPtr r t = do let size = primSizeOf t
                      wp <- primAlloc size
                      return [VPtr r t wp]

primFree :: Ptr.WordPtr -> IO [Value a]
primFree wp = do let p = Ptr.wordPtrToPtr wp
                 Alloc.free p
                 return []

primWrite :: Type a -> Ptr.WordPtr -> Value a -> IO [Value a]
primWrite _ wp (VBool   v) = do let p = Ptr.wordPtrToPtr wp
                                Storable.poke p v
                                return []
primWrite _ wp (VInt8   v) = do let p = Ptr.wordPtrToPtr wp
                                Storable.poke p v
                                return []
primWrite _ wp (VInt16  v) = do let p = Ptr.wordPtrToPtr wp
                                Storable.poke p v
                                return []
primWrite _ wp (VInt32  v) = do let p = Ptr.wordPtrToPtr wp
                                Storable.poke p v
                                return []
primWrite _ wp (VInt64  v) = do let p = Ptr.wordPtrToPtr wp
                                Storable.poke p v
                                return []
primWrite _ wp (VWord8  v) = do let p = Ptr.wordPtrToPtr wp
                                Storable.poke p v
                                return []
primWrite _ wp (VWord16 v) = do let p = Ptr.wordPtrToPtr wp
                                Storable.poke p v
                                return []
primWrite _ wp (VWord32 v) = do let p = Ptr.wordPtrToPtr wp
                                Storable.poke p v
                                return []
primWrite _ wp (VWord64 v) = do let p = Ptr.wordPtrToPtr wp
                                Storable.poke p v
                                return []
primWrite _ wp (VAddr   v) = do let p = Ptr.wordPtrToPtr wp
                                Storable.poke p v
                                return []
primWrite _ _  _           = error "primWrite unimplemented for requested type"

primRead :: Type a -> Ptr.WordPtr -> IO [Value a]
primRead TBool   wp = do let p = Ptr.wordPtrToPtr wp
                         v <- Storable.peek p
                         return [VBool v]
primRead TInt8   wp = do let p = Ptr.wordPtrToPtr wp
                         v <- Storable.peek p
                         return [VInt8 v]
primRead TInt16  wp = do let p = Ptr.wordPtrToPtr wp
                         v <- Storable.peek p
                         return [VInt16 v]
primRead TInt32  wp = do let p = Ptr.wordPtrToPtr wp
                         v <- Storable.peek p
                         return [VInt32 v]
primRead TInt64  wp = do let p = Ptr.wordPtrToPtr wp
                         v <- Storable.peek p
                         return [VInt64 v]
primRead TWord8  wp = do let p = Ptr.wordPtrToPtr wp
                         v <- Storable.peek p
                         return [VWord8 v]
primRead TWord16 wp = do let p = Ptr.wordPtrToPtr wp
                         v <- Storable.peek p
                         return [VWord16 v]
primRead TWord32 wp = do let p = Ptr.wordPtrToPtr wp
                         v <- Storable.peek p
                         return [VWord32 v]
primRead TWord64 wp = do let p = Ptr.wordPtrToPtr wp
                         v <- Storable.peek p
                         return [VWord64 v]
primRead TAddr   wp = do let p = Ptr.wordPtrToPtr wp
                         v <- Storable.peek p
                         return [VAddr v]
primRead TAddr   wp = do let p = Ptr.wordPtrToPtr wp
                         v <- Storable.peek p
                         return [VAddr v]
primRead _ _         = error "primRead unimplemented for requested type"

primOpsMemory
 = [ PP { name  = "sizeOf"
        , tsig  = [("a", TData)] :*> TWord64
        , step  = \[NTs [t]] -> do let s = primSizeOf t
                                   return $ VWord64 s
        , docs  = "Find sizeof a type." }

   , PO { name  = "allocAddr"
        , tsig  = [TWord64] :-> [TAddr]
        , teff  = [TPrm "Memory"]
        , exec  = \[NVs [VWord64 s]] -> primAllocAddr s
        , docs  = "Allocate a raw address." }

   , PO { name  = "freeAddr"
        , tsig  = [TAddr] :-> []
        , teff  = [TPrm "Memory"]
        , exec  = \[NVs [VAddr a]] -> primFree a
        , docs  = "Free an Addr." }

   , PO { name  = "writeAddr"
        , tsig  = [("t", TData)] :*> [TAddr, "t"] :-> []
        , teff  = [TPrm "Memory"]
        , exec  = \[NTs [t], NVs [VAddr a, v]] -> primWrite t a v
        , docs  = "Free an Addr." }

   , PO { name  = "readAddr"
        , tsig  = [("t", TData)] :*> [TAddr] :-> ["t"]
        , teff  = [TPrm "Memory"]
        , exec  = \[NTs [t], NVs [VAddr a]] -> primRead t a
        , docs  = "Free an Addr." }

   , PO { name  = "allocPtr"
        , tsig  = [("r", TRegion), ("t", TData)] :*> TPtr "r" "t"
        , teff  = [TPrm "Memory"]
        , exec  = \[NTs [r, t]] -> primAllocPtr r t
        , docs  = "Allocate a ptr." }

   , PO { name  = "freePtr"
        , tsig  = [("r", TRegion), ("t", TData)] :*> [TPtr "r" "t"] :-> []
        , teff  = [TPrm "Memory"]
        , exec  = \[NTs [_, _], NVs [VPtr _ _ a]] -> primFree a
        , docs  = "Free a Ptr." }

   , PO { name  = "writePtr"
        , tsig  = [("r", TRegion), ("t", TData)] :*> [TPtr "r" "t", "t"] :-> []
        , teff  = [TPrm "Memory"]
        , exec  = \[NTs [_, _], NVs [VPtr _ t a, v]] -> primWrite t a v
        , docs  = "Free an Addr." }

   , PO { name  = "readPtr"
        , tsig  = [("r", TRegion), ("t", TData)] :*> [TPtr "r" "t"] :-> ["t"]
        , teff  = [TPrm "Memory"]
        , exec  = \[NTs [_, _], NVs [VPtr _ t a]] -> primRead t a
        , docs  = "Free an Addr." }
   ]
