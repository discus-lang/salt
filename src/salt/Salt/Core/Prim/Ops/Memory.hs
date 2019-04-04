
module Salt.Core.Prim.Ops.Memory where
import Salt.Core.Prim.Ops.Base
import qualified Data.Word as Word
import qualified Foreign.Marshal.Alloc as Alloc
import qualified Foreign.Ptr as Ptr

primSizeOf :: Type a -> Word.Word64
primSizeOf _ = error "primSizeOf unimplemented"

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

primOpsMemory
 = [ PP { name  = "sizeof"
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

   , PO { name  = "allocPtr"
        , tsig  = [("r", TRegion), ("t", TData)] :*> TPtr "r" "t"
        , teff  = [TPrm "Memory"]
        , exec  = \[NTs [r, t]] -> primAllocPtr r t
        , docs  = "Allocate a ptr." }

   , PO { name  = "freePtr"
        , tsig  = [("r", TRegion), ("t", TData)] :*> [TPtr "r" "t"] :-> []
        , teff  = [TPrm "Memory"]
        , exec  = \[NVs [VPtr _ _ a]] -> primFree a
        , docs  = "Free a Ptr." }

   ]
