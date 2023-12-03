module SimpleLang.Tools.Manual.TypeLevelSLTypeCheck where

import Data.Vector as V
import Data.Map as M
import GHC.TypeNats
import Data.Type.Bool
import Data.Proxy
import Data.Text as T
import Control.Monad (when)

data SLType =
      SLTUnit
    | SLTInt
    | SLTFuncPtr [SLType] SLType
    | SLTPtr SLType
    | SLTStruct [SLType]
    | SLTUnion  [SLType]
    deriving (Show, Eq)

type family NatMax (n :: Nat) (m :: Nat) :: Nat where
  NatMax n m = If (n <=? m) m n

type family SLTSizeOf (t :: SLType) :: Nat where
  SLTSizeOf SLTUnit               = 0
  SLTSizeOf SLTInt                = 1
  SLTSizeOf (SLTFuncPtr args ret) = 1
  SLTSizeOf (SLTPtr t           ) = 1
  SLTSizeOf (SLTStruct (t:ts)   ) = SLTSizeOf t + SLTSizeOf (SLTStruct ts)
  SLTSizeOf (SLTStruct '[]      ) = 0
  SLTSizeOf (SLTUnion  (t:ts)   ) = NatMax (SLTSizeOf t) (SLTSizeOf (SLTUnion ts))
  SLTSizeOf (SLTUnion  '[]      ) = 0

class Member (t :: SLType) (ts :: [SLType])

instance Member t (t:ts)
instance Member t ts => Member t (t':ts)



data SLFuncName (args :: [SLType]) (ret :: SLType) =
        SLFuncMain
      | SLUserFunc Text Text
      deriving (Show, Eq, Ord)

data SLCall (t :: SLType) where
    SLSolidFuncCall :: SLFuncName ts t          -> SLExp ('SLTStruct ts) -> SLCall t
    SLFuncRefCall   :: SLRef ('SLTFuncPtr ts t) -> SLExp ('SLTStruct ts) -> SLCall t

deriving instance Show (SLCall t)

data SLPrim1 =
        SLPrim1Inv
      deriving (Show, Eq)

data SLPrim2 =
        SLPrim2Add
      | SLPrim2Sub
      | SLPrim2Mult
      | SLPrim2Shift
      | SLPrim2And
      | SLPrim2Or
      | SLPrim2Xor
      | SLPrim2Gt
      | SLPrim2Lt
      | SLPrim2Eq
      deriving (Show, Eq)

data SLExp (t :: SLType) where
    SLEConst      :: SLVal                                     -> SLExp 'SLTInt
    SLELocal      :: Int                                       -> SLExp t
    SLEArg        :: Int                                       -> SLExp t
    SLEPtr        :: SLExp t                                   -> SLExp ('SLTPtr t)
    SLEPushCall   :: SLCall t                                  -> SLExp t
    SLEFuncPtr    :: SLFuncName args ret                       -> SLExp ('SLTFuncPtr args ret)
    SLEPrim1      :: SLPrim1 -> SLExp 'SLTInt                  -> SLExp 'SLTInt
    SLEPrim2      :: SLPrim2 -> SLExp 'SLTInt -> SLExp 'SLTInt -> SLExp 'SLTInt
    SLEStructNil  ::                                              SLExp ('SLTStruct '[])
    SLEStructCons :: SLExp t -> SLExp ('SLTStruct ts)          -> SLExp ('SLTStruct (t:ts)) 
    SLEUnion      :: Member t ts => SLExp t                    -> SLExp ('SLTUnion     ts ) 

instance Show (SLExp t) where
  show = T.unpack . prettyPrintSLExp

data SLRef (t :: SLType) = 
          SLRefPtr   (SLExp t)
        | SLRefLocal Int
instance Show (SLRef t) where
  show = T.unpack . prettyPrintSLRef

data SLStatement where
  SLSInitVar        :: Int -> SLExp t     -> SLStatement
  SLSSubst          :: SLRef t -> SLExp t -> SLStatement
  SLSReturn         :: SLExp t            -> SLStatement
  SLSTailCallReturn :: SLCall t           -> SLStatement

instance Show SLStatement where
  show = T.unpack . prettyPrintSLStatement

data SLBlock where
    SLBSingle :: SLStatement -> SLBlock
    SLBMulti  :: V.Vector SLBlock -> SLBlock
    SLBCase   :: V.Vector (SLExp 'SLTInt, SLBlock) -> SLBlock -> SLBlock
    SLBWhile  :: SLExp 'SLTInt -> SLBlock -> SLBlock

instance Show SLBlock where
  show = T.unpack . T.intercalate "\n" . V.toList . prettyPrintSLBlock 0

data SLFuncBlock (args :: [SLType]) (ret :: SLType) =
      SLFuncBlock {
          slfName     :: SLFuncName args ret
        , slfArgCount :: Int
        , slfBlock    :: SLBlock
      }
      deriving (Show)