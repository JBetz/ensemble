{-# LINE 1 "src/Ensemble/Soundfont/FluidSynth/Foreign/Gen.hsc" #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}


module Ensemble.Soundfont.FluidSynth.Foreign.Gen where
import Foreign.Ptr
import Foreign.Ptr (Ptr,FunPtr,plusPtr)
import Foreign.Ptr (wordPtrToPtr,castPtrToFunPtr)
import Foreign.Storable
import Foreign.C.Types
import Foreign.C.String (CString,CStringLen,CWString,CWStringLen)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Array (peekArray,pokeArray)
import Data.Int
import Data.Word

{-# LINE 7 "src/Ensemble/Soundfont/FluidSynth/Foreign/Gen.hsc" #-}

{- enum fluid_gen_type {
    GEN_STARTADDROFS,
    GEN_ENDADDROFS,
    GEN_STARTLOOPADDROFS,
    GEN_ENDLOOPADDROFS,
    GEN_STARTADDRCOARSEOFS,
    GEN_MODLFOTOPITCH,
    GEN_VIBLFOTOPITCH,
    GEN_MODENVTOPITCH,
    GEN_FILTERFC,
    GEN_FILTERQ,
    GEN_MODLFOTOFILTERFC,
    GEN_MODENVTOFILTERFC,
    GEN_ENDADDRCOARSEOFS,
    GEN_MODLFOTOVOL,
    GEN_UNUSED1,
    GEN_CHORUSSEND,
    GEN_REVERBSEND,
    GEN_PAN,
    GEN_UNUSED2,
    GEN_UNUSED3,
    GEN_UNUSED4,
    GEN_MODLFODELAY,
    GEN_MODLFOFREQ,
    GEN_VIBLFODELAY,
    GEN_VIBLFOFREQ,
    GEN_MODENVDELAY,
    GEN_MODENVATTACK,
    GEN_MODENVHOLD,
    GEN_MODENVDECAY,
    GEN_MODENVSUSTAIN,
    GEN_MODENVRELEASE,
    GEN_KEYTOMODENVHOLD,
    GEN_KEYTOMODENVDECAY,
    GEN_VOLENVDELAY,
    GEN_VOLENVATTACK,
    GEN_VOLENVHOLD,
    GEN_VOLENVDECAY,
    GEN_VOLENVSUSTAIN,
    GEN_VOLENVRELEASE,
    GEN_KEYTOVOLENVHOLD,
    GEN_KEYTOVOLENVDECAY,
    GEN_INSTRUMENT,
    GEN_RESERVED1,
    GEN_KEYRANGE,
    GEN_VELRANGE,
    GEN_STARTLOOPADDRCOARSEOFS,
    GEN_KEYNUM,
    GEN_VELOCITY,
    GEN_ATTENUATION,
    GEN_RESERVED2,
    GEN_ENDLOOPADDRCOARSEOFS,
    GEN_COARSETUNE,
    GEN_FINETUNE,
    GEN_SAMPLEID,
    GEN_SAMPLEMODE,
    GEN_RESERVED3,
    GEN_SCALETUNE,
    GEN_EXCLUSIVECLASS,
    GEN_OVERRIDEROOTKEY,
    GEN_PITCH,
    GEN_CUSTOM_BALANCE,
    GEN_CUSTOM_FILTERFC,
    GEN_CUSTOM_FILTERQ,
    GEN_LAST
}; -}
type C'fluid_gen_type = CUInt

{-# LINE 75 "src/Ensemble/Soundfont/FluidSynth/Foreign/Gen.hsc" #-}
c'GEN_STARTADDROFS = 0
c'GEN_STARTADDROFS :: (Num a) => a

{-# LINE 76 "src/Ensemble/Soundfont/FluidSynth/Foreign/Gen.hsc" #-}
c'GEN_ENDADDROFS = 1
c'GEN_ENDADDROFS :: (Num a) => a

{-# LINE 77 "src/Ensemble/Soundfont/FluidSynth/Foreign/Gen.hsc" #-}
c'GEN_STARTLOOPADDROFS = 2
c'GEN_STARTLOOPADDROFS :: (Num a) => a

{-# LINE 78 "src/Ensemble/Soundfont/FluidSynth/Foreign/Gen.hsc" #-}
c'GEN_ENDLOOPADDROFS = 3
c'GEN_ENDLOOPADDROFS :: (Num a) => a

{-# LINE 79 "src/Ensemble/Soundfont/FluidSynth/Foreign/Gen.hsc" #-}
c'GEN_STARTADDRCOARSEOFS = 4
c'GEN_STARTADDRCOARSEOFS :: (Num a) => a

{-# LINE 80 "src/Ensemble/Soundfont/FluidSynth/Foreign/Gen.hsc" #-}
c'GEN_MODLFOTOPITCH = 5
c'GEN_MODLFOTOPITCH :: (Num a) => a

{-# LINE 81 "src/Ensemble/Soundfont/FluidSynth/Foreign/Gen.hsc" #-}
c'GEN_VIBLFOTOPITCH = 6
c'GEN_VIBLFOTOPITCH :: (Num a) => a

{-# LINE 82 "src/Ensemble/Soundfont/FluidSynth/Foreign/Gen.hsc" #-}
c'GEN_MODENVTOPITCH = 7
c'GEN_MODENVTOPITCH :: (Num a) => a

{-# LINE 83 "src/Ensemble/Soundfont/FluidSynth/Foreign/Gen.hsc" #-}
c'GEN_FILTERFC = 8
c'GEN_FILTERFC :: (Num a) => a

{-# LINE 84 "src/Ensemble/Soundfont/FluidSynth/Foreign/Gen.hsc" #-}
c'GEN_FILTERQ = 9
c'GEN_FILTERQ :: (Num a) => a

{-# LINE 85 "src/Ensemble/Soundfont/FluidSynth/Foreign/Gen.hsc" #-}
c'GEN_MODLFOTOFILTERFC = 10
c'GEN_MODLFOTOFILTERFC :: (Num a) => a

{-# LINE 86 "src/Ensemble/Soundfont/FluidSynth/Foreign/Gen.hsc" #-}
c'GEN_MODENVTOFILTERFC = 11
c'GEN_MODENVTOFILTERFC :: (Num a) => a

{-# LINE 87 "src/Ensemble/Soundfont/FluidSynth/Foreign/Gen.hsc" #-}
c'GEN_ENDADDRCOARSEOFS = 12
c'GEN_ENDADDRCOARSEOFS :: (Num a) => a

{-# LINE 88 "src/Ensemble/Soundfont/FluidSynth/Foreign/Gen.hsc" #-}
c'GEN_MODLFOTOVOL = 13
c'GEN_MODLFOTOVOL :: (Num a) => a

{-# LINE 89 "src/Ensemble/Soundfont/FluidSynth/Foreign/Gen.hsc" #-}
c'GEN_UNUSED1 = 14
c'GEN_UNUSED1 :: (Num a) => a

{-# LINE 90 "src/Ensemble/Soundfont/FluidSynth/Foreign/Gen.hsc" #-}
c'GEN_CHORUSSEND = 15
c'GEN_CHORUSSEND :: (Num a) => a

{-# LINE 91 "src/Ensemble/Soundfont/FluidSynth/Foreign/Gen.hsc" #-}
c'GEN_REVERBSEND = 16
c'GEN_REVERBSEND :: (Num a) => a

{-# LINE 92 "src/Ensemble/Soundfont/FluidSynth/Foreign/Gen.hsc" #-}
c'GEN_PAN = 17
c'GEN_PAN :: (Num a) => a

{-# LINE 93 "src/Ensemble/Soundfont/FluidSynth/Foreign/Gen.hsc" #-}
c'GEN_UNUSED2 = 18
c'GEN_UNUSED2 :: (Num a) => a

{-# LINE 94 "src/Ensemble/Soundfont/FluidSynth/Foreign/Gen.hsc" #-}
c'GEN_UNUSED3 = 19
c'GEN_UNUSED3 :: (Num a) => a

{-# LINE 95 "src/Ensemble/Soundfont/FluidSynth/Foreign/Gen.hsc" #-}
c'GEN_UNUSED4 = 20
c'GEN_UNUSED4 :: (Num a) => a

{-# LINE 96 "src/Ensemble/Soundfont/FluidSynth/Foreign/Gen.hsc" #-}
c'GEN_MODLFODELAY = 21
c'GEN_MODLFODELAY :: (Num a) => a

{-# LINE 97 "src/Ensemble/Soundfont/FluidSynth/Foreign/Gen.hsc" #-}
c'GEN_MODLFOFREQ = 22
c'GEN_MODLFOFREQ :: (Num a) => a

{-# LINE 98 "src/Ensemble/Soundfont/FluidSynth/Foreign/Gen.hsc" #-}
c'GEN_VIBLFODELAY = 23
c'GEN_VIBLFODELAY :: (Num a) => a

{-# LINE 99 "src/Ensemble/Soundfont/FluidSynth/Foreign/Gen.hsc" #-}
c'GEN_VIBLFOFREQ = 24
c'GEN_VIBLFOFREQ :: (Num a) => a

{-# LINE 100 "src/Ensemble/Soundfont/FluidSynth/Foreign/Gen.hsc" #-}
c'GEN_MODENVDELAY = 25
c'GEN_MODENVDELAY :: (Num a) => a

{-# LINE 101 "src/Ensemble/Soundfont/FluidSynth/Foreign/Gen.hsc" #-}
c'GEN_MODENVATTACK = 26
c'GEN_MODENVATTACK :: (Num a) => a

{-# LINE 102 "src/Ensemble/Soundfont/FluidSynth/Foreign/Gen.hsc" #-}
c'GEN_MODENVHOLD = 27
c'GEN_MODENVHOLD :: (Num a) => a

{-# LINE 103 "src/Ensemble/Soundfont/FluidSynth/Foreign/Gen.hsc" #-}
c'GEN_MODENVDECAY = 28
c'GEN_MODENVDECAY :: (Num a) => a

{-# LINE 104 "src/Ensemble/Soundfont/FluidSynth/Foreign/Gen.hsc" #-}
c'GEN_MODENVSUSTAIN = 29
c'GEN_MODENVSUSTAIN :: (Num a) => a

{-# LINE 105 "src/Ensemble/Soundfont/FluidSynth/Foreign/Gen.hsc" #-}
c'GEN_MODENVRELEASE = 30
c'GEN_MODENVRELEASE :: (Num a) => a

{-# LINE 106 "src/Ensemble/Soundfont/FluidSynth/Foreign/Gen.hsc" #-}
c'GEN_KEYTOMODENVHOLD = 31
c'GEN_KEYTOMODENVHOLD :: (Num a) => a

{-# LINE 107 "src/Ensemble/Soundfont/FluidSynth/Foreign/Gen.hsc" #-}
c'GEN_KEYTOMODENVDECAY = 32
c'GEN_KEYTOMODENVDECAY :: (Num a) => a

{-# LINE 108 "src/Ensemble/Soundfont/FluidSynth/Foreign/Gen.hsc" #-}
c'GEN_VOLENVDELAY = 33
c'GEN_VOLENVDELAY :: (Num a) => a

{-# LINE 109 "src/Ensemble/Soundfont/FluidSynth/Foreign/Gen.hsc" #-}
c'GEN_VOLENVATTACK = 34
c'GEN_VOLENVATTACK :: (Num a) => a

{-# LINE 110 "src/Ensemble/Soundfont/FluidSynth/Foreign/Gen.hsc" #-}
c'GEN_VOLENVHOLD = 35
c'GEN_VOLENVHOLD :: (Num a) => a

{-# LINE 111 "src/Ensemble/Soundfont/FluidSynth/Foreign/Gen.hsc" #-}
c'GEN_VOLENVDECAY = 36
c'GEN_VOLENVDECAY :: (Num a) => a

{-# LINE 112 "src/Ensemble/Soundfont/FluidSynth/Foreign/Gen.hsc" #-}
c'GEN_VOLENVSUSTAIN = 37
c'GEN_VOLENVSUSTAIN :: (Num a) => a

{-# LINE 113 "src/Ensemble/Soundfont/FluidSynth/Foreign/Gen.hsc" #-}
c'GEN_VOLENVRELEASE = 38
c'GEN_VOLENVRELEASE :: (Num a) => a

{-# LINE 114 "src/Ensemble/Soundfont/FluidSynth/Foreign/Gen.hsc" #-}
c'GEN_KEYTOVOLENVHOLD = 39
c'GEN_KEYTOVOLENVHOLD :: (Num a) => a

{-# LINE 115 "src/Ensemble/Soundfont/FluidSynth/Foreign/Gen.hsc" #-}
c'GEN_KEYTOVOLENVDECAY = 40
c'GEN_KEYTOVOLENVDECAY :: (Num a) => a

{-# LINE 116 "src/Ensemble/Soundfont/FluidSynth/Foreign/Gen.hsc" #-}
c'GEN_INSTRUMENT = 41
c'GEN_INSTRUMENT :: (Num a) => a

{-# LINE 117 "src/Ensemble/Soundfont/FluidSynth/Foreign/Gen.hsc" #-}
c'GEN_RESERVED1 = 42
c'GEN_RESERVED1 :: (Num a) => a

{-# LINE 118 "src/Ensemble/Soundfont/FluidSynth/Foreign/Gen.hsc" #-}
c'GEN_KEYRANGE = 43
c'GEN_KEYRANGE :: (Num a) => a

{-# LINE 119 "src/Ensemble/Soundfont/FluidSynth/Foreign/Gen.hsc" #-}
c'GEN_VELRANGE = 44
c'GEN_VELRANGE :: (Num a) => a

{-# LINE 120 "src/Ensemble/Soundfont/FluidSynth/Foreign/Gen.hsc" #-}
c'GEN_STARTLOOPADDRCOARSEOFS = 45
c'GEN_STARTLOOPADDRCOARSEOFS :: (Num a) => a

{-# LINE 121 "src/Ensemble/Soundfont/FluidSynth/Foreign/Gen.hsc" #-}
c'GEN_KEYNUM = 46
c'GEN_KEYNUM :: (Num a) => a

{-# LINE 122 "src/Ensemble/Soundfont/FluidSynth/Foreign/Gen.hsc" #-}
c'GEN_VELOCITY = 47
c'GEN_VELOCITY :: (Num a) => a

{-# LINE 123 "src/Ensemble/Soundfont/FluidSynth/Foreign/Gen.hsc" #-}
c'GEN_ATTENUATION = 48
c'GEN_ATTENUATION :: (Num a) => a

{-# LINE 124 "src/Ensemble/Soundfont/FluidSynth/Foreign/Gen.hsc" #-}
c'GEN_RESERVED2 = 49
c'GEN_RESERVED2 :: (Num a) => a

{-# LINE 125 "src/Ensemble/Soundfont/FluidSynth/Foreign/Gen.hsc" #-}
c'GEN_ENDLOOPADDRCOARSEOFS = 50
c'GEN_ENDLOOPADDRCOARSEOFS :: (Num a) => a

{-# LINE 126 "src/Ensemble/Soundfont/FluidSynth/Foreign/Gen.hsc" #-}
c'GEN_COARSETUNE = 51
c'GEN_COARSETUNE :: (Num a) => a

{-# LINE 127 "src/Ensemble/Soundfont/FluidSynth/Foreign/Gen.hsc" #-}
c'GEN_FINETUNE = 52
c'GEN_FINETUNE :: (Num a) => a

{-# LINE 128 "src/Ensemble/Soundfont/FluidSynth/Foreign/Gen.hsc" #-}
c'GEN_SAMPLEID = 53
c'GEN_SAMPLEID :: (Num a) => a

{-# LINE 129 "src/Ensemble/Soundfont/FluidSynth/Foreign/Gen.hsc" #-}
c'GEN_SAMPLEMODE = 54
c'GEN_SAMPLEMODE :: (Num a) => a

{-# LINE 130 "src/Ensemble/Soundfont/FluidSynth/Foreign/Gen.hsc" #-}
c'GEN_RESERVED3 = 55
c'GEN_RESERVED3 :: (Num a) => a

{-# LINE 131 "src/Ensemble/Soundfont/FluidSynth/Foreign/Gen.hsc" #-}
c'GEN_SCALETUNE = 56
c'GEN_SCALETUNE :: (Num a) => a

{-# LINE 132 "src/Ensemble/Soundfont/FluidSynth/Foreign/Gen.hsc" #-}
c'GEN_EXCLUSIVECLASS = 57
c'GEN_EXCLUSIVECLASS :: (Num a) => a

{-# LINE 133 "src/Ensemble/Soundfont/FluidSynth/Foreign/Gen.hsc" #-}
c'GEN_OVERRIDEROOTKEY = 58
c'GEN_OVERRIDEROOTKEY :: (Num a) => a

{-# LINE 134 "src/Ensemble/Soundfont/FluidSynth/Foreign/Gen.hsc" #-}
c'GEN_PITCH = 59
c'GEN_PITCH :: (Num a) => a

{-# LINE 135 "src/Ensemble/Soundfont/FluidSynth/Foreign/Gen.hsc" #-}
c'GEN_CUSTOM_BALANCE = 60
c'GEN_CUSTOM_BALANCE :: (Num a) => a

{-# LINE 136 "src/Ensemble/Soundfont/FluidSynth/Foreign/Gen.hsc" #-}
c'GEN_CUSTOM_FILTERFC = 61
c'GEN_CUSTOM_FILTERFC :: (Num a) => a

{-# LINE 137 "src/Ensemble/Soundfont/FluidSynth/Foreign/Gen.hsc" #-}
c'GEN_CUSTOM_FILTERQ = 62
c'GEN_CUSTOM_FILTERQ :: (Num a) => a

{-# LINE 138 "src/Ensemble/Soundfont/FluidSynth/Foreign/Gen.hsc" #-}
c'GEN_LAST = 63
c'GEN_LAST :: (Num a) => a

{-# LINE 139 "src/Ensemble/Soundfont/FluidSynth/Foreign/Gen.hsc" #-}
