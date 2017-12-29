module Color.Palette where

import Prelude

import CSS as CSS
import Color as C
import Color.Scale as Scale
import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Record as CAR
import Data.Codec.Argonaut.Variant as CAV
import Data.Foldable as F
import Data.Generic.Rep (class Generic)
import Data.Either as E
import Data.List as L
import Data.List.NonEmpty as NEL
import Data.Maybe as M
import Data.Profunctor (dimap)
import Data.Record as R
import Data.Variant as V
import Data.Symbol (SProxy(..))
import Color.Palette.Easing as EA
import Math (abs, sqrt, (%))

hslDistance
  ∷ ∀ r z
  . { h ∷ Number , s ∷ Number , l ∷ Number | r }
  → { h ∷ Number , s ∷ Number , l ∷ Number | z }
  → Number
hslDistance a b =
  sqrt
    $ sqr (hueDistance a.h b.h)
    + sqr (mul 100.0 $ a.s - b.s)
    + sqr (mul 100.0 $ a.l - b.l)
  where
  hueDistance a' b' = d'/ 1.8
    where
    d = abs (a' - b')
    d' = if d < 180.0 then d else 360.0 - d

  sqr x = x * x

type Range = { min ∷ Number, max ∷ Number }

type SequentialR r =
  { hueShift ∷ Number
  , darknessRange ∷ Range
  , lightnessRange ∷ Range
  | r }

type DivergingR = SequentialR ( startColorHueShift ∷ Number )

type QualitativeR =
  { colors ∷ NEL.NonEmptyList C.Color }

data Palette
  = Sequential (SequentialR ())
  | Diverging DivergingR
  | Qualitative QualitativeR

derive instance eqPalette ∷ Eq Palette
derive instance genericPalette ∷ Generic Palette _

colorCodec ∷ CA.JsonCodec C.Color
colorCodec = dimap C.toHSLA (\{h, s, l, a} → C.hsla h s l a) $ CA.object "Color" $ CAR.record
  { h: CA.number
  , s: CA.number
  , l: CA.number
  , a: CA.number
  }
codec ∷ CA.JsonCodec Palette
codec = dimap toV fromV $ CAV.variantMatch
  { sequential: E.Right sequentialCodec
  , diverging: E.Right divergingCodec
  , qualitative: E.Right qualitativeCodec
  }
  where
  toV = case _ of
    Sequential s → V.inj (SProxy ∷ SProxy "sequential") s
    Diverging d → V.inj (SProxy ∷ SProxy "diverging") d
    Qualitative q → V.inj (SProxy ∷ SProxy "qualitative") q

  fromV = V.match
    { sequential: Sequential
    , diverging: Diverging
    , qualitative: Qualitative
    }

  rangeCodec = CA.object "Color.Palette.Range" $ CAR.record
    { min: CA.number
    , max: CA.number
    }

  sequentialCodecR =
    { hueShift: CA.number
    , darknessRange: rangeCodec
    , lightnessRange: rangeCodec
    }

  sequentialCodec =
    CA.object "Color.Palette.SequentialR"
      $ CAR.record sequentialCodecR

  divergingCodec =
    CA.object "Color.Palette.DivergingR"
      $ CAR.record
      $ R.insert (SProxy ∷ SProxy "startColorHueShift")
          CA.number sequentialCodecR

  qualitativeCodec =
    CA.object "Color.Palette.QualitativeR"
      $ CAR.record { colors: nelCodec colorCodec }

  nelCodec ∷ ∀ a. CA.JsonCodec a → CA.JsonCodec (NEL.NonEmptyList a)
  nelCodec c = dimap toArrayPair fromArrayPair $ CA.object "NonEmptyList" $ CAR.record
    { head: c
    , tail: CA.array c
    }

  toArrayPair ∷ ∀ a. NEL.NonEmptyList a → { head ∷ a, tail ∷ Array a }
  toArrayPair nel = { head: NEL.head nel, tail: L.toUnfoldable $ NEL.tail nel }

  fromArrayPair ∷ ∀ a. { head ∷ a, tail ∷ Array a } → NEL.NonEmptyList a
  fromArrayPair { head, tail } = NEL.appendFoldable (NEL.singleton head) tail




generate ∷ Int → C.Color → Palette → NEL.NonEmptyList C.Color
generate n seedColor = case _ of
  Sequential r →
    fromStops $ sequentialStops r n seedColor
  Diverging r →
    let
      endStops = sequentialStops r n seedColor
      startStops = Scale.reverseStops
        $ sequentialStops r n
        $ C.hsla (r.startColorHueShift + seed.h) seed.s seed.l seed.a
      stops = Scale.combineStops 0.5 startStops endStops
    in
      fromStops stops
  Qualitative r →
    NEL.sortBy (\a b → compare (C.toHSLA a).h (C.toHSLA b).h)
      $ NEL.appendFoldable (NEL.singleton seedColor)
      $ flip NEL.filter r.colors \color →
          let
            c = C.toHSLA color
          in
           10.0 < hslDistance seed { h: c.h, s: (c.s + seed.s) / 2.0, l: (c.l + seed.l) / 2.0 }
  where
  seed = C.toHSLA seedColor
  fromStops stops =
    M.fromMaybe (pure seedColor)
      $ NEL.fromList
      $ flip Scale.colors' n
      $ Scale.cubehelixSample stops

sequentialStops
  ∷ ∀ r
  . SequentialR r
  → Int
  → C.Color
  → Scale.ColorStops
sequentialStops r n inputColor =
  Scale.ColorStops startColor stops endColor
  where
  input = C.toHSLA inputColor
  endColor =
    C.hsla
      input.h
      (EA.quadratic 0.5 0.9 input.s)
      (EA.quadratic r.darknessRange.min r.darknessRange.max input.l)
      input.a
  end  = C.toHSLA endColor
  start =
    { h: end.h + r.hueShift
    , s: EA.quadratic 0.4 0.7 end.s
    , l: EA.linear r.lightnessRange.min r.lightnessRange.max input.l
    , a: end.a
    }
  startColor = C.hsla start.h start.s start.l start.a
  absHueShift = abs r.hueShift % 180.0
  stops = L.Nil

cssGradient ∷ C.Color → Palette → CSS.BackgroundImage
cssGradient seed = mkGradient <<< case _ of
  Sequential r → generate 5 seed $ Sequential r
  Diverging r → generate 10 seed $ Diverging r
  Qualitative r → generate 10 seed $ Qualitative r
  where
  mkGradient colors = CSS.fromString
    $ "linear-gradient(to right, "
    <> F.intercalate ", " (map C.cssStringHSLA colors)
    <> ")"
