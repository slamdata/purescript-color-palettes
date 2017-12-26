module Color.Palette where

import Prelude

import CSS as CSS
import Color as C
import Color.Scale as Scale
import Data.Array as A
import Data.Foldable as F
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
--import Data.List (List(..))
--import Data.List as List
--import Data.List.NonEmpty as NEL
--import Data.List.Types (NonEmptyList)
import Data.List ((:))
import Data.List as L
import Data.List.NonEmpty as NEL
import Data.NonEmpty ((:|))
import Data.Maybe (fromJust)
import Data.Newtype (class Newtype)
import Math (abs, pow, sqrt, (%))
import Partial.Unsafe (unsafePartial)


foreign import kind Emptiness
foreign import data Empty ∷ Emptiness
foreign import data NonEmpty ∷ Emptiness

foreign import kind PaletteK
foreign import data SequentialK ∷ PaletteK
foreign import data DivergingK ∷ PaletteK
foreign import data QualitativeK ∷ PaletteK

newtype Leibniz (a ∷ PaletteK) (b ∷ PaletteK) = Leibniz (∀ f. f a → f b)


infix 4 type Leibniz as ~

refl ∷ ∀ a. Leibniz a a
refl = Leibniz id

type ColorHSL = { h ∷ Number, s ∷ Number, l ∷ Number }

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

data Palette t
  = Sequential (t ~ SequentialK) (SequentialR ())
  | Diverging (t ~ DivergingK) DivergingR
  | Qualitative (t ~ QualitativeK) QualitativeR

sequential ∷ SequentialR () → Palette SequentialK
sequential = Sequential refl

diverging ∷ DivergingR → Palette DivergingK
diverging = Diverging refl

qualitative ∷ QualitativeR → Palette QualitativeK
qualitative = Qualitative refl

generate ∷ ∀ t. Int → C.Color → Palette t → NEL.NonEmptyList C.Color
generate n seedColor = case _ of
  Sequential _ r →
    pure C.black
  Diverging _ r →
    pure C.black
  Qualitative _ r →
    NEL.sortBy (\a b → compare (C.toHSLA a).h (C.toHSLA b).h)
      $ NEL.appendFoldable (NEL.singleton seedColor)
      $ flip NEL.filter r.colors \color →
          let
            c = C.toHSLA color
          in
           10.0 < hslDistance seed { h: c.h, s: (c.s + seed.s) / 2.0, l: (c.l + seed.l) / 2.0 }
  where
  seed = C.toHSLA seedColor

{-

-}

{-
newtype SequentialGenerator = SequentialGenerator SequentialGeneratorSpec

type SequentialGeneratorSpec =
  { hueShift ∷ Number
  , darknessRange ∷ Range
  , lightnessRange ∷ Range
  }



derive instance sequentialGeneratorNewType ∷ Newtype SequentialGenerator _
derive instance sequentialGeneratorEq ∷ Eq SequentialGenerator
derive instance sequentialGeneratorOrd ∷ Ord SequentialGenerator
derive instance sequentialGeneratorGeneric ∷ Generic SequentialGenerator _
instance sequentialGeneratorShow ∷ Show SequentialGenerator where
  show = genericShow

newtype Range = Range { min ∷ Number, max ∷ Number }

derive instance rangeNewType ∷ Newtype Range _
derive instance rangeEq ∷ Eq Range
derive instance rangeOrd ∷ Ord Range
derive instance rangeGeneric ∷ Generic Range _
instance rangeShow ∷ Show Range where
  show = genericShow


newtype DivergingGenerator = DivergingGenerator DivergingGeneratorSpec

type DivergingGeneratorSpec =
  { sequentialGenerator ∷ SequentialGenerator
  , startColorHueShift ∷ Number
  }

derive instance divergingGeneratorNewType ∷ Newtype DivergingGenerator _
derive instance divergingGeneratorEq ∷ Eq DivergingGenerator
derive instance divergingGeneratorOrd ∷ Ord DivergingGenerator
derive instance divergingGeneratorGeneric ∷ Generic DivergingGenerator _
instance divergingGeneratorShow ∷ Show DivergingGenerator where
  show = genericShow

newtype QualitativeGenerator = QualitativeGenerator QualitativeGeneratorSpec

type QualitativeGeneratorSpec =
  { colors ∷ Array ColorHSL }

newtype ColorHSL = ColorHSL { h ∷ Number, s ∷ Number, l ∷ Number }

derive instance colorHSLNewType ∷ Newtype ColorHSL _
derive instance colorHSLEq ∷ Eq ColorHSL
derive instance colorHSLOrd ∷ Ord ColorHSL
derive instance colorHSLGeneric ∷ Generic ColorHSL _
instance colorHSLShow ∷ Show ColorHSL where
  show = genericShow

derive instance qualitativeGeneratorNewType ∷ Newtype QualitativeGenerator _
derive instance qualitativeGeneratorEq ∷ Eq QualitativeGenerator
derive instance qualitativeGeneratorOrd ∷ Ord QualitativeGenerator
derive instance qualitativeGeneratorGeneric ∷ Generic QualitativeGenerator _
instance qualitativeGeneratorShow ∷ Show QualitativeGenerator where
  show = genericShow

type PaletteRunner = Color → Int → Array Color


sequentialPaletteGenerators ∷ NonEmptyList SequentialGenerator
sequentialPaletteGenerators = unsafeNonEmpty cubehelixGenerators
  where
  cubehelixGenerators = do
    hueShift <- hueShifts [ 0.0] [ 30.0, 60.0, 90.0, 120.0, 150.0, 180.0, 210.0, 240.0, 270.0, 300.0, 330.0, 360.0]
    [ SequentialGenerator { hueShift, darknessRange: Range {min: 0.1, max: 0.5}, lightnessRange: Range {min: 0.85, max: 0.97} }
    , SequentialGenerator { hueShift, darknessRange: Range {min: 0.0, max: 0.2}, lightnessRange: Range {min: 0.92, max: 1.0} }
    ]

hueShifts ∷ Array Number → Array Number → Array Number
hueShifts mid hues = reverse hues <> mid <> map (_ * -1.0) hues

divergingPaletteGenerators ∷ NonEmptyList DivergingGenerator
divergingPaletteGenerators = unsafeNonEmpty $ cubehelixGenerators
  where
  cubehelixGenerators = do
    -- TODO make sure spaces do not overlap
    hueShift <- hueShifts [0.0] [45.0, 90.0]
    sequentialGenerator <-
      [ SequentialGenerator { hueShift, darknessRange: Range {min: 0.1, max: 0.3}, lightnessRange: Range {min: 0.95, max: 1.0} }
      , SequentialGenerator { hueShift, darknessRange: Range {min: 0.2, max: 0.5}, lightnessRange: Range {min: 0.95, max: 1.0} }
      ]
    secondaryHueShift <- [-135.0, -90.0, 90.0, 135.0, 180.0]
    pure $ DivergingGenerator
      { sequentialGenerator
      , startColorHueShift: secondaryHueShift
      }
qualitativePaletteGenerators ∷ NonEmptyList QualitativeGenerator
qualitativePaletteGenerators = unsafeNonEmpty $ map
  ({colors: _} >>> QualitativeGenerator)
  [ [ hsl 0.0     0.0     0.4
    , hsl 24.29   0.785   0.4196
    , hsl 29.24   0.9675  0.7588
    , hsl 60.0    1.0     0.8
    , hsl 120.0   0.4066  0.6431
    , hsl 214.0   0.5172  0.4549
    , hsl 265.26  0.3065  0.7569
    , hsl 328.49  0.9835  0.4745
    ]
  , [ hsl 0.0     0.0     0.4
    , hsl 25.95   0.9817  0.4294
    , hsl 38.98   0.7026  0.3824
    , hsl 44.47   0.9828  0.4549
    , hsl 88.24   0.6939  0.3843
    , hsl 162.14  0.7081  0.3627
    , hsl 244.48  0.3059  0.5706
    , hsl 329.37  0.7983  0.5333
    ]
  , [ hsl 0.61    0.9245  0.7922
    , hsl 21.46   0.6313  0.4255
    , hsl 29.88   1.0     0.5
    , hsl 33.8    0.9726  0.7137
    , hsl 60.0    1.0     0.8
    , hsl 91.76   0.5705  0.7078
    , hsl 116.38  0.5686  0.4
    , hsl 200.66  0.5214  0.7706
    , hsl 204.16  0.7062  0.4137
    , hsl 269.03  0.4326  0.4216
    , hsl 280.0   0.3051  0.7686
    , hsl 359.4   0.7945  0.4961
    ]
  , [ hsl 0.0     0.0     0.949
    , hsl 4.68    0.9059  0.8333
    , hsl 34.77   0.9778  0.8235
    , hsl 40.5    0.4348  0.8196
    , hsl 60.0    1.0     0.9
    , hsl 108.95  0.4872  0.8471
    , hsl 207.5   0.4615  0.7961
    , hsl 285.6   0.3165  0.8451
    , hsl 329.14  0.8974  0.9235
    ]
  , [ hsl 0.0     0.0     0.8
    , hsl 24.44   0.9529  0.8333
    , hsl 35.68   0.5692  0.8725
    , hsl 50.37   1.0     0.8412
    , hsl 80.45   0.6875  0.8745
    , hsl 153.19  0.4476  0.7941
    , hsl 219.31  0.3867  0.8529
    , hsl 322.86  0.6563  0.8745
    ]
  , [ hsl 0.0     0.0     0.6
    , hsl 21.9    0.6117  0.4039
    , hsl 29.88   1.0     0.5
    , hsl 60.0    1.0     0.6
    , hsl 118.22  0.4056  0.4882
    , hsl 206.98  0.5397  0.4686
    , hsl 292.24  0.3527  0.4725
    , hsl 328.47  0.8806  0.7373
    , hsl 359.41  0.7953  0.498
    ]
  , [ hsl 0.0     0.0     0.702
    , hsl 16.75   0.9625  0.6863
    , hsl 35.56   0.609   0.7392
    , hsl 49.04   1.0     0.5922
    , hsl 82.73   0.6286  0.5882
    , hsl 161.09  0.4299  0.5804
    , hsl 221.61  0.3735  0.6745
    , hsl 323.23  0.6596  0.7235
    ]
  , [ hsl 0.0     0.0     0.851
    , hsl 6.13    0.9448  0.7157
    , hsl 31.74   0.9748  0.6882
    , hsl 52.5    1.0     0.7176
    , hsl 60.0    1.0     0.851
    , hsl 82.05   0.6393  0.6412
    , hsl 108.95  0.4872  0.8471
    , hsl 169.71  0.443   0.6902
    , hsl 204.58  0.4854  0.6647
    , hsl 247.5   0.3019  0.7922
    , hsl 299.02  0.3161  0.6216
    , hsl 329.36  0.8868  0.8961
    ]
  ]
  where
  hsl h s l = ColorHSL {h, s, l}


runQualitativeGenerator ∷ Int → Color → QualitativeGenerator → Array Color
runQualitativeGenerator n seedColor (QualitativeGenerator {colors}) =
  takeer seedColor (fromFoldable $ foldr foldFunc List.Nil colors) n
  where
  distance start end = abs $ end - start
  seed = Color.toHSLA seedColor
  foldFunc (ColorHSL color) rest =
    let
      mixed = {h: color.h, s: (color.s + seed.s) / 2.0, l: (color.l + seed.l) / 2.0 }
    in
      if hslDistance seed mixed > 10.0 then
        Cons (Color.hsla mixed.h mixed.s mixed.l 1.0) rest
      else
        rest
  takeer seed arr count = huesort $ take (count - 1) arr <> [seed]
  huesort = sortBy huesorter
  huesorter a b =  compare (Color.toHSLA a).h (Color.toHSLA b).h
  -- TODO use Lab for persceptual distance
  hslDistance ∷ ∀ r z
    . { h ∷ Number , s ∷ Number , l ∷ Number | r }
    → { h ∷ Number , s ∷ Number , l ∷ Number | z }
    → Number
  hslDistance a b = sqrt $ sqr (hueDistance a_h b_h) + sqr (a_s - b_s)  + sqr (a_l - b_l)
    where
    hueDistance a' b' = d'/ 1.8
      where
      d = abs (a' - b')
      d' = if d < 180.0 then d else 360.0 - d
    a_h = a.h
    b_h = b.h
    a_s = a.s * 100.0
    b_s = b.s * 100.0
    a_l = a.l * 100.0
    b_l = b.l * 100.0
  sqr a = a * a

runSequentialGenerator ∷ Int → Color → SequentialGenerator → Array Color
runSequentialGenerator n seed (SequentialGenerator spec) =
  (mkRunner $ mkSequentialPalette spec)
  seed
  n

runDivergingGenerator ∷ Int → Color → DivergingGenerator → Array Color
runDivergingGenerator n seed (DivergingGenerator spec) = (mkRunner scale) seed n
  where
  { sequentialGenerator: SequentialGenerator sequentialGenerator
  , startColorHueShift
  } = spec
  scale = \color →
    let
      hsl = Color.toHSLA color
      start = Scale.reverseStops
        $ mkSequentialPalette sequentialGenerator
        $ Color.hsla (startColorHueShift + hsl.h) hsl.s hsl.l hsl.a
      end = mkSequentialPalette sequentialGenerator color
    in start `Scale.combineStops 0.5` end

sequentialToCSSGradient ∷  Color → SequentialGenerator → CSS.BackgroundImage
sequentialToCSSGradient seed g = mkGradient $ runSequentialGenerator 5 seed g

divergingToCSSGradient ∷ Color → DivergingGenerator → CSS.BackgroundImage
divergingToCSSGradient seed g = mkGradient $ runDivergingGenerator 10 seed g

unsafeNonEmpty ∷ Array ~> NonEmptyList
unsafeNonEmpty arr = unsafePartial $ fromJust $ NEL.fromFoldable arr

mkGradient ∷ Array Color → CSS.BackgroundImage
mkGradient colors = CSS.fromString
  $ "linear-gradient(to right, " <> intercalate ", " (map Color.cssStringHSLA colors) <> ")"

mkRunner ∷ (Color → Scale.ColorStops) → PaletteRunner
mkRunner f c n = fromFoldable $ Scale.colors' (Scale.cubehelixSample $ f c) n


mkSequentialPalette
  ∷ SequentialGeneratorSpec
  → Color
  → Scale.ColorStops
mkSequentialPalette palette inputColor = Scale.ColorStops startColor stops endColor
  where
  { hueShift
  , lightnessRange: Range lightnessRange
  , darknessRange: Range darknessRange
  } = palette
  input = Color.toHSLA inputColor
  endColor = Color.hsla
    input.h
    (quadratic 0.5 0.90 input.s)
    (quadratic darknessRange.min darknessRange.max input.l)
    input.a
  end = Color.toHSLA endColor
  start =
    { h: end.h + hueShift
    , s: quadratic 0.4 0.70 end.s
    , l: linear lightnessRange.min lightnessRange.max  input.l
    , a: end.a
    }
  startColor = Color.hsla start.h start.s start.l start.a
  absHueShift = (abs hueShift % 180.0)
  stops = Nil

type Easing = Number → Number → Number → Number

polynomial ∷ Number → Easing
polynomial power start end progress = (pow progress power) * (end - start) + start

linear ∷ Easing
linear = polynomial 1.0

quadratic ∷ Easing
quadratic = polynomial 2.0
-}
