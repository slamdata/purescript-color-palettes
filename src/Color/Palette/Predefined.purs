module Color.Palette.Predefined where

import Prelude

import Color as C
import Color.Palette as CP
import Data.Int as Int
import Data.List ((:))
import Data.List as L
import Data.List.NonEmpty as NEL
import Data.Maybe as M
import Data.NonEmpty ((:|))
import Data.Record as R
import Data.Symbol (SProxy(..))

hueShifts ∷ Number → L.List Number → NEL.NonEmptyList Number
hueShifts mid hues =
  M.fromMaybe (pure mid)
    $ NEL.fromList
    $ L.reverse hues
    <> L.singleton mid
    <> map (_ * -1.0) hues


sequentials ∷ NEL.NonEmptyList CP.Palette
sequentials = map CP.Sequential do
  hueShift ← hueShifts 0.0 $ map (mul 30.0 <<< Int.toNumber) $ L.range 1 12
  NEL.NonEmptyList
    $  { hueShift, darknessRange: firstDarkness, lightnessRange: firstLightness }
    :| { hueShift, darknessRange: secondDarkness, lightnessRange: secondLightness }
    :  L.Nil
  where
  firstDarkness = { min: 0.1, max: 0.5 }
  firstLightness = { min: 0.85, max: 0.97 }
  secondDarkness = { min: 0.0, max: 0.2 }
  secondLightness = { min: 0.92, max: 1.0 }

divergings ∷ NEL.NonEmptyList CP.Palette
divergings = map CP.Diverging do
  hueShift ← hueShifts 0.0 $ 45.0 : 90.0 : L.Nil
  seq ← NEL.NonEmptyList
    $  { hueShift, darknessRange: { min: 0.1, max: 0.3 }, lightnessRange: { min: 0.95, max: 1.0 } }
    :| { hueShift, darknessRange: { min: 0.2, max: 0.5 }, lightnessRange: { min: 0.95, max: 1.0 } }
    :  L.Nil
  secondaryHueShift ← NEL.NonEmptyList $ -135.0 :| -90.0 : 90.0 : 135.0 : 180.0 : L.Nil
  pure $ R.insert (SProxy ∷ SProxy "startColorHueShift") secondaryHueShift seq

qualitatives ∷ NEL.NonEmptyList CP.Palette
qualitatives = map (CP.Qualitative <<< { colors: _ })
  $ NEL.NonEmptyList
    $ (NEL.NonEmptyList
        $  C.hsl 0.0     0.0     0.4
        :| C.hsl 24.29   0.785   0.4196
        :  C.hsl 29.24   0.9675  0.7588
        :  C.hsl 60.0    1.0     0.8
        :  C.hsl 120.0   0.4066  0.6431
        :  C.hsl 214.0   0.5172  0.4549
        :  C.hsl 265.26  0.3065  0.7569
        :  C.hsl 328.49  0.9835  0.4745
        :  L.Nil)
    :| (NEL.NonEmptyList
        $  C.hsl 0.0     0.0     0.4
        :| C.hsl 25.95   0.9817  0.4294
        :  C.hsl 38.98   0.7026  0.3824
        :  C.hsl 44.47   0.9828  0.4549
        :  C.hsl 88.24   0.6939  0.3843
        :  C.hsl 162.14  0.7081  0.3627
        :  C.hsl 244.48  0.3059  0.5706
        :  C.hsl 329.37  0.7983  0.5333
        :  L.Nil)
    :  (NEL.NonEmptyList
        $  C.hsl 0.61    0.9245  0.7922
        :| C.hsl 21.46   0.6313  0.4255
        :  C.hsl 29.88   1.0     0.5
        :  C.hsl 33.8    0.9726  0.7137
        :  C.hsl 60.0    1.0     0.8
        :  C.hsl 91.76   0.5705  0.7078
        :  C.hsl 116.38  0.5686  0.4
        :  C.hsl 200.66  0.5214  0.7706
        :  C.hsl 204.16  0.7062  0.4137
        :  C.hsl 269.03  0.4326  0.4216
        :  C.hsl 280.0   0.3051  0.7686
        :  C.hsl 359.4   0.7945  0.4961
        :  L.Nil)
    :  (NEL.NonEmptyList
        $  C.hsl 0.0     0.0     0.949
        :| C.hsl 4.68    0.9059  0.8333
        :  C.hsl 34.77   0.9778  0.8235
        :  C.hsl 40.5    0.4348  0.8196
        :  C.hsl 60.0    1.0     0.9
        :  C.hsl 108.95  0.4872  0.8471
        :  C.hsl 207.5   0.4615  0.7961
        :  C.hsl 285.6   0.3165  0.8451
        :  C.hsl 329.14  0.8974  0.9235
        :  L.Nil)
    :  (NEL.NonEmptyList
        $  C.hsl 0.0     0.0     0.8
        :| C.hsl 24.44   0.9529  0.8333
        :  C.hsl 35.68   0.5692  0.8725
        :  C.hsl 50.37   1.0     0.8412
        :  C.hsl 80.45   0.6875  0.8745
        :  C.hsl 153.19  0.4476  0.7941
        :  C.hsl 219.31  0.3867  0.8529
        :  C.hsl 322.86  0.6563  0.8745
        :  L.Nil)
    :  (NEL.NonEmptyList
        $  C.hsl 0.0     0.0     0.6
        :| C.hsl 21.9    0.6117  0.4039
        :  C.hsl 29.88   1.0     0.5
        :  C.hsl 60.0    1.0     0.6
        :  C.hsl 118.22  0.4056  0.4882
        :  C.hsl 206.98  0.5397  0.4686
        :  C.hsl 292.24  0.3527  0.4725
        :  C.hsl 328.47  0.8806  0.7373
        :  C.hsl 359.41  0.7953  0.498
        :  L.Nil)
    :  (NEL.NonEmptyList
        $  C.hsl 0.0     0.0     0.702
        :| C.hsl 16.75   0.9625  0.6863
        :  C.hsl 35.56   0.609   0.7392
        :  C.hsl 49.04   1.0     0.5922
        :  C.hsl 82.73   0.6286  0.5882
        :  C.hsl 161.09  0.4299  0.5804
        :  C.hsl 221.61  0.3735  0.6745
        :  C.hsl 323.23  0.6596  0.7235
        :  L.Nil)
    :  (NEL.NonEmptyList
        $  C.hsl 0.0     0.0     0.851
        :| C.hsl 6.13    0.9448  0.7157
        :  C.hsl 31.74   0.9748  0.6882
        :  C.hsl 52.5    1.0     0.7176
        :  C.hsl 60.0    1.0     0.851
        :  C.hsl 82.05   0.6393  0.6412
        :  C.hsl 108.95  0.4872  0.8471
        :  C.hsl 169.71  0.443   0.6902
        :  C.hsl 204.58  0.4854  0.6647
        :  C.hsl 247.5   0.3019  0.7922
        :  C.hsl 299.02  0.3161  0.6216
        :  C.hsl 329.36  0.8868  0.8961
        :  L.Nil)
    : L.Nil
