module Color.Palette.Easing where

import Prelude

import Math (pow)

type Easing = Number → Number → Number → Number

polynomial ∷ Number → Easing
polynomial power start end progress = (pow progress power) * (end - start) + start

linear ∷ Easing
linear = polynomial 1.0

quadratic ∷ Easing
quadratic = polynomial 2.0
