inductive Variant
  | monochrome
  | neutral
  | tonalSpot
  | vibrant
  | expressive
  | content
  | fidelity
  | rainbow
  | fruitSalad

namespace Variant

def label : Variant → String
  | .monochrome => "monochrome"
  | .neutral    => "neutral"
  | .tonalSpot  => "tonal spot"
  | .vibrant    => "vibrant"
  | .expressive => "expressive"
  | .content    => "content"
  | .fidelity   => "fidelity"
  | .rainbow    => "rainbow"
  | .fruitSalad => "fruit salad"

def description : Variant → String
  | .monochrome =>
    "All colors are grayscale, no chroma."
  | .neutral =>
    "Close to grayscale, a hint of chroma."
  | .tonalSpot =>
    "Pastel tokens, low chroma palettes (32).\nDefault Material You theme at 2021 launch."
  | .vibrant =>
    "Pastel colors, high chroma palettes. (max).\nThe primary palette's chroma is at maximum.\nUse Fidelity instead if tokens should alter their tone to match the palette vibrancy."
  | .expressive =>
    "Pastel colors, medium chroma palettes.\nThe primary palette's hue is different from source color, for variety."
  | .content =>
    "Almost identical to Fidelity.\nTokens and palettes match source color.\nPrimary Container is source color, adjusted to ensure contrast with surfaces.\n\nTertiary palette is analogue of source color.\nFound by dividing color wheel by 6, then finding the 2 colors adjacent to source.\nThe one that increases hue is used."
  | .fidelity =>
    "Tokens and palettes match source color.\nPrimary Container is source color, adjusted to ensure contrast with surfaces.\nFor example, if source color is black, it is lightened so it doesn't match surfaces in dark mode.\n\nTertiary palette is complement of source color."
  | .rainbow =>
    "A playful theme - the source color's hue does not appear in the theme."
  | .fruitSalad =>
    "A playful theme - the source color's hue does not appear in the theme."

end Variant
