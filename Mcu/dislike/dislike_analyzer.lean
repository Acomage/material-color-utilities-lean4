import Mcu.hct.hct

namespace dislike_analyzer

def isDisliked (h : hct) : Bool :=
  let huePasses := h.hue.round >= 90.0 && h.hue.round <= 111.0
  let chromaPasses := h.chroma.round > 16.0
  let tonePasses := h.tone.round < 65.0
  huePasses && chromaPasses && tonePasses

def fixIfDisliked (h : hct) : hct :=
  if isDisliked h then
    hct.from_ h.hue h.chroma 70.0
  else h

end dislike_analyzer
