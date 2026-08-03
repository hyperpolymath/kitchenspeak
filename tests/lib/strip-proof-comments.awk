# Strip comments from proof/source files before scanning for dangerous constructs.
#
# Line-based filtering is not enough: a Coq (* ... *) or Lean /- ... -/ block
# spans lines, and a CONTINUATION line carries no marker of its own. Measured
# 2026-07-29: verification/proofs/coq/TypeSafety.v:6 ("All proofs must be
# complete - NO Admitted allowed.") is line 6 of a block comment and was
# reported as a dangerous construct -- i.e. the repo was failed for DOCUMENTING
# that it forbids Admitted.
#
# Handles: (* *)  /- -/  {- -}  /* */  plus the line forms -- and //
BEGIN { inblk = 0 }
{
  line = $0; out = ""; i = 1; n = length(line)
  while (i <= n) {
    two = substr(line, i, 2)
    if (inblk) {
      if (two == close_tok) { inblk = 0; i += 2; continue }
      i++; continue
    }
    if (two == "(*") { inblk = 1; close_tok = "*)"; i += 2; continue }
    if (two == "/-") { inblk = 1; close_tok = "-/"; i += 2; continue }
    if (two == "{-") { inblk = 1; close_tok = "-}"; i += 2; continue }
    if (two == "/*") { inblk = 1; close_tok = "*/"; i += 2; continue }
    if (two == "--" || two == "//") { break }
    out = out substr(line, i, 1); i++
  }
  print out
}
