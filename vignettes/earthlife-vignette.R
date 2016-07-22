## ------------------------------------------------------------------------

# The function `get_by_taxon` will be described more fully later.  We want to return an
# `occurrence` object fo illustrate the various methods available:

basic_occ <- get_by_taxon('Poaceae', lower = TRUE)
str(basic_occ)

## ------------------------------------------------------------------------
basic_occ

## ------------------------------------------------------------------------
citation(basic_occ)

