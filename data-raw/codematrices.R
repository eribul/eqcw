
# British
uk <-
  codematrix(
    rep(0, 5),
    c(.069, .104, .036, .123, .071),
    c(.314, .214, .094, .386, .236),
    const = c(1, .081, .35)
  )

# Swedish
se <-
  codematrix(
    rep(0, 5),
    c(.0666, .0276, .1012, .0345, .0552),
    c(.1247, .0276, .1355, .0904, .2077),
    const = c(.9694, 0, .0433)
  )


# Save as internal datasets
devtools::use_data(se, uk, internal = TRUE, overwrite = TRUE)
