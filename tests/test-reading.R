library(avianz2r)
df3 <- readAnnots("3comp/")
df2 <- readAnnots("2comp/")
dfref = structure(list(tstart = structure(c(1516670625.38179, 1516670638.81926, 1516670648.5815,
                                  1516670634.63545, 1516670634.63545, 1516670664.80962,
                                  1516670655.93487, 1516674835.38179, 1516674848.81926,
                                  1516674858.5815, 1516674844.63545, 1516674844.63545,
                                  1516674874.80962, 1516674865.93487), tzone = "UTC",
                                  class = c("POSIXct","POSIXt")),
                       tend = structure(c(1516670634.49165, 1516670644.39768, 1516670652.76531,
                                  1516670674.06473, 1516670674.06473, 1516670680.91097,
                                  1516670661.51329, 1516674844.49165, 1516674854.39768,
                                  1516674862.76531, 1516674884.06473, 1516674884.06473,
                                  1516674890.91097, 1516674871.51329), tzone = "UTC",
                                  class = c("POSIXct", "POSIXt")),
            freqmin = c(1034, 1403, 1314, 5375, 5375, 471, 1026, 1034, 1403, 1314, 5375, 5375, 471, 1026),
            freqmax = c(4058, 4776, 4798, 6485, 6485, 3800, 3622, 4058, 4776, 4798, 6485, 6485, 3800, 3622),
            species = c("Don't Know", "Kiwi (Little Spotted)", "Kiwi (Little Spotted)", "Morepork", "Kakapo",
                        "Bellbird/Tui", "Falcon (NZ)", "Don't Know", "Kiwi (Little Spotted)",
                        "Kiwi (Little Spotted)", "Morepork", "Kakapo", "Bellbird/Tui", "Falcon (NZ)"),
            certainty = c(0, 100, 100, 100, 100, 100, 50, 0, 100, 100, 100, 100, 100, 50),
            filter = c("M", "M", "M", "M", "M", "M", "M", "M", "M", "M", "M", "M", "M", "M"),
            calltype = c(NA, NA, "Female", NA, NA, NA, NA, NA, NA, "Female", NA, NA, NA, NA),
            ftime = structure(c(1516670625, 1516670625, 1516670625, 1516670625, 1516670625, 1516670625,
                               1516670625, 1516674835, 1516674835, 1516674835, 1516674835, 1516674835,
                               1516674835, 1516674835), tzone = "UTC", class = c("POSIXct", "POSIXt")),
            rec = c("recA", "recA", "recA", "recA", "recA", "recA", "recA", "recB", "recB", "recB",
                    "recB", "recB", "recB", "recB")), row.names = c(NA, -14L),
            class = c("tbl_df", "tbl", "data.frame"))

stopifnot(all.equal(df3, dfref))
stopifnot(all.equal(df2, df3))

