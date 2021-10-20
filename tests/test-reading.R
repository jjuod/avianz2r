library(avianz2r)
df3 <- readAnnots("3comp/")
df2 <- readAnnots("2comp/")
dfref = structure(list(tstart = c(0.381786312368715, 13.8192642706131,
                                  23.5814968287526, 9.63545031712474, 9.63545031712474, 39.8096236786469,
                                  30.934866807611, 0.381786312368715, 13.8192642706131, 23.5814968287526,
                                  9.63545031712474, 9.63545031712474, 39.8096236786469, 30.934866807611),
                       tend = c(9.49164708475699, 19.3976828752643, 27.765310782241,
            49.0647272727273, 49.0647272727273, 55.9109682875264, 36.5132854122621,
            9.49164708475699, 19.3976828752643, 27.765310782241, 49.0647272727273,
            49.0647272727273, 55.9109682875264, 36.5132854122621),
            freqmin = c(1034, 1403, 1314, 5375, 5375, 471, 1026, 1034, 1403, 1314, 5375, 5375, 471, 1026),
            freqmax = c(4058, 4776, 4798, 6485, 6485, 3800, 3622, 4058, 4776, 4798, 6485, 6485, 3800, 3622),
            species = c("Don't Know", "Kiwi (Little Spotted)", "Kiwi (Little Spotted)", "Morepork", "Kakapo",
                        "Bellbird/Tui", "Falcon (NZ)", "Don't Know", "Kiwi (Little Spotted)",
                        "Kiwi (Little Spotted)", "Morepork", "Kakapo", "Bellbird/Tui", "Falcon (NZ)"),
            certainty = c(0, 100, 100, 100, 100, 100, 50, 0, 100, 100, 100, 100, 100, 50),
            filter = c("M", "M", "M", "M", "M", "M", "M", "M", "M", "M", "M", "M", "M", "M"),
            calltype = c(NA, NA, "Female", NA, NA, NA, NA, NA, NA, "Female", NA, NA, NA, NA),
            time = structure(c(1516670625, 1516670625, 1516670625, 1516670625, 1516670625, 1516670625,
                               1516670625, 1516674835, 1516674835, 1516674835, 1516674835, 1516674835,
                               1516674835, 1516674835), tzone = "UTC", class = c("POSIXct", "POSIXt")),
            rec = c("recA", "recA", "recA", "recA", "recA", "recA", "recA", "recB", "recB", "recB",
                    "recB", "recB", "recB", "recB")), row.names = c(NA, -14L), class = c("tbl_df", "tbl", "data.frame"))

all.equal(df3, dfref)
all.equal(df2, df3)

