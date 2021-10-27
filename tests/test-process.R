library(avianz2r)
df3 = readAnnots("3comp/")
clocklags = data.frame(rec=c("recA", "recB"),
                       lag=c(-5, 10))
dflagged = lag_clocks(df3, clocklags)

reftstart = structure(c(1516670620.38179, 1516670633.81926, 1516670643.5815,
                        1516670629.63545, 1516670629.63545, 1516670659.80962, 1516670650.93487,
                        1516674845.38179, 1516674858.81926, 1516674868.5815, 1516674854.63545,
                        1516674854.63545, 1516674884.80962, 1516674875.93487),
                      tzone = "UTC", class = c("POSIXct", "POSIXt"))
reftend = structure(c(1516670629.49165, 1516670639.39768, 1516670647.76531,
                      1516670669.06473, 1516670669.06473, 1516670675.91097, 1516670656.51329,
                      1516674854.49165, 1516674864.39768, 1516674872.76531, 1516674894.06473,
                      1516674894.06473, 1516674900.91097, 1516674881.51329),
                    tzone = "UTC", class = c("POSIXct", "POSIXt"))

# time must be lagged appropriately
stopifnot(all.equal(dflagged$tstart, reftstart))
stopifnot(all.equal(dflagged$tend, reftend))

# all other columns must be unchanged
stopifnot(all.equal(df3[,-c(1,2)],
                    dflagged[,-c(1,2)]))
