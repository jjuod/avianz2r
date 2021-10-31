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

#  ----------------

annots = data.frame(rec=c("rec1", "rec1", "rec2", "rec2"),
  tstart=c(0.0, 5.5, 1.5, 9), tend=c(5.0, 6.5, 6.0, 13),
  dist=c(100, 75, 20, 66))

callsref = data.frame(id=c("0","0","9"), rec=c("rec1", "rec2", "rec2"),
                       dist=c(100,20,66))

## merge into two unique calls + 1 recapture:
calls = annots_to_calls(annots, gap=0)
stopifnot(all.equal(calls, callsref, check.attributes=F))

## try out groups:
# should be identical to above:
calls2 = annots_to_calls(annots, groups=list(c("rec1", "rec2")))
stopifnot(all.equal(calls2, calls))
# should not merge across the recorders, only within:
callssep = annots_to_calls(annots, groups=list("rec1", "rec2"))
stopifnot(nrow(callssep)>nrow(calls))
