#' Shift timestamps for each recorder
#'
#' Applies recorder-level adjustments (lag or lead) to synchronize timestamps across recorders.
#'
#' @param annots A dataframe containing at least `tstart` and `tend` columns with
#'   values to be adjusted, and a `rec` column with recorder IDs for matching with lags.
#'   AviaNZ annotations can be read into this form directly with [readAnnots()] or
#'   [readAnnotsFile()].
#'
#' @param lags A dataframe containing two columns: `rec` with recorder IDs,
#'   and `lag` with the lags to be applied to the each recorder. Recorders will be matched
#'   with `data`, and `lag` simply added to the `tstart` and `tend` of each annotation.
#'   Lags can be positive, zero or negative (i.e. leads) numbers, provided on the same
#'   scale as the time in `tstart` or `tend`, so typically seconds.
#'   Lags must be provided for all recorders present in data (provide 0 if some
#'   recorders do not need adjustment).
#'
#' @return The input dataframe with `tstart` and `tend` values adjusted.
#' @export
#'
#' @examples
#' # Read annotations from two recorders
#' annotdir = system.file("extdata", package="avianz2r", mustWork=TRUE)
#' annots <- readAnnots(annotdir)
#' # Adjust the second recorder by +10 s. The third recorder is not present in data
#' clocklags = data.frame(rec=c("recA", "recB", "recC"),
#'                        lag=c(0, 10, -5))
#' lag_clocks(annots, clocklags)
lag_clocks <- function(annots, lags){
    # Format checks:
    if(!is.data.frame(annots) | !"rec" %in% colnames(annots) |
       !"tstart" %in% colnames(annots) | !"tend" %in% colnames(annots)){
        stop("Data malformed, must be a dataframe with 'rec', 'tstart' and 'tend' columns.")
    }
    if("lags" %in% colnames(annots)){
        stop("Data already contains a 'lag' column. Rename that if it should be kept.")
    }
    if(!is.data.frame(lags) | !"lag" %in% colnames(lags)){
        stop("Lags malformed, must be a dataframe with 'rec' and 'lag' columns.")
    }
    not_in_lags = setdiff(annots$rec, lags$rec)
    not_in_data = setdiff(lags$rec, annots$rec)
    if(!all(annots$rec %in% lags$rec)){
        stop(paste("Clock shifts missing for some recorders:", not_in_lags))
    }
    if(length(not_in_data)>0){
        warning(paste("These recorders were not found in the data:", not_in_data))
    }
    if(anyDuplicated(lags$rec)){
        stop("Only one lag can be provided for each recorder")
    }

    # Actual adjustment:
    annots = dplyr::left_join(annots, lags, by="rec") %>%
        dplyr::mutate(tstart = tstart+lag, tend = tend+lag) %>%
        dplyr::select(!lag)

    return(annots)
}


#' Convert annotations to a call history
#'
#' Converts a dataframe of timestamped annotations into a shape compatible
#'   with [ascr::create.capt()].
#'
#' Acoustic SCR requires identifying which detections in a survey represent
#'   re-captures of the same cue. This function does that simply by using
#'   temporal overlap: any annotations that overlap in time by at least `gap`
#'   seconds are assumed to be recaptures of the same call, and assigned a
#'   shared call ID. Note that this will merge both recaptures across recorders,
#'   and also annotations within each recorder, such as when several annotations
#'   are produced for the same call.
#'   Note that any further annotation details (call type, species) will be ignored,
#'   i.e. calls labelled as different species, overlapping in time, will still be
#'   merged. If you wish to use such information, split the data by the groups yourself
#'   and apply this function to each separately.
#'
#' @param annots A dataframe containing at least `tstart` and `tend` columns,
#'   and a `rec` column with recorder IDs. The timestamp columns are typically
#'   numeric or POSIXt. AviaNZ annotations can be read into
#'   this form directly with [readAnnots()] or [readAnnotsFile()].
#' @param gap A number in the same units as `tstart,tend`.
#'   Annotations that are separated by this much or more will be merged into
#'   a single call. Default 0 merges any annotations that overlap or touch;
#'   setting it larger is useful if the calls are often detected as multiple
#'   annotations which should be merged into a single unit. Setting it below 0
#'   will only merge annotations that overlap by at least `gap` time units, which
#'   could help avoid falsely merging distinct calls if they occur frequently.
#'
#' @return A dataframe where... ??? TODO describe exactly the columns
#'   Recaptures of the same call will be assigned the same ID.
#' @export
#'
#' @examples
#' ## rec1 captures what looks like two pieces of the same call
#' ## rec2 recaptures part of the same call, and another cue:
#' annots <- data.frame(rec=c("rec1", "rec1", "rec2", "rec2"),
#'   tstart=c(0.0, 5.5, 1.5, 9), tend=c(5.0, 6.5, 6.0, 13))
#' ## merge into two unique calls + 1 recapture:
#' annots_to_calls(annots, gap=0)
#' ## or we can separate out the short annotation in rec1 as another call:
#' annots_to_calls(annots, gap=-1)
annots_to_calls <- function(annots, gap=0){
    # Expect all annotations from the same species.
    # I don't see why anyone would analyze multiple species together,
    # but it doesn't really break anything for this, so just warn.
    if("species" %in% colnames(annots)){
        if(length(unique(annots$species))>1){
            warning(paste("Multiple species annotations detected. Note that species labels",
                          " will be ignored when merging the annotations into recaptures.",
                          " Filter by species and apply this function separately if this is not desired."))
        }
    }

    # Importantly, checking if times are actually in UTC,
    # to minimize any DST gap issues
    if(!is.data.frame(annots) | !"rec" %in% colnames(annots) |
       !"tend" %in% colnames(annots) | !"tstart" %in% colnames(annots)){
        stop("Annotations malformed, must be a dataframe with 'rec', 'tstart' and 'tend' columns")
    }
    if(!is.numeric(gap) | length(gap)>1){
        stop("Gap must be a single number.")
    }

    annots = annots[order(annots$tstart),]

    # The output at first will have 1 row per each input row,
    # and some duplicates (call pieces for the same recorder)
    # will be removed later
    output = data.frame(id=0, rec=annots$rec)

    # Group overlapping annotations into calls:
    # (doesn't actually merge, just assigns appropriate callIDs
    # indicating pieces or recaptures of the same call)
    currend = -Inf  # (ensures that first row always starts a new call)
    for(i in 1:nrow(annots)){
        if(annots$tstart[i]<=currend+gap){
            # same call continues
            currend = max(currend, annots$tend[i])
        } else {
            # end prev call, start a new one
            # (callID is just the timestamp now, could change it)
            callID = as.character(annots$tstart[i])
            currend = annots$tend[i]
        }
        # In both cases, a new row is created in output
        # (to add a new recorder/piece of the same call,
        # or a new call)
        output$id[i] = callID
    }
    # Note that this could all be vectorized w/ cummax if speed actually becomes
    # a problem.

    # Drop duplicates, i.e. pieces of the same call:
    # (calls like [1,3] and [3,4] on the same rec were both included so far)
    output = unique(output)

    # Prepare for create.capt:
    output = output[,c("session", "id", "rec")]
    # TODO ensure other aux info is not dropped
    return(output)
}



# TODO could actually add a separate function to merge call pieces.
# That would allow merging within each recorder, while the function
# above merges across recorders as well.
# annots_to_presabs <- function(annots, occasiongap="8hr"){
#     recs = unique(annots$rec)
#     # Recorder names may be modified to create acceptable column names
#     if(any(recs!=make.names(recs, unique=T))){
#         warning("Recorder names were modified to match R column name requirements")
#         annots$rec = make.names(annots$rec, unique=T)
#     }
#
#     # Convert into 0/1 by second
#     for(r in 1:nrow(annots)){
#         day = annots$relday[r]+1
#         rec = annots$rec[r]
#         # Determine the start of the current occasion
#         daystart = ... # TODO determine
#         callstart = floor(annots$tstart[r]-daystart)
#         callend = ceiling(annots$tend[r]-daystart)
#         presence[[day]][callstart:callend, rec] = 1
#     }
#
#     # Flatten over days into a single df
#     presence = bind_rows(presence)
#     return(presence)
# }

# TODO grouped merging (pass groups as argument)

# TODO convert to 0-1 for binary concordance metrics?


#' Title
#'
#' @param calls
#' @param gpspos
#' @param survey.recs
#'
#' @return A named list with `calls` and `traps` elements which can be directly
#'   used in [ascr::create.capt()]. The main difference from inputs is that `calls`
#'   will have ... TODO
#' @export
#'
#' @examples
#'
#' # Recorder positions in eastings-northings (preferrably in meters).
#' # You will likely want to project these from GPS, e.g. using rgdal::spTransform
#' gpspos = data.frame(rec=c("recA", "recB"),
#'                     east=c(-50, 50),
#'                     north=c(0, 175))
#'
#' # format calls and traps appropriately
#' l = prepare_capt()
#'
#' # Prepare the actual capt object and run acoustic SCR
#' library(ascr)
#' capt = ascr::create.capt(l$calls, traps=l$traps)
#' mask = ascr::create.mask(traps, buffer=700)
#' cr = ascr::fit.ascr(capt, traps, mask)
prepare_capt <- function(calls, gpspos, survey.recs=NULL){
    # Input checks
    if(!is.data.frame(calls) | !"rec" %in% colnames(calls) |
       !"id" %in% colnames(calls)){
        stop("Call history malformed, must be a dataframe with 'rec' and 'id' columns")
    }
    if(!is.data.frame(gpspos) | !"rec" %in% colnames(gpspos) |
       !"east" %in% colnames(gpspos) | !"north" %in% colnames(gpspos)){
        stop("Recorder positions malformed, must be a dataframe with 'rec', 'east' and 'north' columns")
    }
    # recid will be merged from gpspos, and so cannot be in the input:
    calls$recid = NULL

    # Remove any recorders that were broken/not used in this survey
    if(!is.null(survey.recs)){
        if(!all(survey.recs %in% gpspos$rec)){
            stop("Some of the specified survey recorders not present in the position data.")
        }
        gpspos = gpspos[which(gpspos$rec %in% survey.recs),]
        if(nrow(gpspos)==0){
            stop("All recorders dropped. Check if survey.recs is specified correctly.")
        }
    }
    if(any(duplicated(gpspos$rec))) stop("Recorder positions must be unique")

    # In theory some recorders may naturally have no calls
    # or the user may choose to not use them (not provide positions)
    # so just warn in these cases:
    not_in_gpspos = setdiff(calls$rec, gpspos$rec)
    not_in_calls = setdiff(gpspos$rec, calls$rec)
    if(length(not_in_gpspos)>0){
        warning(paste("Dropping these recorders for which no positions provided:", not_in_gpspos))
    }
    if(length(not_in_calls)>0){
        warning(paste("No calls detected for these recorders:", not_in_calls))
    }

    # create a numeric index column, in case rec names are characters
    gpspos$recid = 1:nrow(gpspos)
    message(sprintf("Using %d recorders.", nrow(gpspos)))

    # Call history prep:
    calls$occ = 1  # add dummy occasion parameter, ignored in ascr
    calls$session = 1  # add session parameter
    # TODO user should be able to have flexibility in this,
    # but not sure where to pass it

    # Replace recorder names with numeric IDs
    calls = as.data.frame(dplyr::inner_join(calls, gpspos, by="rec"))

    # Reorder columns per ascr::create.capt requirements:
    cols_start = c("session", "id", "occ", "recid")
    cols_rest = setdiff(colnames(calls), cols_start)
    calls = calls[,c(cols_start, cols_rest)]

    # The remaining columns will be dropped by ascr::create.capt,
    # so warn here - it may suggest that the user didn't name something properly:
    cols_accepted = c("bearing", "dist", "ss", "toa",
                      "east", "north", "rec") # columns that we add
    cols_unknown = setdiff(cols_rest, cols_accepted)
    if(length(cols_unknown)>0){
        warning(paste("The following columns will be ignored by ascr:",
                      cols_unknown))
    }

    # Trap prep:
    # require input already projected to northings-eastings
    traps = as.matrix(gpspos[,c("east", "north")])

    return(list(calls=calls, traps=traps))
}
