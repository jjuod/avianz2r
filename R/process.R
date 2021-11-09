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
        stop(txtmsg("Clock shifts missing for some recorders:", not_in_lags))
    }
    if(length(not_in_data)>0){
        warning(txtmsg("These recorders were not found in the data:", not_in_data))
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
#' Converts a dataframe of annotation timestamps into call captures and recaptures,
#'   for use in inference models such as acoustic SCR.
#'
#' Acoustic SCR requires identifying which detections in a survey represent
#'   re-captures of the same cue. This function does that simply by using
#'   temporal overlap: any annotations that overlap in time by at least `gap`
#'   seconds are assumed to be recaptures of the same call, and assigned a
#'   shared call ID. Note that this will merge both recaptures across recorders,
#'   and also annotations within each recorder, such as when several annotations
#'   are produced for the same call.
#'   Any further annotation details (call type, species) will be ignored,
#'   i.e. calls labelled as different species, overlapping in time, will still be
#'   merged. If you wish to use such information, split the data by the groups yourself
#'   and apply this function to each separately.
#'
#' @param annots A dataframe containing at least `tstart` and `tend` columns,
#'   and a `rec` column with recorder IDs. The timestamp columns are typically
#'   numeric or POSIXt. AviaNZ annotations can be read into
#'   this form directly with [readAnnots()] or [readAnnotsFile()].
#'   Auxiliary columns appropriate for [ascr::create.capt()] (`bearing`, `dist`, `ss`,
#'   `toa`), if included, will be kept, although not merged cleverly -- if multiple
#'   timestamps on a single recorder are merged into one capture, the
#'   measures of the first of these fragments will be assigned to it.
#' @param gap A number in the same units as `tstart,tend`.
#'   Annotations that are separated by this much or less will be merged into
#'   a single call. Default 0 merges any annotations that overlap or touch;
#'   setting it larger is useful if the calls are often detected as multiple
#'   annotations which should be merged into a single unit. Setting it below 0
#'   will only merge annotations that overlap by at least `gap` time units, which
#'   could help avoid falsely merging distinct calls if they occur frequently.
#' @param groups A list of vectors containing recorder IDs. Each of these vectors
#'   will be processed separately (i.e. calls will not be merged across
#'   these groups even if they coincide in time). This is useful for defining
#'   non-overlapping recorder groups, to avoid false positive recaptures.
#'   See examples.
#'
#' @return A dataframe containing recaptures of each call in long form:
#'   columns `id` (assigned call ID string, typically the start of the earliest
#'   timestamp included in that call) and `rec` (recorder ID on which this
#'   capture or recapture was registered) will always be present,
#'   as well as any auxiliary information in the input that is compatible with
#'   the `ascr` package.
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
#'
#' ## example of using the groups argument:
#' ## say there is a third recorder, but we know that it was too far
#' ## to produce any true recaptures with the others (so any overlaps are
#' ## just coincidences.)
#' ## We can still use it in SCR, but we do not wish to merge its
#' ## annotations, so we define it as a separate group here:
#' annots <- rbind(annots, data.frame(rec="rec3", tstart=3, tend=5))
#' annots_to_calls(annots, groups=list(c("rec1", "rec2"), "rec3"))
annots_to_calls <- function(annots, gap=0, groups=NULL){
    # Expect all annotations from the same species.
    # I don't see why anyone would analyze multiple species together,
    # but it doesn't really break anything for this, so just warn.
    if("species" %in% colnames(annots)){
        if(length(unique(annots$species))>1){
            warning(paste("Multiple species annotations detected. Note that species labels",
                          "will be ignored when merging the annotations into recaptures.",
                          "Filter by species and apply this function separately if this is not desired."))
        }
    }

    if(!is.data.frame(annots) | !"rec" %in% colnames(annots) |
       !"tend" %in% colnames(annots) | !"tstart" %in% colnames(annots)){
        stop("Annotations malformed, must be a dataframe with 'rec', 'tstart' and 'tend' columns")
    }
    if(!is.numeric(gap) | length(gap)>1){
        stop("Gap must be a single number.")
    }
    if(lubridate::is.POSIXlt(annots$tstart) | lubridate::is.POSIXlt(annots$tend)){
        warning(paste("Using local times may give unexpected results if DST shifts fall within",
                "the survey period. Verify the output carefully or switch to POSIXct."))
    }

    if(!is.null(groups)){
        data_recs = unique(annots$rec)
        group_recs = unlist(groups)
        if(!is.list(groups)){
            stop("Groups must be NULL or a list of recorder name vectors.")
        }
        if(any(duplicated(group_recs) | !group_recs %in% data_recs)){
            stop("Recorders specified in groups must be unique and found in the data.")
        }
        not_in_groups = setdiff(data_recs, group_recs)
        if(length(not_in_groups)>0){
            warning(txtmsg("The following recorders were not found in groups",
                          "and will be excluded:", not_in_groups))
        }
    }

    # Given no groups:
    if(is.null(groups) | (is.list(groups) & length(groups)==1)){
        output = annots_to_calls_1group(annots, gap)
    } else {
        output = list()
        for(grix in seq_along(groups)){
            gr = groups[[grix]]
            gr_annots = annots[annots$rec %in% gr,]
            gr_output = annots_to_calls_1group(gr_annots, gap)
            gr_output$id = paste(grix, gr_output$id, sep="_")
            output[[length(output)+1]] = gr_output
        }
        output = as.data.frame(dplyr::bind_rows(output))
    }

    return(output)
}


# Internal function for merging annotations within a single group.
annots_to_calls_1group <- function(annots, gap=0){
    annots = annots[order(annots$tstart),]

    # The output at first will have 1 row per each input row,
    # and some duplicates (call pieces for the same recorder)
    # will be removed later
    output = data.frame(id=0, rec=annots$rec)
    # Copy over any auxiliary info useful for capt
    for(auxcol in c("bearing", "dist", "ss", "toa")){
        if(auxcol %in% colnames(annots)){
            output[,auxcol] = annots[,auxcol]
        }
    }

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
    output = output[!duplicated(output[,1:2]),]

    return(output)
}


#' Merge timestamps with small gaps
#'
#' Merges annotations separated by at most a chosen time gap within each
#' recorder.
#'
#' This is a replacement for [annots_to_calls()], in case you want to only merge
#' subsequent annotations within each recorder and not across groups, e.g. for
#' combining syllables into calls. This is useful if (1) you want a more liberal
#' gap to be applied for within-recorder merges than for between-recorder
#' merges, (2) preserving additional call information is needed -- this function
#' can be modified more easily than [annots_to_calls()].
#'
#' @param annots A dataframe containing at least `tstart` and `tend` columns,
#'   and a `rec` column with recorder IDs. The timestamp columns are typically
#'   numeric or POSIXt. AviaNZ annotations can be read into this form directly
#'   with [readAnnots()] or [readAnnotsFile()]. Any additional columns will be
#'   ignored -- edit this function to add additional merging rules if you wish.
#' @param gap A number in the same units as `tstart,tend`. Annotations that are
#'   separated by this much or less will be merged into a single call. I.e. 0
#'   merges any annotations that overlap or touch; positive `gap` is likely the most
#'   useful for merging syllables, but it can also be set below 0 to only
#'   merge annotations that overlap by at least `gap` time units.
#'
#' @return A dataframe containing `tstart` and `tend` columns corresponding to the
#'   timestamps after merging, and `rec` with recorder ID as provided in the input.
#'
#' @export
#'
#' @examples
#' ## several detections over 2 recorders:
#' annots <- data.frame(rec=c("rec1", "rec1", "rec1", "rec2", "rec2"),
#'   tstart=c(0.0, 3.5, 7.1, 1.5, 5.5), tend=c(3.0, 4.5, 8.1, 6.0, 8.0))
#' ## merge into 3 unique calls:
#' merge_syllables(annots, gap=2)
merge_syllables <- function(annots, gap){
    # Expect all annotations from the same species.
    # I don't see why anyone would analyze multiple species together,
    # but it doesn't really break anything for this, so just warn.
    if("species" %in% colnames(annots)){
        if(length(unique(annots$species))>1){
            warning(paste("Multiple species annotations detected. Note that species labels",
                          "will be ignored when merging the annotations into recaptures.",
                          "Filter by species and apply this function separately if this is not desired."))
        }
    }

    if(!is.data.frame(annots) | !"rec" %in% colnames(annots) |
       !"tend" %in% colnames(annots) | !"tstart" %in% colnames(annots)){
        stop("Annotations malformed, must be a dataframe with 'rec', 'tstart' and 'tend' columns")
    }
    if(!is.numeric(gap) | length(gap)>1){
        stop("Gap must be a single number.")
    }
    if(lubridate::is.POSIXlt(annots$tstart) | lubridate::is.POSIXlt(annots$tend)){
        warning(paste("Using local times may give unexpected results if DST shifts fall within",
                      "the survey period. Verify the output carefully or switch to POSIXct."))
    }

    allrecs = unique(annots$rec)
    output = list()
    for(r in allrecs){
        rec_annots = annots[annots$rec==r,]
        rec_annots = rec_annots[order(rec_annots$tstart),]
        # Same algorithm as for call merging:
        currend = -Inf
        rec_annots$callID = NA
        j = 0  # call counter
        for(i in 1:nrow(rec_annots)){
            if(rec_annots$tstart[i]<=currend+gap){
                # same call continues
                currend = max(currend, rec_annots$tend[i])
            } else {
                # end prev call, start a new one
                j = j+1
                currend = rec_annots$tend[i]
            }
            rec_annots$callID[i] = j
        }
        rec_output = dplyr::group_by(rec_annots, callID) %>%
                dplyr::summarize(tstart=min(tstart),
                                 tend=max(tend),
                                 rec=r
                                 )  # add your own summary functions for any additional info here
                                    # following dplyr::summarize format (vector in - scalar out)

        rec_output$callID = NULL
        output[[length(output)+1]] = rec_output
    }
    output = as.data.frame(dplyr::bind_rows(output))

    return(output)
}


#' Final formatting of call history for ascr
#'
#' Makes some minor formatting changes to allow using a call history in
#' [ascr::create.capt()].
#'
#' @param calls A call history dataframe with columns `id` (arbitrary ID of each
#'   call) and `rec` (recorder ID on which this capture/recapture was
#'   registered). Auxiliary info may be provided in columns `bearing`, `dist`,
#'   `ss`, `toa`. All cues will be assigned to the same session.
#'   If you need a multi-session model, inspect and modify the outputs manually
#'   (see [ascr::fit.ascr()] and [ascr::create.capt()] for format instructions).
#' @param gpspos A dataframe with the recorder locations: columns `rec`, `east`
#'   and `north` with recorder ID, eastings and northings. `rec` is used for
#'   matching with `calls` and creating a new, numeric recorder ID. Coordinates
#'   will be unchanged, so you should prepare them as for [ascr::fit.ascr()]. In
#'   principle any system is suitable; we recommend using [rgdal::spTransform()]
#'   to project the collected GPS data into UTM or similar projections, so that
#'   the inputs are meters easting or northing, for meaningful interpretation of
#'   the results.
#' @param survey.recs Optional: a vector of recorder IDs from `rec` to include.
#'   Defaults to `NULL` = include all.
#'   For example, it allows to conveniently exclude any recorders that did not
#'   work on a particular occasion without editing the location data.
#'
#' @return A named list with `calls` and `traps` elements which can be directly
#'   used in [ascr::create.capt()]. The main difference from the inputs is that
#'   `calls` will have a numeric recorder ID, pointing to rows in `traps`,
#'   and dummy columns for session and occasion, while `traps` will be a matrix
#'   without any recorder ID column.
#' @export
#'
#' @examples
#'
#' # Recorder positions in eastings-northings:
#' gpspos = data.frame(rec=c("rec1", "rec2"),
#'                     east=c(-50, 50),
#'                     north=c(0, 175))
#'
#' # Call history, from annotations:
#' annots =data.frame(rec=c("rec1", "rec1", "rec2", "rec2"),
#'   tstart=c(0.0, 5.5, 1.5, 9), tend=c(5.0, 6.5, 6.0, 13))
#' calls = annots_to_calls(annots)
#' l = prepare_capt(calls, gpspos)
#'
#' # Prepare the actual capt object and run acoustic SCR
#' \dontrun{
#' library(ascr)
#' capt = ascr::create.capt(l$calls, traps=l$traps)
#' mask = ascr::create.mask(traps, buffer=700)
#' cr = ascr::fit.ascr(capt, traps, mask)
#' }
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
        calls = calls[which(calls$rec %in% survey.recs),]

        if(nrow(gpspos)==0 | nrow(calls)==0){
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
        warning(txtmsg("Dropping these recorders for which no positions provided:", not_in_gpspos))
    }
    if(length(not_in_calls)>0){
        warning(txtmsg("No calls detected for these recorders:", not_in_calls))
    }

    # create a numeric index column, in case rec names are characters
    gpspos$recid = 1:nrow(gpspos)
    message(sprintf("Using %d recorders.", nrow(gpspos)))

    # Call history prep:
    calls$occ = 1  # add dummy occasion parameter, ignored in ascr
    calls$session = 1 # add session parameter. Multi-session possible,
    # but requires complicated custom processing of traps into a list

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
        warning(txtmsg("The following columns will be ignored by ascr:",
                      cols_unknown))
    }

    # Trap prep:
    # require input already projected to northings-eastings
    traps = as.matrix(gpspos[,c("east", "north")])

    return(list(calls=calls, traps=traps))
}
