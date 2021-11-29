#' Read one AviaNZ annotation file
#'
#' Reads one AviaNZ format .data file ("new style" JSON) into an R dataframe.
#'
#' @param file Path to the JSON file. See AviaNZ documentation for current
#'   format requirements.
#'
#' @return A dataframe (namely, tibble) with one row per each species label, and
#'   at least 5 columns:
#'   * `tstart` and `tend` (numeric, annotation position in seconds
#'   from the file start)
#'   * `freqmin` and `freqmax` (numeric, frequency bounds in Hz,
#'   or 0, if not specified)
#'   * any further columns from the label, typically `species`,
#'   `certainty` and `filter`.
#'
#'   Note that an AviaNZ annotation may have multiple labels, corresponding to
#'   different species; these will be parsed into individual rows.
#'
#' @export
#'
#' @examples
#' annotfile = system.file("extdata", "recB_230118_023355.wav.data", package="avianz2r", mustWork=TRUE)
#' df <- readAnnotsFile(annotfile)
readAnnotsFile <- function(file){
    # NOTE: assumes "new format" annotations (AviaNZ>=2.0)
    a = rjson::fromJSON(file=file)
    if(length(a)==0){
        # In case AviaNZ creates empty .data somehow.
        warning(txtmsg("No lines found in file", file))
        return(a)
    }

    # Drop metadata:
    meta = a[[1]]
    a = a[-1]

    # This is basically a sanity check to see if the first entry
    # was actually metadata:
    if(is.null(meta[["Operator"]]) | is.null(meta[["Reviewer"]]))
        stop("Annotations appear malformed (operator or reviewer field not found)")

    # TODO might be worth checking this, if duration is needed for something downstream.
    # if(is.null(meta[["Duration"]])) stop("Duration field is required for reading annotations.\n")

    # Convert JSON-style list to data frame.
    # Fields 1-4 become columns, label becomes a list column
    a = data.frame(do.call(rbind, a))
    if(nrow(a)==0) return(a)
    if(ncol(a)!=5) stop("Non-standard data format (could not identify 5 columns)")
    colnames(a) = c("tstart", "tend", "freqmin", "freqmax", "label")
    a$tstart = as.numeric(a$tstart)
    a$tend = as.numeric(a$tend)
    # Some old annotations may be stored in either start-end order
    flipped_t = which(a$tstart>a$tend)
    if(length(flipped_t)>0){
        a[flipped_t, c("tstart", "tend")] = a[flipped_t, c("tend", "tstart")]
    }
    a$freqmin = as.numeric(a$freqmin)
    a$freqmax = as.numeric(a$freqmax)

    # Now, parse the label column:
    # 1. repeat rows if multiple species labels found
    a = tidyr::unnest_longer(a, .data$label)
    # 2. separate out label components (sp. name, certainty, CT...)
    a = tidyr::unnest_wider(a, .data$label)

    return(a)
}

# Core loop for directory reading
loop_over_files <- function(d, filenames, alldts, recnames){
    as = list()
    finfo = data.frame(filenames, alldts, recnames)
    for(fi in 1:nrow(finfo)){
        filename = finfo$filenames[fi]
        message(paste("...reading file", filename))
        a = readAnnotsFile(file.path(d, filename))
        if(length(a)>1){
            a$ftime = finfo$alldts[fi]
            a$rec = finfo$recnames[fi]
            # Could also store the filename:
            # a$fname = filename
            as[[length(as)+1]] = a
        }
    }
    return(as)
}

#' Read AviaNZ annotations
#'
#' Reads one directory of AviaNZ format .data files into an R dataframe.
#'
#' A note on timezones: this function assumes that the times in filenames are in UTC,
#'   even though the recorder clocks are typically set to local time when starting a survey.
#'   However, most of the devices do not adjust for any time offset changes (i.e. DST) during
#'   the survey, so they do not exactly follow any real timezone. If you wish to cast
#'   the timestamps to a particular local time, or incorporate any clock adjustments
#'   made manually during the survey, simply add the appropriate offset to the timestamps.
#'
#' @param dir Directory path containing AviaNZ annotations ("new-style" JSON format, as
#'   defined in AviaNZ documentation). Will be explored recursively. File tree and
#'   naming should follow one of these formats:
#'   * `rec_date_time.wav.data` format. The name of each `.data` file must contain
#'     exactly three underscore-separated components, which will be parsed into
#'     recorder ID, date and time. These files may be arranged into subdirectories in
#'     anyway -- this arrangement will be ignored, and only the filename parsed.
#'   * `rec/date_time.wav.data` format. The name of each `.data` file must contain
#'     exactly two underscore-separated components, which will be parsed into date
#'     and time. These files must be arranged into subdirectories. The recorder name
#'     will be determined from the lowest-level subdirectory: so the recorder for
#'     `projectX/site1/day1/rec2/20010101_001234.wav.data` will be identified as `rec2`.
#'     Higher-level directory structure, such as the `site1` here, is not important.
#'
#'   If neither of these formats match, unparsed filenames and relative annotation times
#'   will be returned.
#' @param time.formats All permissible date-time formats for the filenames, in [strptime()]
#'   specification. Datetimes that cannot be identified with these will cause an error.
#' @param exact Passed to [lubridate::parse_date_time()]. If `FALSE`, file datetimes will be
#'   resolved by a more sophisticated process, which might help when a mixture of formats
#'   is present. See [lubridate::parse_date_time()] for details. Use with care, as
#'   times are critical for subsequent handling -- in case of any suspicious outputs,
#'   a safer solution may be to separate the data into subdirectories by format,
#'   and load each separately.
#' @param ext Extension of the annotation files (regex). Can be used to read
#'   files with alternative names, such as when loading testing results.
#'   Internal use mostly, do not tweak unless you know what you are doing.
#'
#' @return A dataframe (namely, tibble) with one row per each species label, and
#'   at least 6 columns:
#'   * `tstart` and `tend` (POSIXct, annotation timestamp in "UTC", as described above;
#'   or numeric, seconds relative to file start, if file times could not be identified)
#'   * `freqmin` and `freqmax` (numeric, frequency bounds in Hz,
#'   or 0, if not specified)
#'   * any further columns from the label, typically `species`,
#'   `certainty` and `filter`
#'   * `ftime` (POSIXct, starting time of the file) or `fname` (character, name of the
#'   audio file, if timestamps could not be identified)
#'   * `rec` (character, ID of the recorder).
#'
#'   Note that an AviaNZ annotation may have multiple labels on a single timestamp, corresponding to
#'   different species; these will be parsed into individual rows.
#'
#' @export
#'
#' @examples
#' # Read two annotation files from two recorders, stored in a single folder
#' # and named in rec_date_time format
#' annotdir = system.file("extdata", package="avianz2r", mustWork=TRUE)
#' df <- readAnnots(annotdir)
#'
#' # Timestamps will be converted to actual times whenever possible.
#' # Relative times (e.g. from file start) can be recreated with difftime:
#' difftime(df$tstart, df$ftime)
#' difftime(df$tstart, lubridate::ymd_hms("20180123 01:00:00"), units="secs")
#'
#' \dontrun{
#' # If the filenames do not have timestamps, you can obtain a similar output
#' # by attaching the times manually, like this:
#' df <- readAnnots(notimestampdir)
#' recTimes <- data.frame(ftime=lubridate::ymd_hms(c("20210701_053000", "20210701_194500")),
#'                        fname=c("morning.wav", "evening.wav"))
#' df <- dplyr::left_join(df, recTimes, by="fname")
#' df$tstart <- df$tstart + df$ftime
#' df$tend <- df$tend + df$ftime
#' }
readAnnots <- function(dir, time.formats=c("%Y%m%d_%H%M%S", "%d%m%y_%H%M%S"), exact=TRUE,
                       ext=".(wav|bmp).data$"){
    # Scan for wav or bmp annotations
    print(dir)
    print(list.files(dir))
    filenames = list.files(dir, pattern=ext, recursive=TRUE,
                           ignore.case=T)
    if(length(filenames)==0) stop(paste("no .data files found in directory", dir))

    # Parse file names
    fn_parts = strsplit(basename(filenames), "_")
    # Extracts lowest subdirs
    subdirs = basename(dirname(filenames))

    # 3-comp and 2-comp+subdir names can be parsed fully,
    # other formats will default to the fallback case,
    # but will throw error if some parse and some don't:
    found_tstamps = F
    if(all(sapply(fn_parts, length)==3)){
        message("*** Attempting filename parsing in REC_DATE_TIME format ***")
        # Merge the last two parts into a timestamp
        tstamps = sapply(fn_parts, function(x) paste(x[2],
                                    sub(ext, "", x[3], ignore.case=T), sep="_"))
        # Attempt to parse timestamps. We pretend they are in POSIXct,
        # to avoid DST gaps which are not accounted by field sensors etc.
        alldts = lubridate::parse_date_time(tstamps, time.formats, exact=exact, tz="UTC")
        # Force error (lubridate only warns on failure)
        if(any(is.na(alldts))){
            print(filenames[which(is.na(alldts))])
            stop("The above filename(s) could not be parsed into timestamps, cannot continue")
        }

        recnames = sapply(fn_parts, "[", 1)
        as = loop_over_files(dir, filenames, alldts, recnames)
        found_tstamps = T
    } else if(all(sapply(fn_parts, length)==2) &
              all(subdirs!="") & all(subdirs!=".")){
        message("*** Attempting filename parsing in REC/DATE_TIME format ***")
        # Merge the two parts into a timestamp
        tstamps = sapply(fn_parts, function(x) paste(x[1], sub(ext, "", x[2]), sep="_"))
        # Attempt to parse timestamps. We pretend they are in POSIXct,
        # to avoid DST gaps which are not accounted by field sensors etc.
        alldts = lubridate::parse_date_time(tstamps, time.formats, exact=exact, tz="UTC")
        # Force error (lubridate only warns on failure)
        if(any(is.na(alldts))){
            print(filenames[which(is.na(alldts))])
            stop("The above filename(s) could not be parsed into timestamps, cannot continue")
        }

        as = loop_over_files(dir, filenames, alldts, subdirs)
        found_tstamps = T
    # no-timestamp fallback:
    } else {
        warning("No consistent timestamp format could be identified in the filenames.")
        as = loop_over_files(dir, filenames, sub(".data$", "", filenames), NA)
    }

    if(length(as)==0){
        warning("no annotations read!")
        return(as)
    }

    as = dplyr::bind_rows(as)

    if (found_tstamps){
        # Convert timestamp to "absolute" time (pretend-UTC,
        # i.e. recorder clock time registered as UTC).
        as$tstart = as$ftime + as$tstart
        as$tend = as$ftime + as$tend
    } else {
        # no timestamp found, so stored values are just names:
        colnames(as)[colnames(as)=="ftime"] = "fname"
    }

    message(sprintf("Loaded %d annotations from %d recorders", nrow(as), length(unique(as$rec))))

    return(as)
}


#' Read AviaNZ recognizer test results
#'
#' Reads in three sets of annotations produced in AviaNZ recognizer testing.
#'
#' When testing a recognizer in AviaNZ, the automatic detections are stored in
#' .tmpdata files and .tmp2data files (automatic detections before CNN).
#' Comparing these, as well as the original .data files with manual annotations,
#' might be of interest. This function reads in all these sets of annotations
#' into one data frame. It is essentially a wrapper for [avianz2r::readAnnots()]
#' - see there for details.
#'
#' @param dir Directory path containing AviaNZ annotations.
#' @param ... Passed directly to [avianz2r::readAnnots()].
#'
#' @return A dataframe (namely, tibble) with one row per each species label, all
#'   columns as described in [avianz2r::readAnnots()], plus a `detector` column:
#'   "manual", "auto", or "auto_wv" indicating that the row is from the original
#'   annotations, final detections, or detections after the wavelet stage only,
#'   respectively. For filters without a CNN, same rows will be present as
#'   "auto" and "auto_wv".
#'
#' @export
#'
readTestAnnots <- function(dir, ...){
    manual = readAnnots(dir, ext=".(wav|bmp).data$", ...)
    auto_wv = readAnnots(dir, ext=".(wav|bmp).tmp2data$", ...)
    auto = readAnnots(dir, ext=".(wav|bmp).tmpdata$", ...)

    as = dplyr::bind_rows("manual"=manual, "auto_wv"=auto_wv, "auto"=auto, .id="detector")

    return(as)
}
