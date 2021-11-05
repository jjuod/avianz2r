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
    a = rjson::fromJSON(file=file)
    if(length(a)==0){
        # In case AviaNZ creates empty .data somehow.
        warning(txtmsg("No lines found in file", file))
        return(a)
    }

    # TODO adapt to old format as well.
    # Currently assumes "new format" annotations (AviaNZ>=2.0)

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
#' @param dir Directory path containing AviaNZ annotations ("new-style" JSON format, as
#'   defined in AviaNZ documentation). Will be explored recursively. File tree and
#'   naming must follow one of these formats:
#'   * `rec_date_time.wav.data` format. The filename of each `.data` file must contain
#'     exactly three underscore-separated components, which will be parsed into
#'     recorder ID, date and time. These files may be arranged into subdirectories in
#'     anyway -- this arrangement will be ignored, and only the filename parsed.
#'   * `rec/date_time.wav.data` format. The filename of each `.data` file must contain
#'     exactly two underscore-separated components, which will be parsed into date
#'     and time. These files must be arranged into subdirectories. The recorder name
#'     will be determined from the lowest-level subdirectory: so the recorder for
#'     `projectX/site1/day1/rec2/20010101_001234.wav.data` will be identified as `rec2`.
#'     Higher-level directory structure, such as the `site1` here, are not important.
#' @param time.formats All permissible timestamp formats for the files, in [strptime()]
#'   format specification.
#' @param exact Passed to [lubridate::parse_date_time()]. If `FALSE`, timestamps will be
#'   resolved by a more sophisticated process, which might help when a mixture of formats
#'   is present. See [lubridate::parse_date_time()] for details. Use with care, as
#'   timestamps are critical for subsequent handling -- in case of any suspicious outputs,
#'   a safer solution may be to separate the data into subdirectories by format,
#'   and load each separately.
#'
#' @return A dataframe (namely, tibble) with one row per each species label, and
#'   at least 5 columns:
#'   * `tstart` and `tend` (numeric, annotation position in seconds
#'   from the file start)
#'   * `freqmin` and `freqmax` (numeric, frequency bounds in Hz,
#'   or 0, if not specified)
#'   * any further columns from the label, typically `species`,
#'   `certainty` and `filter`
#'   * `ftime` (POSIXct, starting time of the file)
#'   * `rec` (name of the recorder).
#'
#'   Note that an AviaNZ annotation may have multiple labels, corresponding to
#'   different species; these will be parsed into individual rows.
#'
#' @export
#'
#' @examples
#' # Read two annotation files from two recorders, stored in a single folder
#' # and named in rec_date_time format
#' annotdir = system.file("extdata", package="avianz2r", mustWork=TRUE)
#' df <- readAnnots(annotdir)
readAnnots <- function(dir, time.formats=c("%Y%m%d_%H%M%S", "%d%m%y_%H%M%S"), exact=TRUE){
    # Scan for wav or bmp annotations
    print(dir)
    print(list.files(dir))
    filenames = list.files(dir, pattern=".(wav|bmp).data$", recursive=TRUE,
                           ignore.case=T)
    if(length(filenames)==0) stop(paste("no .data files found in directory", dir))

    # Parse file names
    fn_parts = strsplit(basename(filenames), "_")
    # Extracts lowest subdirs
    subdirs = basename(dirname(filenames))

    # TODO currently allowing 3-comp and 2-comp+subdir names.
    # need no-timestamp fallback

    if(all(sapply(fn_parts, length)==3)){
        message("*** Attempting filename parsing in REC_DATE_TIME format ***")
        # Merge the last two parts into a timestamp
        tstamps = sapply(fn_parts, function(x) paste(x[2],
                                    sub(".(wav|bmp).data", "", x[3], ignore.case=T), sep="_"))
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
    } else if(all(sapply(fn_parts, length)==2) &
              all(subdirs!="") & all(subdirs!=".")){
        message("*** Attempting filename parsing in REC/DATE_TIME format ***")
        # Merge the two parts into a timestamp
        tstamps = sapply(fn_parts, function(x) paste(x[1], sub(".(wav|bmp).data", "", x[2]), sep="_"))
        # Attempt to parse timestamps. We pretend they are in POSIXct,
        # to avoid DST gaps which are not accounted by field sensors etc.
        alldts = lubridate::parse_date_time(tstamps, time.formats, exact=exact, tz="UTC")
        # Force error (lubridate only warns on failure)
        if(any(is.na(alldts))){
            print(filenames[which(is.na(alldts))])
            stop("The above filename(s) could not be parsed into timestamps, cannot continue")
        }

        as = loop_over_files(dir, filenames, alldts, subdirs)
    } else {
        print(fn_parts)
        stop(paste("Consistent filename format could not be identifed.\n",
                    "Check if they are formatted correctly."))
    }

    if(length(as)==0){
        warning("no annotations read!")
        return(as)
    }

    as = dplyr::bind_rows(as)

    # Convert timestamp to "absolute" time (pretend-UTC,
    # i.e. recorder clock time registered as UTC).
    as$tstart = as$ftime + as$tstart
    as$tend = as$ftime + as$tend

    # TODO maybe drop ftime afterwards?

    message(sprintf("Loaded %d annotations from %d recorders", nrow(as), length(unique(as$rec))))

    return(as)
}
