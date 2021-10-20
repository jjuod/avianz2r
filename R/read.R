# Read one AviaNZ .data file (JSON)
# into a dataframe
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
        warning(paste("No lines found in file", file))
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
    a$freqmin = as.numeric(a$freqmin)
    a$freqmax = as.numeric(a$freqmax)

    # Now, parse the label column:
    # 1. repeat rows if multiple species labels found
    a = tidyr::unnest_longer(a, label)
    # 2. separate out label components (sp. name, certainty, CT...)
    a = tidyr::unnest_wider(a, label)

    # TODO note that each file might create unique optional columns (calltype etc)

    return(a)
}



readAnnots <- function(dir, time.formats=c("%Y%m%d_%H%M%S", "%d%m%y_%H%M%S"), exact=TRUE){
    # Scan for wav or bmp annotations
    filenames = list.files(dir, pattern=".(wav|bmp).data$", recursive=TRUE)
    if(length(filenames)==0) stop(paste("no .data files found in directory", dir))

    as = list()

    # Parse file names
    fn_parts = strsplit(basename(filenames), "_")

    if(all(sapply(fn_parts, length)==3)){
        message("Attempting filename parsing in REC_DATE_TIME format")
        # Merge the last two parts into a timestamp
        tstamps = sapply(fn_parts, function(x) paste(x[2], sub(".(wav|bmp).data", "", x[3]), sep="_"))
        # Attempt to parse timestamps. We pretend they are in POSIXct,
        # to avoid DST gaps which are not accounted by field sensors etc.
        alldts = lubridate::parse_date_time(tstamps, time.formats, exact=exact, tz="UTC")
        # Force error (lubridate only warns on failure)
        if(any(is.na(alldts))){
            print(filenames[which(is.na(alldts))])
            stop("The above filename(s) could not be parsed into timestamps, cannot continue")
        }

        recnames = sapply(fn_parts, "[", 1)
        finfo = data.frame(filenames, alldts, recnames)
        for(fi in 1:nrow(finfo)){
            filename = finfo$filenames[fi]
            message(paste("reading file", filename))
            a = readAnnotsFile(file.path(dir, filename))
            if(length(a)>1){
                a$time = finfo$alldts[fi]
                a$rec = finfo$recnames[fi]
                # Could also store the filename:
                # a$fname = filename
                as = c(as, a)
            }
        }
        as = dplyr::bind_rows(as)

    } else {
        print(fn_parts)
        stop(paste("Not all filenames have 3 underscore-separated components.",
                   "Check if they are formatted correctly."))
    }


    # TODO currently assuming perfect names
    if(nrow(as)==0) warning("no annotations read!")


    # actual start and end of call
    annot$start = annot$time + seconds(annot$X1)
    annot$end = annot$time + seconds(annot$X2)

    annot$calllength = annot$end - annot$start

    return(as)
}
