#' Shift timestamps for each recorder
#'
#' Applies recorder-level adjustments (lag or lead) to synchronize timestamps across recorders.
#'
#' @param data A dataframe containing at least `tstart` and `tend` columns with
#'   values to be adjusted, and a `rec` with recorder IDs for matching with lags.
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
lag_clocks <- function(data, lags){
    # Format checks:
    if(!is.data.frame(data) | !"rec" %in% colnames(data) |
       !"tstart" %in% colnames(data) | !"tend" %in% colnames(data)){
        stop("Data malformed, must be a dataframe with 'rec', 'tstart' and 'tend' columns.")
    }
    if("lags" %in% colnames(data)){
        stop("Data already contains a 'lag' column. Rename that if it should be kept.")
    }
    if(!is.data.frame(lags) | !"lag" %in% colnames(lags)){
        stop("Lags malformed, must be a dataframe with 'rec' and 'lag' columns.")
    }
    not_in_lags = setdiff(data$rec, lags$rec)
    not_in_data = setdiff(lags$rec, data$rec)
    if(!all(data$rec %in% lags$rec)){
        stop(paste("Clock shifts missing for some recorders:", not_in_lags))
    }
    if(length(not_in_data)>0){
        warning(paste("These recorders were not found in the data:", not_in_data))
    }
    if(anyDuplicated(lags$rec)){
        stop("Only one lag can be provided for each recorder")
    }

    # Actual adjustment:
    data = dplyr::left_join(data, lags, by="rec") %>%
        dplyr::mutate(tstart = tstart+lag, tend = tend+lag) %>%
        dplyr::select(!lag)

    return(data)
}
