#' Visualize annotation times
#'
#' Plots the provided timestamps for a quick visual overview, separating by
#' species and recorders.
#'
#' @param annots A dataframe containing at least `tstart` and `tend` columns.
#'   The annotations will be shown as line segments covering `[tstart; tend]`.
#'   These columns must be POSIXt timestamps. Columns `rec` and/or `species`, if
#'   present and containing more than one unique value, will be used to spread
#'   out the timestamps along the Y axis. AviaNZ annotations can be read into
#'   this form directly with [readAnnots()] or [readAnnotsFile()].
#' @param days,hours Optional, numeric vectors. If specified, will only plot
#'   annotations starting within this(these) day(s) of the month or hour(s).
#'   Typically, only several hours of data can be usefully shown on this type of
#'   plot, as afterwards calls become too short and dense to distinguish. These
#'   parameters are provided as a quick way to inspect the data in parts.
#' @param colourby Optional, string, name of a column in `annots`. If provided,
#'   the annotation marks will be coloured according to this column's value
#'   (i.e. it is passed as the `col` aesthetic to ggplot). Can also be set to
#'   `"rec"` or `"species"` which would otherwise be plotted on the y axis.
#'
#' @return A ggplot plot object. Such plots can be modified with the `+`
#'   operator; see [ggplot2] documentation for details.
#' @export
#'
#' @examples
#' annotdir = system.file("extdata", package="avianz2r", mustWork=TRUE)
#' df <- readAnnots(annotdir)
#' plot_annots(df, hours=1)
#' # colouring by species:
#' plot_annots(df, colourby="species")
#' # modifying the plot to split off custom time periods as rows:
#' library(ggplot2)
#' library(lubridate)
#' plot_annots(df[df$rec=="recA",]) +
#'   facet_wrap(~minute(tstart), nrow=2, scales="free") +
#'   scale_x_datetime(date_breaks="10 sec", date_minor_breaks="1 sec", date_label="%M.%S")
plot_annots <- function(annots, days=NULL, hours=NULL, colourby=""){
    if(!is.data.frame(annots) | !"tstart" %in% colnames(annots) | !"tend" %in% colnames(annots)){
        stop("Data malformed, must be a dataframe with 'rec', 'tstart' and 'tend' columns.")
    }
    if(!lubridate::is.POSIXt(annots$tstart) | !lubridate::is.POSIXt(annots$tend)){
        stop("Data malformed, timestamps must be in POSIXct or POSIXlt format.")
    }

    if(!is.null(days)){
        annots = annots[which(lubridate::day(annots$tstart) %in% days),]
    }
    if(!is.null(hours)){
        annots = annots[which(lubridate::hour(annots$tstart) %in% hours),]
    }
    if(colourby!="" & !colourby %in% colnames(annots)){
        stop("Column indicated in the colourby argument not found.")
    }

    # Determine Y aesthetic: by species, recorders, or both, depending
    # on what has variety, and what hasn't been set to colour.
    byspecies = byrec = FALSE
    if("species" %in% colnames(annots)){
        if(length(unique(annots$species))>1 & colourby!="species"){
            byspecies=TRUE
        }
    }
    if("rec" %in% colnames(annots)){
        if(length(unique(annots$rec))>1 & colourby!="rec"){
            byrec=TRUE
        }
    }
    if(byspecies & byrec){
        annots$y = paste(annots$rec, annots$species, sep="/")
    } else if(byspecies){
        annots$y = annots$species
    } else if(byrec){
        annots$y = annots$rec
    } else {
        annots$y = "calls"  # dummy, just to have all marks at same y
    }
    annots$y = as.character(annots$y)


    totalsec = diff(range(annots$tstart))
    # Some heuristics to choose appropriate breaks,
    # because ggplot2::scale_*_datetime for some reason only accepts
    # fixed size breaks.
    # Note also that time periods must be in cut.POSIXt format.
    if(totalsec<lubridate::minutes(3)){
        date_breaks="30 sec"
        date_minor_breaks="5 sec"
    } else if(totalsec<lubridate::minutes(10)){
        date_breaks="1 min"
        date_minor_breaks="10 sec"
    } else if(totalsec<lubridate::hours(2)){
        date_breaks="10 min"
        date_minor_breaks="2 min"
    } else if(totalsec<lubridate::hours(12)){
        date_breaks="1 hour"
        date_minor_breaks="10 min"
    } else {
        date_breaks="6 hour"
        date_minor_breaks="1 hour"
    }
    if(colourby==""){
        pstart = ggplot2::ggplot(annots) +
            ggplot2::geom_segment(ggplot2::aes(x=.data$tstart, xend=.data$tend,
                                               y=.data$y, yend=.data$y),
                                  size=5, col="red")
    } else {
        pstart = ggplot2::ggplot(annots) +
            ggplot2::geom_segment(ggplot2::aes(x=.data$tstart, xend=.data$tend,
                                               y=.data$y, yend=.data$y,
                                               col=.data[[colourby]]),
                                  size=5)
    }

    p = pstart +
        ggplot2::scale_x_datetime(date_breaks=date_breaks, date_minor_breaks=date_minor_breaks,
                                  date_labels="%H:%M:%S",    # dates are ignored, but understood to be naturally
                                  expand=ggplot2::expansion(add=10),  # 10 second additive expansion
                                  name=NULL) +
        ggplot2::ylab(NULL) +
        ggplot2::theme_minimal() + ggplot2::theme(panel.grid.major.y = ggplot2::element_blank(),
                                                  strip.background = ggplot2::element_blank(),
                                                  strip.text = ggplot2::element_blank())
    return(p)
}
