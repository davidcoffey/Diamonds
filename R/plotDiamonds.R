#' Plot diamonds data
#'
#' Plots a single observation as line plot or box plot.
#'
#' @param data A data frame extracted from Diamonds in the `raw`` format.
#' @param chart A character vector of the chart type.  Available options include
#' "line" or "box".
#' @param observation A character vector of a single ObservationId to be plotted.
#' @param groups A Boolean indicating if the data should be grouped.  If this
#' option is selected, filter the data first using the filterDiamonds function
#' and specify a grouping variable.
#' @param id A Boolean indicating if the patient medical record numbers should
#' be replaced with patient IDs.  If this option is selected, filter the data
#' first using the filterDiamonds function and specify IDs.
#' @param timepoint A Boolean indicating if the the name of the time point should
#' be displayed in place of the nubmer of days since the first time point.  If
#' this option is selected, filter the data first using the filterDiamonds
#' function and time points.
#' @param interactive A Boolean indicating if the plot should interactive.  If
#' TRUE then plot is interactive, if FALSE then the plot is static.
#' @return Returns a line plot or box plot in ggplot sytle.
#' @export
#' @import ggplot2
#' @importFrom plotly ggplotly
#' @importFrom RColorBrewer brewer.pal
#' @importFrom gtools mixedsort
plotDiamonds <- function(data, observation, chart, id = FALSE, timepoint = FALSE, groups = FALSE, interactive = FALSE){
    if(length(observation) > 1){stop("More than one observation specified", call. = FALSE)}
    data = data[!is.na(data$ObservationValueNumeric) & data$ObservationId %in% observation,]
    if(id == TRUE){
        patient <- "PatientId"
    } else {
        patient <- "PatientMRN"
    }
    if(timepoint == TRUE){
        xaxis <- "TimePoint"
        if(!any(names(data) == "TimePoint")){stop("There is no 'TimePoint' column in the specified data", call. = FALSE)}
        data$TimePoint = as.factor(data$TimePoint)
        data$TimePoint = factor(data$TimePoint, levels = gtools::mixedsort(levels(data$TimePoint)))
        xlabel <- ""
    } else {
        if(any(names(data) == "DaysFromFirstTimePoint")){
            if(chart == "line") {
                xaxis <- "DaysFromFirstTimePoint"
                xlabel <- "Days from first time point"
            } else {
                xaxis <- patient
            }

        } else {
            xaxis <- "ObservationDate"
            xlabel <- ""
        }
    }

    if(groups == TRUE){
        if(!any(names(data) == "Groups")){stop("There is no 'Groups' column in the specified data", call. = FALSE)}
        if(chart == "box"){
                p <- ggplot2::ggplot(data, aes_string(x = xaxis, y = "ObservationValueNumeric")) +
                    facet_grid(~Groups) +
                    geom_boxplot(outlier.size = 1, outlier.color = "grey") +
                    #coord_cartesian(ylim = quantile(data$ObservationValueNumeric, c(0.1, 0.9))) +
                    theme_minimal() +
                    theme(axis.text.x = element_text(angle=90, vjust=0.5, hjust = 1)) +
                    labs(x = "", y = observation)
        }
        if(chart == "line"){
            ncolors <- length(levels(data$PatientMRN))
            getPalette <- grDevices::colorRampPalette(RColorBrewer::brewer.pal(ifelse(ncolors < 9 & ncolors > 2, ncolors, 9), "Set1"))
            p <- ggplot2::ggplot(data, aes_string(x = xaxis,
                                           y = "ObservationValueNumeric",
                                           group = patient,
                                           color = patient)) +
                facet_wrap(~Groups) +
                geom_line() +
                geom_point() +
                theme_minimal() +
                theme(axis.text.x = element_text(angle=90, vjust=0.5, hjust = 1)) +
                scale_color_manual(values = getPalette(ncolors)) +
                labs(x = xlabel, y = observation, color = "")
        }
    } else {
        if(chart == "box"){
            p <- ggplot2::ggplot(data, aes_string(x = xaxis, y = "ObservationValueNumeric")) +
                #facet_wrap(~ObservationId) +
                geom_boxplot(outlier.size = 1, outlier.color = "grey") +
                #coord_cartesian(ylim =  quantile(data$ObservationValueNumeric, c(0.1, 0.9))) +
                theme_minimal() +
                theme(axis.text.x = element_text(angle=90, vjust=0.5, hjust = 1)) +
                labs(x = "", y = observation)
        }
        if(chart == "line"){
            ncolors <- length(levels(data$PatientMRN))
            getPalette <- grDevices::colorRampPalette(RColorBrewer::brewer.pal(ifelse(ncolors < 9 & ncolors > 2, ncolors, 9), "Set1"))
            p <- ggplot2::ggplot(data, aes_string(x = xaxis,
                                           y = "ObservationValueNumeric",
                                           group = patient,
                                           color = patient)) +
                #facet_wrap(~ObservationId) +
                geom_line() +
                geom_point() +
                theme_minimal() +
                theme(axis.text.x = element_text(angle=90, vjust=0.5, hjust = 1)) +
                scale_color_manual(values = getPalette(ncolors)) +
                labs(x = xlabel, y = observation, color = "")
        }
    }
    if(interactive == TRUE){
        return(plotly::ggplotly(p, tooltip = c("y", "x", "colour")))
    } else {
        return(p)
    }
}
