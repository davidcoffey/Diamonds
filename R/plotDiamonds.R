#' Plot diamonds data
#'
#' Plots extracted data from the Diamonds database as a histogram, density plot,
#' blox plot, or line plot.
#'
#' @param data A data frame extracted from the Diamonds database in "raw" format.
#' @param chart A character vector of the chart type.  Available options include
#' "histogram", "density", "box", "line".
#' @param by A character vector indicating whether the plots should be iterated
#' by patient or observation.  Available options include "PatientMRN" and
#' "ObservationId".
#' @return Returns a histogram, density plot, blox plot, or line plot in ggplot
#' sytle.
#' @export
#' @import ggplot2
#' @importFrom RColorBrewer brewer.pal

plotDiamonds <- function(data, chart, by){
    data = data[!is.na(data$ObservationValueNumeric),]
    if(by == "ObservationId"){
        if(chart == "histogram"){
            ncolumns <- length(levels(data$ObservationId))
            ncolumns <- round(ifelse(ncolumns < 5, 4, ncolumns/4), digits = 0)
            ncolors <- length(levels(data$PatientMRN))
            getPalette <- grDevices::colorRampPalette(RColorBrewer::brewer.pal(ifelse(ncolors < 9, ncolors, 9), "Set1"))
            p <- ggplot2::ggplot(data, aes(x = ObservationValueNumeric, fill = PatientMRN)) +
                facet_wrap(~ObservationId, scales = "free", ncol = ncolumns) +
                geom_histogram(bins = 30) +
                theme_minimal() +
                scale_fill_manual(values = getPalette(ncolors)) +
                labs(x = "Observation value", y = "Count")
        }
        if(chart == "density"){
            ncolumns <- length(levels(data$ObservationId))
            ncolumns <- round(ifelse(ncolumns < 5, 4, ncolumns/4), digits = 0)
            ncolors <- length(levels(data$PatientMRN))
            getPalette <- grDevices::colorRampPalette(RColorBrewer::brewer.pal(ifelse(ncolors < 9, ncolors, 9), "Set1"))
            p <- ggplot2::ggplot(data, aes(x = ObservationValueNumeric, fill = PatientMRN)) +
                facet_wrap(~ObservationId, scales = "free", ncol = ncolumns) +
                geom_density(alpha = 0.7) +
                theme_minimal() +
                scale_fill_manual(values = getPalette(ncolors)) +
                labs(x = "Observation value", y = "Density")
        }
        if(chart == "box"){
            ncolumns <- length(levels(data$ObservationId))
            ncolumns <- round(ifelse(ncolumns < 5, 4, ncolumns/4), digits = 0)
            p <- ggplot2::ggplot(data, aes(x = ObservationId, y = ObservationValueNumeric)) +
                facet_wrap(~ObservationId, scales = "free", ncol = ncolumns) +
                geom_boxplot(outlier.size = 0.5) +
                theme_minimal() +
                labs(x = "", y = "Observation value")
        }
        if(chart == "line"){
            ncolumns <- length(levels(data$ObservationId))
            ncolumns <- round(ifelse(ncolumns < 7, 1, ncolumns/6), digits = 0)
            nrows <- length(levels(data$ObservationId))
            ncolors <- length(levels(data$PatientMRN))
            getPalette <- grDevices::colorRampPalette(RColorBrewer::brewer.pal(ifelse(ncolors < 9, ncolors, 9), "Set1"))
            p <- ggplot2::ggplot(data, aes(x = DaysFromFirstObservation,
                                           y = ObservationValueNumeric,
                                           group = PatientMRN,
                                           color = PatientMRN)) +
                facet_wrap(~ObservationId, scales = "free", nrow = nrows, ncol = ncolumns) +
                geom_line() +
                geom_point() +
                theme_minimal() +
                scale_color_manual(values = getPalette(ncolors)) +
                labs(x = "Days from first observation", y = "Observation value", color = "Patient MRN")
        }
    }
    if(by == "PatientMRN"){
        if(chart == "histogram"){
            ncolumns <- length(levels(data$PatientMRN))
            ncolumns <- round(ifelse(ncolumns < 5, 4, ncolumns/4), digits = 0)
            ncolors <- length(levels(data$ObservationId))
            getPalette <- grDevices::colorRampPalette(RColorBrewer::brewer.pal(ifelse(ncolors < 9, ncolors, 9), "Set1"))
            p <- ggplot2::ggplot(data, aes(x = ObservationValueNumeric, fill = ObservationId)) +
                facet_wrap(~PatientMRN, scales = "free", ncol = ncolumns) +
                geom_histogram(bins = 30) +
                scale_fill_manual(values = getPalette(ncolors)) +
                theme_minimal() +
                labs(x = "Observation value", y = "Count")
        }
        if(chart == "density"){
            ncolumns <- length(levels(data$PatientMRN))
            ncolumns <- round(ifelse(ncolumns < 5, 4, ncolumns/4), digits = 0)
            ncolors <- length(levels(data$ObservationId))
            getPalette <- grDevices::colorRampPalette(RColorBrewer::brewer.pal(ifelse(ncolors < 9, ncolors, 9), "Set1"))
            p <- ggplot2::ggplot(data, aes(x = ObservationValueNumeric, fill = ObservationId)) +
                facet_wrap(~PatientMRN, scales = "free", ncol = ncolumns) +
                geom_density(alpha = 0.7) +
                scale_fill_manual(values = getPalette(ncolors)) +
                theme_minimal() +
                labs(x = "Observation value", y = "Density")
        }
        if(chart == "box"){
            ncolumns <- length(levels(data$PatientMRN))
            ncolumns <- round(ifelse(ncolumns < 5, 4, ncolumns/4), digits = 0)
            p <- ggplot2::ggplot(data, aes(x = ObservationId, y = ObservationValueNumeric)) +
                facet_wrap(~PatientMRN, scales = "free", ncol = ncolumns) +
                geom_boxplot(outlier.size = 0.5) +
                theme_minimal() +
                theme(axis.text.x = element_text(angle=90, vjust=0.5, hjust = 1)) +
                labs(x = "", y = "Observation value")
        }
        if(chart == "line"){
            ncolumns <- length(levels(data$PatientMRN))
            ncolumns <- round(ifelse(ncolumns < 7, 1, ncolumns/6), digits = 0)
            nrows <- length(levels(data$PatientMRN))
            ncolors <- length(levels(data$ObservationId))
            getPalette <- grDevices::colorRampPalette(RColorBrewer::brewer.pal(ifelse(ncolors < 9, ncolors, 9), "Set1"))
            p <- ggplot2::ggplot(data, aes(x = DaysFromFirstObservation,
                                           y = ObservationValueNumeric,
                                           group = ObservationId,
                                           color = ObservationId)) +
                facet_wrap(~PatientMRN, scales = "free", nrow = nrows, ncol = ncolumns) +
                geom_line() +
                geom_point() +
                theme_minimal() +
                scale_color_manual(values = getPalette(ncolors)) +
                labs(x = "Days from first observation", y = "Observation value", color = "Observation")
        }
    }
    return(p)
}
