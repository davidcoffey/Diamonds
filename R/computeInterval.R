#' Computes statistics between first and last dates
#'
#' Computes the first and last date along with the time between, number, and mean number of days between dates.
#'
#' @param data A data frame with a PatientMRN and date column
#' @param dateColumn A character vector giving the name of the date column
#' @return Returns a data frame with the first and last date along with
#' the time between, number, and mean number of days between dates.
#' @export
#' @importFrom dplyr summarize group_by
computeInterval = function(data, dateColumn = "ObservationDate"){
    group <- dplyr::group_by(unique(data[,c("PatientMRN", dateColumn)]), PatientMRN)
    names(group)[2] <- "dateColumn"
    table <- dplyr::summarize(group,
                              FirstDate = min(dateColumn),
                              LastDate = max(dateColumn),
                              DaysBetween = max(dateColumn) - min(dateColumn),
                              NumberDates = length(dateColumn),
                              MeanDaysBetweenDates = round((max(dateColumn) - min(dateColumn))/length(dateColumn), digits = 0))
    return(table)
}
