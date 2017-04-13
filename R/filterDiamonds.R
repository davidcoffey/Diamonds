#' Filter diamonds
#'
#' Filters extracted Diamonds data by specified time points.
#'
#' @param data A data frame extracted from the Diamonds database in "raw" format.
#' @param timepoints A data frame with the first column being the patient
#' medical record numbers and the second being the desired dates to filter
#' around (e.g. date of diagnosis or date of treatment).  There may be multiple
#' dates per patient.
#' @param range A character vector specifying the range of dates for each time
#' point to be extracted.  Available options include "on", "before", "after",
#' and  "within".  If "within" is selected a range of values must be provided in
#' the "within" argument.
#' @param within An integer vector of length 2 providing the number of dates
#' before and after each time point to filter.  For example, c(-14, 14) would
#' indicate that all dates less than or greater than 14 days of the specified
#' time point are to be filtered.
#' @param format A character vector indicating the output format.  Options
#' include "raw", "byObservationId", "byDaysFromFirstTimePoint", or
#' "byObservationDate".
#' @return Returns a data frame with patient observations from the Diamonds
#' filtered by the desired time points database.  A new column is added giving
#' the number of days from the first specified time point.
#' @export
#' @importFrom reshape melt cast
filterDiamonds <- function(data, timepoints, range = "within",
                           within = c(-14,14), format = "raw") {
    filtered.all <- data.frame()
    i = 1
    for(i in 1:nrow(timepoints)){
        patient <- as.character(timepoints[i, 1])
        timepoint <- as.Date(timepoints[i, 2])
        min.timepoint <- min(as.Date(timepoints[timepoints$PatientMRN == patient, 2]))
        DaysFromFirstTimePoint <- as.integer(timepoint - min.timepoint)

        if(range == "on"){
            filtered = data[data$PatientMRN == patient & data$ObservationDate == timepoint,]
        }
        if(range == "after"){
            filtered = data[data$PatientMRN == patient & data$ObservationDate >= timepoint,]
        }
        if(range == "before"){
            filtered = data[data$PatientMRN == patient & data$ObservationDate <= timepoint,]
        }
        if(range == "within"){
            filtered = data[data$PatientMRN == patient &
                                (data$ObservationDate >= timepoint + min(within)) &
                                (data$ObservationDate <= timepoint + max(within)),]
        }
        filtered$DaysFromFirstTimePoint <- rep(DaysFromFirstTimePoint, nrow(filtered))
        filtered.all <- rbind(filtered.all, filtered)
    }
    if(format == "raw") {
        return(filtered.all)
    }
    if(format == "byObservationId") {
        melt <- reshape::melt(filtered.all, id = c("PatientMRN", "ObservationDate", "ObservationId"),
                     measure.vars = c("ObservationValueNumeric"))
        cast <- reshape::cast(melt, PatientMRN + ObservationDate ~ ObservationId, mean)
        return(cast)
    }
    if(format == "byDaysFromFirstTimePoint") {
        melt <- reshape::melt(filtered.all, id = c("PatientMRN", "DaysFromFirstTimePoint", "ObservationId"),
                     measure.vars = c("ObservationValueNumeric"))
        cast <- reshape::cast(melt, PatientMRN + ObservationId ~ DaysFromFirstTimePoint, mean, fill = "")
        return(cast)
    }
    if(format == "byObservationDate") {
        melt <- reshape::melt(filtered.all, id = c("PatientMRN", "ObservationDate", "ObservationId"),
                     measure.vars = c("ObservationValueNumeric"))
        cast <- reshape::cast(melt, PatientMRN + ObservationId ~ ObservationDate, mean, fill = "")
        return(cast)
    }
}
