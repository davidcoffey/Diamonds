#' Extract diamonds
#'
#' Extracts patient observations from the Diamonds database and returns a
#' data frame.
#'
#' @param channel RODBC connection object returned by odbcConnect.
#' @param demographics Boolean value indicating whether demographic information
#' should be included.
#' @param observations A character vector of observation identifiers. Refer to
#' the University of Washington Lab Medicine website to search of test codes
#' (\url{http://menu.labmed.washington.edu/oltg}).
#' @param patients A character vector of patient medical record numbers.
#' @param format A character vector indicating the output format.  Options
#' include "raw", "byObservationId", "byDaysFromFirstObservation", or
#' "byObservationDate".
#' @return Returns a data frame with patient observations from the Diamonds database.
#' @export
#' @importFrom reshape melt cast

extractDiamonds <- function(channel, demographics = TRUE, observations,
                            patients, format = "raw") {
    observations <- paste(observations, collapse = "', '")
    patients <- paste(patients, collapse = "', '")
    if(demographics == TRUE){
        data = sqlQuery(channel, paste("
           SELECT
           PatientMRN,
           Observation,
           ObservationId,
           ObservationDate,
           ObservationTime,
           DaysFromFirstObservation,
           ObservationValue,
           ObservationValueNumeric,
           Units,
           ReferencesRange,
           PatientDateOfBirth,
           PatientDeathDate,
           PatientDeathIndicator,
           PatientBirthPlace,
           PatientSex,
           PatientRace,
           PatientEthnicGroup,
           PatientCity,
           PatientState,
           PatientZipCode,
           PatientCountryCode,
           PatientLanguage,
           PatientMaritalStatus
           FROM vExam
           INNER JOIN vFactDiagnosticExam
           ON vExam.ExamKey = vFactDiagnosticExam.ExamKey
           INNER JOIN vPatient
           ON vFactDiagnosticExam.PatientKey = vPatient.PatientKey
           INNER JOIN vObservationDate
           ON vObservationDate.ObservationDateKey = vFactDiagnosticExam.ObservationDateKey
           INNER JOIN vObservationTime
           ON vObservationTime.ObservationTimeKey = vFactDiagnosticExam.ObservationTimeKey
           WHERE vFactDiagnosticExam.ObservationId IN ('", observations, "')
           AND vPatient.PatientMRN IN ('", patients, "')", sep = ""))
        data$ObservationTime = gsub(pattern = ":00.0000000", data$ObservationTime, replacement = "")
        data$ObservationDate = as.Date(data$ObservationDate, format = "%Y-%m-%d")
        data$PatientDateOfBirth = as.Date(data$PatientDateOfBirth, format = "%Y-%m-%d")
        data$PatientDeathDate = as.Date(data$PatientDeathDate, format = "%Y-%m-%d")
        data = data[order(data$PatientMRN, data$ObservationDate),]
    }else{
        data = sqlQuery(channel, paste("
           SELECT
           PatientMRN,
           Observation,
           ObservationId,
           ObservationDate,
           ObservationTime,
           DaysFromFirstObservation,
           ObservationValue,
           ObservationValueNumeric,
           Units,
           ReferencesRange
           FROM vExam
           INNER JOIN vFactDiagnosticExam
           ON vExam.ExamKey = vFactDiagnosticExam.ExamKey
           INNER JOIN vPatient
           ON vFactDiagnosticExam.PatientKey = vPatient.PatientKey
           INNER JOIN vObservationDate
           ON vObservationDate.ObservationDateKey = vFactDiagnosticExam.ObservationDateKey
           INNER JOIN vObservationTime
           ON vObservationTime.ObservationTimeKey = vFactDiagnosticExam.ObservationTimeKey
           WHERE vFactDiagnosticExam.ObservationId IN ('", observations, "')
           AND vPatient.PatientMRN IN ('", patients, "')", sep = ""))
        data$ObservationTime = gsub(pattern = ":00.0000000", data$ObservationTime, replacement = "")
        data$ObservationDate = as.Date(data$ObservationDate, format = "%Y-%m-%d")
        data = data[order(data$PatientMRN, data$ObservationDate),]
    }
    if(format == "raw") {
        return(data)
    }
    if(format == "byObservationId") {
        melt <- melt(data, id = c("PatientMRN", "ObservationDate", "ObservationId"),
                     measure.vars = c("ObservationValueNumeric"))
        cast <- cast(melt, PatientMRN + ObservationDate ~ ObservationId, mean)
        return(cast)
    }
    if(format == "byDaysFromFirstObservation") {
        melt <- melt(data, id = c("PatientMRN", "DaysFromFirstObservation", "ObservationId"),
                     measure.vars = c("ObservationValueNumeric"))
        cast <- cast(melt, PatientMRN + ObservationId ~ DaysFromFirstObservation, mean, fill = "")
        return(cast)
    }
    if(format == "byObservationDate") {
        melt <- melt(data, id = c("PatientMRN", "ObservationDate", "ObservationId"),
                     measure.vars = c("ObservationValueNumeric"))
        cast <- cast(melt, PatientMRN + ObservationId ~ ObservationDate, mean, fill = "")
        return(cast)
    }
}
