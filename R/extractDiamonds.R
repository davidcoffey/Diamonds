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
#' @import stringr
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

    data$ObservationValueNumeric = ifelse(grepl(pattern = "too small|no spike|polyclonal|normal|oligoclonal|no bence jones|not detectable|not seen", data$ObservationValue, ignore.case = TRUE), 0,
                                          ifelse(grepl(pattern ="g/d|g/24|grams per 24 hours", data$ObservationValue, ignore.case = TRUE),
                                                as.numeric(gsub(pattern = "[[:space:]]|[[:alpha:]]|/|=|24", replacement = "", stringr::str_match(stringr::str_to_lower(data$ObservationValue), ".....g/d|.....g/24|.....grams per 24 hours"))),
                                           ifelse(grepl(pattern ="pg/mL|ng/mL", data$ObservationValue, ignore.case = TRUE),
                                                 as.numeric(gsub(pattern = "[[:space:]]|[[:alpha:]]|/|:|(|)", replacement = "", stringr::str_match(stringr::str_matchstr_to_lower(data$ObservationValue), ":......"))),
                                            as.numeric(data$ObservationValueNumeric))))

    if(format == "raw") {
        return(data)
    }
    if(format == "byObservationId") {
        melt <- reshape::melt(data, id = c("PatientMRN", "ObservationDate", "ObservationId"),
                     measure.vars = c("ObservationValueNumeric"))
        cast <- reshape::cast(melt, PatientMRN + ObservationDate ~ ObservationId, mean)
        return(cast)
    }
    if(format == "byDaysFromFirstObservation") {
        melt <- reshape::melt(data, id = c("PatientMRN", "DaysFromFirstObservation", "ObservationId"),
                     measure.vars = c("ObservationValueNumeric"))
        cast <- reshape::cast(melt, PatientMRN + ObservationId ~ DaysFromFirstObservation, mean, fill = "")
        return(cast)
    }
    if(format == "byObservationDate") {
        melt <- reshape::melt(data, id = c("PatientMRN", "ObservationDate", "ObservationId"),
                     measure.vars = c("ObservationValueNumeric"))
        cast <- reshape::cast(melt, PatientMRN + ObservationId ~ ObservationDate, mean, fill = "")
        return(cast)
    }
}
