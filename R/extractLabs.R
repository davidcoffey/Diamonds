#' Extract labs
#'
#' Extracts patient labs from the Diamonds database and returns a data frame.
#'
#' @param connection ODBC connection object returned by dbGetQuery.
#' @param labs A character vector of lab test identifiers. Refer to
#' the University of Washington Lab Medicine website to search of test codes
#' (\url{http://menu.labmed.washington.edu/oltg}).  If no limit is desired
#' then set as NULL.
#' @param patients A character vector of patient medical record numbers.  If no
#' limit is desired then set as NULL.
#' @param format A character vector indicating the output format.  Options
#' include "raw", "byObservationId", "byDaysFromFirstObservation", or
#' "byObservationDate".
#' @param n Number of records to retrieve.  Use n = -1 to retrieve all records.
#' @return Returns a data frame with patient labs from the Diamonds database.
#' @export
#' @importFrom reshape melt cast
#' @import stringr DBI dplyr
extractLabs <- function(connection, labs = NULL, patients = NULL, format = "raw", n = -1) {
    if(is.null(labs)){
        labs <- "LIKE '%'"
    } else {
        labs <- paste("IN ('", paste(labs, collapse = "', '"), "')", sep = "")
    }

    if(is.null(patients)){
        patients <- "LIKE '%'"
    } else {
        patients <- paste("IN ('", paste(patients, collapse = "', '"), "')", sep = "")
    }
    data <- DBI::dbGetQuery(connection, paste("SELECT
                                               PatientMRN,
                                               Observation,
                                               ObservationId,
                                               ObservationDateDesc,
                                               ObservationMilitaryHour,
                                               ObservationMinute,
                                               DaysFromFirstObservation,
                                               ObservationValue,
                                               ObservationValueNumeric,
                                               AbnormalFlags,
                                               Units,
                                               ReferencesRange
                                               FROM FH_clinicalDW.Heme.vExam
                                               INNER JOIN FH_clinicalDW.Heme.vFactDiagnosticExam
                                               ON FH_clinicalDW.Heme.vExam.ExamKey = FH_clinicalDW.Heme.vFactDiagnosticExam.ExamKey
                                               INNER JOIN FH_clinicalDW.Heme.vPatient
                                               ON FH_clinicalDW.Heme.vFactDiagnosticExam.PatientKey = FH_clinicalDW.Heme.vPatient.PatientKey
                                               INNER JOIN FH_clinicalDW.Heme.vObservationDate
                                               ON FH_clinicalDW.Heme.vObservationDate.ObservationDateKey = FH_clinicalDW.Heme.vFactDiagnosticExam.ObservationDateKey
                                               INNER JOIN FH_clinicalDW.Heme.vObservationTime
                                               ON FH_clinicalDW.Heme.vObservationTime.ObservationTimeKey = FH_clinicalDW.Heme.vFactDiagnosticExam.ObservationTimeKey
                                               WHERE FH_clinicalDW.Heme.vFactDiagnosticExam.ObservationId ", labs, " AND FH_clinicalDW.Heme.vPatient.PatientMRN ", patients, sep = ""), n)
    data$PatientMRN <- as.factor(data$PatientMRN)
    data$ObservationTime <- paste(data$ObservationMilitaryHour, data$ObservationMinute, sep = ":")
    data$ObservationDateDesc <- as.Date(data$ObservationDateDesc, format = "%b %d, %Y")
    data <- dplyr::select(data, -ObservationMilitaryHour, -ObservationMinute)
    data <- dplyr::rename(data, ObservationDate = ObservationDateDesc)
    data <- data[order(data$PatientMRN, data$ObservationDate),]
    data$ObservationValueNumeric <- ifelse(grepl(pattern = "too small|no spike|polyclonal|normal|oligoclonal|no bence jones|not detectable|not seen", data$ObservationValue, ignore.case = TRUE), 0,
                                           ifelse(grepl(pattern ="g/d|g/24|grams per 24 hours", data$ObservationValue, ignore.case = TRUE),
                                                  gsub(pattern = "[[:space:]]|[[:alpha:]]|/|=|24", replacement = "", stringr::str_match(stringr::str_to_lower(data$ObservationValue), ".....g/d|.....g/24|.....grams per 24 hours")),
                                                  ifelse(grepl(pattern ="pg/mL|ng/mL", data$ObservationValue, ignore.case = TRUE),
                                                         gsub(pattern = "[[:space:]]|[[:alpha:]]|/|:|(|)", replacement = "", stringr::str_match(stringr::str_to_lower(data$ObservationValue), ":......")),
                                                         data$ObservationValueNumeric)))
    data$ObservationValueNumeric <- suppressWarnings(as.numeric(data$ObservationValueNumeric))
    if(any(grepl(pattern = "-", data$ReferencesRange))){
        data$ReferencesRange <- ifelse(data$ReferencesRange == "", NA, data$ReferencesRange)
        ReferencesRange <- data.frame(do.call(rbind, strsplit(data$ReferencesRange, '-')))
        ReferencesRange <- suppressWarnings(data.frame(apply(ReferencesRange, 2, function(x) as.numeric(x))))
        names(ReferencesRange) <- c("LowerLimit", "UpperLimit")
        data <- cbind(data, ReferencesRange)
    }
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
