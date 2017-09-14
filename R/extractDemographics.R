#' Extract demographics
#'
#' Extracts patient demographics from the Diamonds database and returns a data frame.
#'
#' @param connection ODBC connection object returned by dbConnect.
#' @param patients A character vector of patient medical record numbers.  If no
#' limit is desired then set as NULL.
#' @param n Number of records to retrieve.  Use n = -1 to retrieve all records.
#' @return Returns a data frame with patient demographics from the Diamonds database.
#' @export
#' @import DBI
extractDemographics <- function(connection, patients = NULL, format = "raw", n = -1) {
    if(is.null(patients)){
        patients <- "LIKE '%'"
    } else {
        patients <- paste("IN ('", paste(patients, collapse = "', '"), "')", sep = "")
    }
    data <- DBI::dbGetQuery(connection, paste("SELECT
                                               PatientMRN,
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
                                               FROM FH_clinicalDW.Heme.vPatient
                                               WHERE FH_clinicalDW.Heme.vPatient.PatientMRN ", patients, sep = ""), n)
    data$PatientMRN <- as.factor(data$PatientMRN)
    data$PatientDateOfBirth <- as.Date(data$PatientDateOfBirth, format = "%Y-%m-%d")
    data$PatientDeathDate <- as.Date(data$PatientDeathDate, format = "%Y-%m-%d")
    return(data)
}
