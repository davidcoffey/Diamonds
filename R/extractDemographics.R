#' Extract demographics
#'
#' Extracts patient demographics from the Diamonds database and returns a data frame.
#'
#' @param connection ODBC connection object returned by dbConnect.
#' @param patients A character vector of patient medical record numbers.  If no
#' limit is desired then set as NULL.
#' @param n Number of records to retrieve.  Use n = -1 to retrieve all records.
#' @return Returns a data frame with patient demographics from the Diamonds database.
#' PatientAge and PatientAgeGroup are added columns calculated from today's date.
#' RacialGroup column is also added which aggregates similar races into a single group.
#' @export
#' @import DBI
extractDemographics <- function(connection, patients = NULL, n = -1) {
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
    data$PatientAge <- ifelse(data$PatientDeathIndicator != "Y" | data$PatientDeathDate == "1753-01-01",
                              as.integer((Sys.Date() - data$PatientDateOfBirth)/365),
                              as.integer((data$PatientDeathDate - data$PatientDateOfBirth)/365))
    data$PatientAgeGroup <- ifelse(data$PatientAge < 20, "< 20",
                                   ifelse(data$PatientAge < 30, "20-29",
                                          ifelse(data$PatientAge < 40, "30-39",
                                                 ifelse(data$PatientAge < 50, "40-49",
                                                        ifelse(data$PatientAge < 60, "50-59",
                                                               ifelse(data$PatientAge < 70, "60-69",
                                                                      ifelse(data$PatientAge < 80, "70-79",
                                                                             ifelse(data$PatientAge < 90, "80-89", "> 90"))))))))
    data$RacialGroup <- ifelse(data$PatientRace %in% c("White","Caucasian"), "White",
                               ifelse(data$PatientRace %in% c("Black or African American", "Black"), "Black",
                                      ifelse(data$PatientRace %in% c("Asian", "Asian Other", "Chinese", "Korean", "Japanese", "Asian Indian", "Cambodian", "Vietnamese"), "Asian",
                                             ifelse(data$PatientRace %in% c("Unknown", "Unavailable or Unknown", "Other, Not Reported or Unknown", "Declined to answer"), "Not reported", "Other"))))
    data$PatientSex <- ifelse(data$PatientSex == "U", "Unknown", data$PatientSex)
    data$PatientDeathIndicator = ifelse(data$PatientDeathIndicator == "Y", "Dead",
                                        ifelse(data$PatientDeathIndicator == "N", "Alive", "Unknown"))
    return(data)
}
