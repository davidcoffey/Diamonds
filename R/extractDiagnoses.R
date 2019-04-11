#' Extract diagnoses
#'
#' Extracts patient diagnoses from the Diamonds database and returns a data frame.
#'
#' @param connection ODBC connection object returned by dbConnect.
#' @param diagnoses A character vector of ICD9 or ICD10 codes.  If no limit is desired
#' then set as NULL. May use \% or _ for partial matches.
#' @param patients A character vector of patient medical record numbers.  If no limit is desired
#' then set as NULL.
#' @param format A character vector indicating the output format.  Options
#' include "raw" or "byDiagnosis".
#' @param n Number of records to retrieve.  Use n = -1 to retrieve all records.
#' @return Returns a data frame with patient diagnoses and the date they were assigned in
#' the Diamonds database.
#' @export
#' @importFrom reshape melt cast
#' @import stringr DBI
extractDiagnoses <- function(connection, diagnoses = NULL, patients = NULL, format = "raw", n = -1) {
    if(is.null(diagnoses)){
        diagnoses <- "LIKE '%'"
    } else {
        diagnoses <- paste("'", paste(diagnoses, collapse = "' OR FH_clinicalDW.Heme.vDiagnosis.DxCode LIKE '"), "'", sep = "")
    }

    if(is.null(patients)){
        patients <- "LIKE '%'"
    } else {
        patients <- paste("IN ('", paste(patients, collapse = "', '"), "')", sep = "")
    }
    data <- DBI::dbGetQuery(connection, paste("SELECT DISTINCT
                                               PatientMRN,
                                               DxCodingMethod,
                                               DxCode,
                                               DxDescription,
                                               CCSLevel1Name,
                                               CCSLevel2Name,
                                               CCSLevel3Name,
                                               ContactDate
                                               FROM FH_clinicalDW.Heme.vDiagnosis
                                               INNER JOIN FH_clinicalDW.Heme.vFactFacilityBilling
                                               ON FH_clinicalDW.Heme.vDiagnosis.DiagnosisKey = FH_clinicalDW.Heme.vFactFacilityBilling.DiagnosisKey
                                               INNER JOIN FH_clinicalDW.Heme.vPatient
                                               ON FH_clinicalDW.Heme.vFactFacilityBilling.PatientKey = FH_clinicalDW.Heme.vPatient.PatientKey
                                               INNER JOIN FH_clinicalDW.Heme.vContactDate
                                               ON FH_clinicalDW.Heme.vFactFacilityBilling.ContactDateKey = FH_clinicalDW.Heme.vContactDate.ContactDateKey
                                               WHERE FH_clinicalDW.Heme.vDiagnosis.DxCode LIKE ", diagnoses, " AND FH_clinicalDW.Heme.vPatient.PatientMRN ", patients, sep = ""), n)
    data$PatientMRN <- as.factor(data$PatientMRN)
    data$ContactDate <- as.Date(data$ContactDate, format = "%Y-%m-%d")
    data = data[order(data$PatientMRN, data$DxCode),]
    if(format == "raw") {
        return(data)
    }
    if(format == "byDiagnosis") {
        melt <- reshape::melt(data, id = "PatientMRN", measure.vars = "DxCode")
        cast <- reshape::cast(melt, PatientMRN ~ value, length)

        return(cast)
    }
}
