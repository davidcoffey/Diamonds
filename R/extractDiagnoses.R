#' Extract diagnoses
#'
#' Extracts patient diagnoses from the Diamonds database and returns a data frame.
#'
#' @param connection ODBC connection object returned by dbConnect.
#' @param demographics Boolean value indicating whether demographic information
#' should be included.
#' @param diagnoses A character vector of ICD9 or ICD10 codes.  If no limit is desired
#' then set as NULL.
#' @param patients A character vector of patient medical record numbers.  If no limit is desired
#' then set as NULL.
#' @param format A character vector indicating the output format.  Options
#' include "raw" or "byDiagnosis".
#' @param n Number of records to retrieve.  Use n = Inf to retrieve all records.
#' @return Returns a data frame with patient diagnoses from the Diamonds database.
#' @export
#' @importFrom reshape melt cast
#' @import stringr odbc
extractDiagnoses <- function(connection, demographics = TRUE, diagnoses = NULL,
                        patients = NULL, format = "raw", n = Inf) {
    if(is.null(diagnoses)){
        diagnoses <- "LIKE '%'"
    } else {
        diagnoses <- paste("IN ('", paste(diagnoses, collapse = "', '"), "')", sep = "")
    }

    if(is.null(patients)){
        patients <- "LIKE '%'"
    } else {
        patients <- paste("IN ('", paste(patients, collapse = "', '"), "')", sep = "")
    }
    if(demographics == TRUE){
        data <- odbc::dbFetch(odbc::dbSendQuery(connection, paste("SELECT DISTINCT
                                                                   PatientMRN,
                                                                   DxCodingMethod,
                                                                   DxCode,
                                                                   DxDescription,
                                                                   CCSLevel1Name,
                                                                   CCSLevel2Name,
                                                                   CCSLevel3Name,
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
                                                                   FROM vDiagnosis
                                                                   INNER JOIN vFactFacilityBilling
                                                                   ON vDiagnosis.DiagnosisKey = vFactFacilityBilling.DiagnosisKey
                                                                   INNER JOIN vPatient
                                                                   ON vFactFacilityBilling.PatientKey = vPatient.PatientKey
                                                                   WHERE vDiagnosis.DxCode ", diagnoses, " AND vPatient.PatientMRN ", patients, sep = "")), n)
        data$PatientMRN = as.factor(data$PatientMRN)
        data$PatientDateOfBirth = as.Date(data$PatientDateOfBirth, format = "%Y-%m-%d")
        data$PatientDeathDate = as.Date(data$PatientDeathDate, format = "%Y-%m-%d")
        data = data[order(data$PatientMRN, data$DxCode),]
    }else{
        data <- odbc::dbFetch(odbc::dbSendQuery(connection, paste("SELECT DISTINCT
                                                                   PatientMRN,
                                                                   DxCodingMethod,
                                                                   DxCode,
                                                                   DxDescription,
                                                                   CCSLevel1Name,
                                                                   CCSLevel2Name,
                                                                   CCSLevel3Name
                                                                   FROM vDiagnosis
                                                                   INNER JOIN vFactFacilityBilling
                                                                   ON vDiagnosis.DiagnosisKey = vFactFacilityBilling.DiagnosisKey
                                                                   INNER JOIN vPatient
                                                                   ON vFactFacilityBilling.PatientKey = vPatient.PatientKey
                                                                   WHERE vDiagnosis.DxCode ", diagnoses, " AND vPatient.PatientMRN ", patients, sep = "")), n)
        data$PatientMRN = as.factor(data$PatientMRN)
        data = data[order(data$PatientMRN, data$DxCode),]
    }
    if(format == "raw") {
        return(data)
    }
    if(format == "byDiagnosis") {
        melt <- reshape::melt(data, id = "PatientMRN", measure.vars = "DxCode")
        cast <- reshape::cast(melt, PatientMRN ~ value, length)

        return(cast)
    }
}
