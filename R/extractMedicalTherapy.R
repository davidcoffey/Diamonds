#' Extract medical therapy
#'
#' Extracts medical therapy from the Caisis database and returns a data frame.
#'
#' @param connection ODBC connection object returned by dbConnect.
#' @param patients A character vector of patient medical record numbers.  If no
#' limit is desired then set as NULL.
#' @param n Number of records to retrieve.  Use n = -1 to retrieve all records.
#' @return Returns a data frame with medical therapy from the Caisis database.
#' @export
#' @import DBI
extractMedicalTherapy <- function(connection, patients = NULL, n = -1) {
    if(is.null(patients)){
        patients <- "LIKE '%'"
    } else {
        patients <- paste("IN ('", paste(patients, collapse = "', '"), "')", sep = "")
    }
    data <- DBI::dbGetQuery(connection, paste("SELECT
                                               CaisisProd.dbo.vDatasetPatients.PtMRN as 'PatientMRN',
                                               CaisisProd.dbo.vDatasetMedicalTherapy.MedTxDate,
                                               CaisisProd.dbo.vDatasetMedicalTherapy.MedTxType,
                                               CaisisProd.dbo.vDatasetMedicalTherapy.MedTxAgent,
                                               CaisisProd.dbo.vDatasetMedicalTherapy.MedTxCycle,
                                               CaisisProd.dbo.vDatasetMedicalTherapy.MedTxStopDate,
                                               CaisisProd.dbo.vDatasetMedicalTherapy.MedTxConditioning,
                                               CaisisProd.dbo.vDatasetMedicalTherapy.MedTxDonor,
                                               CaisisProd.dbo.vDatasetMedicalTherapy.MedTxTransplantType,
                                               CaisisProd.dbo.vDatasetMedicalTherapy.EnteredTime
                                               FROM CaisisProd.dbo.vDatasetPatients
                                               INNER JOIN CaisisProd.dbo.vDatasetMedicalTherapy ON CaisisProd.dbo.vDatasetPatients.PatientId = CaisisProd.dbo.vDatasetMedicalTherapy.PatientId
                                               WHERE CaisisProd.dbo.vDatasetPatients.PtMRN ", patients, sep = ""), n=-1)
    data$PatientMRN = as.factor(data$PatientMRN)
    data$MedTxDate = as.Date(data$MedTxDate, format = "%Y-%m-%d")
    data$MedTxStopDate = as.Date(data$MedTxStopDate, format = "%Y-%m-%d")
    data$EnteredTime = as.Date(data$EnteredTime, format = "%Y-%m-%d")
    return(data)
}
