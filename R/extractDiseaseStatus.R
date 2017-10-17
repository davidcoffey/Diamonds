#' Extract disease status
#'
#' Extracts disease status from the Caisis database and returns a data frame.
#'
#' @param connection ODBC connection object returned by dbConnect.
#' @param patients A character vector of patient medical record numbers.  If no
#' limit is desired then set as NULL.
#' @param n Number of records to retrieve.  Use n = -1 to retrieve all records.
#' @return Returns a data frame with patient labs from the Caisis database.
#' @export
#' @import DBI
extractDiseaseStatus <- function(connection, patients = NULL, n = -1) {
    if(is.null(patients)){
        patients <- "LIKE '%'"
    } else {
        patients <- paste("IN ('", paste(patients, collapse = "', '"), "')", sep = "")
    }
    data <- DBI::dbGetQuery(connection, paste("SELECT
                                                CaisisProd.dbo.vDatasetPatients.PtMRN as 'PatientMRN',
                                                CaisisProd.dbo.vDatasetStatus.StatusDisease,
                                                CaisisProd.dbo.vDatasetStatus.StatusDate,
                                                CaisisProd.dbo.vDatasetStatus.Status,
                                                CaisisProd.dbo.vDatasetPatients.PtDeathDate as 'PatientDeathDate',
                                                CaisisProd.dbo.vDatasetPatients.PtDeathType,
                                                CaisisProd.dbo.vDatasetClinicalStages.ClinStageDate,
                                                CaisisProd.dbo.vDatasetClinicalStages.ClinStageSystem,
                                                CaisisProd.dbo.vDatasetClinicalStages.ClinStageS,
                                                CaisisProd.dbo.vDatasetStatus.EnteredTime
                                                FROM CaisisProd.dbo.vDatasetPatients
                                                INNER JOIN CaisisProd.dbo.vDatasetStatus ON CaisisProd.dbo.vDatasetPatients.PatientId = CaisisProd.dbo.vDatasetStatus.PatientId
                                                INNER JOIN CaisisProd.dbo.vDatasetClinicalStages ON CaisisProd.dbo.vDatasetPatients.PatientId = CaisisProd.dbo.vDatasetClinicalStages.PatientId
                                                WHERE CaisisProd.dbo.vDatasetPatients.PtMRN ", patients, sep = ""), n=-1)
    data$PatientMRN = as.factor(data$PatientMRN)
    data$StatusDate = as.Date(data$StatusDate, format = "%Y-%m-%d")
    data$ClinStageDate = as.Date(data$ClinStageDate, format = "%Y-%m-%d")
    data$PatientDeathDate = as.Date(data$PatientDeathDate, format = "%Y-%m-%d")
    data$EnteredTime = as.Date(data$EnteredTime, format = "%Y-%m-%d")
return(data)
}
