#' Extract radiology reports
#'
#' Extracts radiology reports from the Caisis database and returns a data frame.
#'
#' @param connection ODBC connection object returned by dbConnect.
#' @param patients A character vector of patient medical record numbers.  If no
#' limit is desired then set as NULL.
#' @param n Number of records to retrieve.  Use n = -1 to retrieve all records.
#' @return Returns a data frame with radiology reports from the Caisis database.
#' @export
#' @import DBI
extractRadiology <- function(connection, patients = NULL, n = -1) {
    if(is.null(patients)){
        patients <- "LIKE '%'"
    } else {
        patients <- paste("IN ('", paste(patients, collapse = "', '"), "')", sep = "")
    }
    data <- DBI::dbGetQuery(connection, paste("SELECT
                                                PtMRN as 'PatientMRN',
                                                CaisisProd.dbo.vDatasetDiagnostics.DxDataSource,
                                                CaisisProd.dbo.vDatasetDiagnostics.DxType,
                                                CaisisProd.dbo.vDatasetDiagnostics.DxAcc,
                                                CaisisProd.dbo.vDatasetDiagnostics.DxDate,
                                                CaisisProd.dbo.vDatasetDiagnostics.DxIndication,
                                                CaisisProd.dbo.vDatasetDiagnostics.DxNotes
                                                FROM CaisisProd.dbo.vDatasetPatients
                                                INNER JOIN CaisisProd.dbo.vDatasetDiagnostics ON CaisisProd.dbo.vDatasetPatients.PatientId = CaisisProd.dbo.vDatasetDiagnostics.PatientId
                                                WHERE CaisisProd.dbo.vDatasetPatients.PtMRN ", patients, sep = ""), n=-1)
    data$PatientMRN = as.factor(data$PatientMRN)
    data$DxDate = as.Date(data$DxDate, format = "%Y-%m-%d")
    return(data)
}
