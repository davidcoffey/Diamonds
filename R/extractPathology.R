#' Extract pathology reports
#'
#' Extracts pathology reports from the Caisis database and returns a data frame.
#'
#' @param connection ODBC connection object returned by dbConnect.
#' @param patients A character vector of patient medical record numbers.  If no
#' limit is desired then set as NULL.
#' @param n Number of records to retrieve.  Use n = -1 to retrieve all records.
#' @return Returns a data frame with pathology reports from the Caisis database.
#' @export
#' @import DBI
extractPathology <- function(connection, patients = NULL, n = -1) {
    if(is.null(patients)){
        patients <- "LIKE '%'"
    } else {
        patients <- paste("IN ('", paste(patients, collapse = "', '"), "')", sep = "")
    }
    data <- DBI::dbGetQuery(connection, paste("SELECT
                                               PtMRN as 'PatientMRN',
                                               CaisisProd.dbo.vDatasetPathology.PathInstitution,
                                               CaisisProd.dbo.vDatasetPathology.PathDate,
                                               CaisisProd.dbo.vDatasetProcedures.ProcDate,
                                               CaisisProd.dbo.vDatasetProcedures.ProcName,
                                               CaisisProd.dbo.vDatasetPathology.PathNum,
                                               CaisisProd.dbo.vDatasetPathology.Pathologist,
                                               CaisisProd.dbo.vDatasetPathology.PathSpecimenType,
                                               CaisisProd.dbo.vDatasetPathology.PathSite,
                                               CaisisProd.dbo.vDatasetPathology.PathSide,
                                               CaisisProd.dbo.vDatasetPathology.PathHistology,
                                               CaisisProd.dbo.vDatasetPathology.PathNotes
                                               FROM CaisisProd.dbo.vDatasetPatients
                                               INNER JOIN CaisisProd.dbo.vDatasetPathology ON CaisisProd.dbo.vDatasetPatients.PatientId = CaisisProd.dbo.vDatasetPathology.PatientId
                                               INNER JOIN CaisisProd.dbo.vDatasetProcedures ON CaisisProd.dbo.vDatasetPathology.ProcedureId = CaisisProd.dbo.vDatasetProcedures.ProcedureId
                                               WHERE CaisisProd.dbo.vDatasetPatients.PtMRN ", patients, sep = ""), n=-1)
    data$PatientMRN = as.factor(data$PatientMRN)
    data$PathDate = as.Date(data$PathDate, format = "%Y-%m-%d")
    data$ProcDate = as.Date(data$ProcDate, format = "%Y-%m-%d")
    return(data)
}
