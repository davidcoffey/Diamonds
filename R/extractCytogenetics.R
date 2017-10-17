#' Extract FISH and cytogenetics reports
#'
#' Extracts FISH and cytogenetics from the Caisis database and returns a data frame.
#'
#' @param connection ODBC connection object returned by dbConnect.
#' @param patients A character vector of patient medical record numbers.  If no
#' limit is desired then set as NULL.
#' @param n Number of records to retrieve.  Use n = -1 to retrieve all records.
#' @return Returns a data frame with FISH and cytogenetics reports from the Caisis database.
#' @export
#' @import DBI
extractCytogenetics <- function(connection, patients = NULL, n = -1) {
    if(is.null(patients)){
        patients <- "LIKE '%'"
    } else {
        patients <- paste("IN ('", paste(patients, collapse = "', '"), "')", sep = "")
    }
    data <- DBI::dbGetQuery(connection, paste("SELECT
                                              CaisisProd.dbo.vDatasetPatients.PtMRN as 'PatientMRN',
                                              CaisisProd.dbo.vDatasetPathology.PathInstitution,
                                              CaisisProd.dbo.vDatasetPathTest.PathDate,
                                              CaisisProd.dbo.vDatasetProcedures.ProcDate,
                                              CaisisProd.dbo.vDatasetPathology.PathNum,
                                              CaisisProd.dbo.vDatasetPathTest.PathMethod,
                                              CaisisProd.dbo.vDatasetPathTest.PathTest,
                                              CaisisProd.dbo.vDatasetPathTest.PathResult,
                                              CaisisProd.dbo.vDatasetPathTest.PathNotes,
                                              CaisisProd.dbo.vDatasetPathTest.PathKaryotype,
                                              CaisisProd.dbo.vDatasetMyelomaPath.PathCytogeneticsAbnormalResults,
                                              CaisisProd.dbo.vDatasetPathTest.EnteredTime
                                              FROM CaisisProd.dbo.vDatasetPatients
                                              FULL JOIN CaisisProd.dbo.vDatasetPathology ON CaisisProd.dbo.vDatasetPatients.PatientId = CaisisProd.dbo.vDatasetPathology.PatientId
                                              INNER JOIN CaisisProd.dbo.vDatasetPathTest ON CaisisProd.dbo.vDatasetPathology.PathologyId = CaisisProd.dbo.vDatasetPathTest.PathologyId
                                              FULL JOIN CaisisProd.dbo.vDatasetProcedures ON CaisisProd.dbo.vDatasetPathology.ProcedureId = CaisisProd.dbo.vDatasetProcedures.ProcedureId
                                              FULL JOIN CaisisProd.dbo.vDatasetMyelomaPath ON CaisisProd.dbo.vDatasetMyelomaPath.PathologyId = CaisisProd.dbo.vDatasetPathology.PathologyId
                                              WHERE CaisisProd.dbo.vDatasetPatients.PtMRN ", patients, sep = ""), n=-1)
    data$PatientMRN = as.factor(data$PatientMRN)
    data$PathDate = as.Date(data$PathDate, format = "%Y-%m-%d")
    data$ProcDate = as.Date(data$ProcDate, format = "%Y-%m-%d")
    data$EnteredTime = as.Date(data$EnteredTime, format = "%Y-%m-%d")
    return(data)
}
