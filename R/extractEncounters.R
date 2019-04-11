#' Extract clinical encounters
#'
#' Extracts clinical encounters from the Caisis database and returns a data frame.
#'
#' @param connection ODBC connection object returned by dbConnect.
#' @param patients A character vector of patient medical record numbers.  If no
#' limit is desired then set as NULL.
#' @param n Number of records to retrieve.  Use n = -1 to retrieve all records.
#' @return Returns a data frame of clinical encounters from the Caisis database.
#' @export
#' @import DBI
extractEncounters <- function(connection, patients = NULL, n = -1) {
    if(is.null(patients)){
        patients <- "LIKE '%'"
    } else {
        patients <- paste("IN ('", paste(patients, collapse = "', '"), "')", sep = "")
    }
    data <- DBI::dbGetQuery(connection, paste("SELECT
                                              CaisisProd.dbo.vDatasetPatients.PtMRN as 'PatientMRN',
                                              CaisisProd.dbo.vDatasetEncounters.EncType,
                                              CaisisProd.dbo.vDatasetEncounters.EncDate,
                                              CaisisProd.dbo.vDatasetEncounters.EncPhysician,
                                              CaisisProd.dbo.vDatasetEncounters.EncECOG_Score,
                                              CaisisProd.dbo.vDatasetEncounters.EnteredTime
                                              FROM CaisisProd.dbo.vDatasetEncounters
                                              INNER JOIN CaisisProd.dbo.vDatasetPatients
                                              ON CaisisProd.dbo.vDatasetPatients.PatientId = CaisisProd.dbo.vDatasetEncounters.PatientId
                                              WHERE CaisisProd.dbo.vDatasetPatients.PtMRN ", patients, sep = ""), n=-1)
    data$PatientMRN = as.factor(data$PatientMRN)
    data$EncDate = as.Date(data$EncDate, format = "%Y-%m-%d")
    data$EnteredTime = as.Date(data$EnteredTime, format = "%Y-%m-%d")
    return(data)
}
