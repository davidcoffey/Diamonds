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
                                               PtMRN as 'PatientMRN',
                                               StatusDisease,
                                               StatusDate,
                                               Status,
                                               PtDeathDate as 'PatientDeathDate',
                                               PtDeathType
                                               FROM CaisisProd.dbo.vDatasetPatients
                                               INNER JOIN CaisisProd.dbo.vDatasetStatus ON CaisisProd.dbo.vDatasetPatients.PatientId = CaisisProd.dbo.vDatasetStatus.PatientId
                                               WHERE CaisisProd.dbo.vDatasetPatients.PtMRN ", patients, sep = ""), n=-1)
    data$PatientMRN = as.factor(data$PatientMRN)
    data$PatientDeathDate = as.Date(data$PatientDeathDate, format = "%Y-%m-%d")
return(data)
}
