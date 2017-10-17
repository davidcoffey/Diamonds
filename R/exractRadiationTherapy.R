#' Extract radiation therapy
#'
#' Extracts radiation therapy from the Caisis database and returns a data frame.
#'
#' @param connection ODBC connection object returned by dbConnect.
#' @param patients A character vector of patient medical record numbers.  If no
#' limit is desired then set as NULL.
#' @param n Number of records to retrieve.  Use n = -1 to retrieve all records.
#' @return Returns a data frame with radiation therapy from the Caisis database.
#' @export
#' @import DBI
extractRadiationTherapy <- function(connection, patients = NULL, n = -1) {
    if(is.null(patients)){
        patients <- "LIKE '%'"
    } else {
        patients <- paste("IN ('", paste(patients, collapse = "', '"), "')", sep = "")
    }
    data <- DBI::dbGetQuery(connection, paste("SELECT
                                               CaisisProd.dbo.vDatasetPatients.PtMRN as 'PatientMRN',
                                               CaisisProd.dbo.vDatasetRadiationTherapy.RadTxType,
                                               CaisisProd.dbo.vDatasetRadiationTherapy.RadTxDate,
                                               CaisisProd.dbo.vDatasetRadiationTherapy.RadTxStopDate,
                                               CaisisProd.dbo.vDatasetRadiationTherapy.RadTxTarget,
                                               CaisisProd.dbo.vDatasetRadiationTherapy.RadTxTotalDose,
                                               CaisisProd.dbo.vDatasetRadiationTherapy.EnteredTime
                                               FROM CaisisProd.dbo.vDatasetPatients
                                               INNER JOIN CaisisProd.dbo.vDatasetRadiationTherapy ON CaisisProd.dbo.vDatasetPatients.PatientId = CaisisProd.dbo.vDatasetRadiationTherapy.PatientId
                                               WHERE CaisisProd.dbo.vDatasetPatients.PtMRN ", patients, sep = ""), n=-1)
    data$PatientMRN = as.factor(data$PatientMRN)
    data$RadTxDate = as.Date(data$RadTxDate, format = "%Y-%m-%d")
    data$RadTxStopDate = as.Date(data$RadTxStopDate, format = "%Y-%m-%d")
    data$EnteredTime = as.Date(data$EnteredTime, format = "%Y-%m-%d")
    return(data)
}
