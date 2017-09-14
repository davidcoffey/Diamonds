#' Extract reserach protocols
#'
#' Extracts reserach protocols from the Caisis database and returns a data frame.
#'
#' @param connection ODBC connection object returned by dbConnect.
#' @param patients A character vector of patient medical record numbers.  If no
#' limit is desired then set as NULL.
#' @param n Number of records to retrieve.  Use n = -1 to retrieve all records.
#' @return Returns a data frame with reserach protocols from the Caisis database.
#' @export
#' @import DBI
extractProtocols <- function(connection, patients = NULL, n = -1) {
    if(is.null(patients)){
        patients <- "LIKE '%'"
    } else {
        patients <- paste("IN ('", paste(patients, collapse = "', '"), "')", sep = "")
    }
    data <- DBI::dbGetQuery(connection, paste("SELECT
                                               PtMRN as 'PatientMRN',
                                               CaisisProd.dbo.vDatasetProtocols.ProtocolNum,
                                               CaisisProd.dbo.vDatasetPatientProtocolStatus.PtProtocolStatusDate,
                                               CaisisProd.dbo.vDatasetPatientProtocolStatus.PtProtocolStatus,
                                               CaisisProd.dbo.vDatasetProtocols.ProtocolInstitution,
                                               CaisisProd.dbo.vDatasetProtocols.ProtocolTitle,
                                               CaisisProd.dbo.vDatasetProtocols.ProtocolNotes,
                                               CaisisProd.dbo.vDatasetProtocols.DiseaseSite,
                                               CaisisProd.dbo.vDatasetProtocols.ProtocolPhase,
                                               CaisisProd.dbo.vDatasetProtocols.ProtocolStatus,
                                               CaisisProd.dbo.vDatasetProtocols.ProtocolPI,
                                               CaisisProd.dbo.vDatasetProtocols.ProtocolOpenDate,
                                               CaisisProd.dbo.vDatasetProtocols.ProtocolClosedDate,
                                               CaisisProd.dbo.vDatasetProtocols.ProtocolNCTID
                                               FROM CaisisProd.dbo.vDatasetPatients
                                               INNER JOIN CaisisProd.dbo.vDatasetPatientProtocols ON CaisisProd.dbo.vDatasetPatients.PatientId = CaisisProd.dbo.vDatasetPatientProtocols.PatientId
                                               INNER JOIN CaisisProd.dbo.vDatasetPatientProtocolStatus ON CaisisProd.dbo.vDatasetPatientProtocols.PatientProtocolId = CaisisProd.dbo.vDatasetPatientProtocolStatus.PatientProtocolId
                                               INNER JOIN CaisisProd.dbo.vDatasetProtocols ON CaisisProd.dbo.vDatasetPatientProtocols.protocolId = CaisisProd.dbo.vDatasetProtocols.protocolId
                                               WHERE CaisisProd.dbo.vDatasetPatients.PtMRN ", patients, sep = ""), n=-1)
    data$PatientMRN = as.factor(data$PatientMRN)
    data$PtProtocolStatusDate = as.Date(data$PtProtocolStatusDate, format = "%Y-%m-%d")
    data$ProtocolOpenDate = as.Date(data$ProtocolOpenDate, format = "%Y-%m-%d")
    data$ProtocolClosedDate = as.Date(data$ProtocolClosedDate, format = "%Y-%m-%d")
    return(data)
}
