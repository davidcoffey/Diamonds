#' Extract medical therapy
#'
#' Extracts disease status from the Caisis database and returns a data frame.
#'
#' @param connection ODBC connection object returned by dbConnect.
#' @param patients A character vector of patient medical record numbers.  If no
#' limit is desired then set as NULL.
#' @param duration A boolean indicating if the duration per line of therapy should
#' be computed.  A line of therapy is defined as drugs initiated within 14 days of each
#' other.  The duration is defined as the time in days between two lines of therapy.
#' If TRUE, then line and duraiton columns are added to the output.
#' @param n Number of records to retrieve.  Use n = -1 to retrieve all records.
#' @return Returns a data frame with medical therapy from the Caisis database.
#' @export
#' @import DBI
extractMedicalTherapy <- function(connection, patients = NULL, n = -1, duration = FALSE) {
    if(is.null(patients)){
        patients.sql <- "LIKE '%'"
    } else {
        patients.sql <- paste("IN ('", paste(patients, collapse = "', '"), "')", sep = "")
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
                                               WHERE CaisisProd.dbo.vDatasetPatients.PtMRN ", patients.sql, sep = ""), n=-1)
    data$PatientMRN = as.factor(data$PatientMRN)
    data$MedTxDate = as.Date(data$MedTxDate, format = "%Y-%m-%d")
    data$MedTxStopDate = as.Date(data$MedTxStopDate, format = "%Y-%m-%d")
    data$EnteredTime = as.Date(data$EnteredTime, format = "%Y-%m-%d")
    if(duration == TRUE){
        # Extract earliest diagnosis date
        survival <- Diamonds::extractSurvival(connection = connection, patients = patients)
        dxDate <- na.omit(survival[,c("PatientMRN", "EarliestDiagnosisDate")])
        data <- merge(data, dxDate)
        data$DaysFromDxStart <- data$MedTxDate - data$EarliestDiagnosisDate

        # Sort therapy by ID then by start day
        data <- data[order(data$PatientMRN, data$DaysFromDxStart, decreasing = FALSE), ]

        # Create empty dataframes
        line.duration <- data.frame()

        # Create a vector of patients to index on
        patients <- as.character(unique(data$PatientMRN))

        i <- 1
        for(i in 1:length(patients)) {
            # Determine line of therapy
            therapy.per.pateint <- data[data$PatientMRN == patients[i], ]

            # Recalculate line of therapy by line of therapy (defined as drugs given within 14 days of each)
            k <- 1
            for(k in 1:nrow(therapy.per.pateint)) {
                if(k == 1){
                    therapy.per.pateint$DaysFromDxStartByLine[k] <- therapy.per.pateint$DaysFromDxStart[k]
                } else {
                    therapy.per.pateint$DaysFromDxStartByLine[k] <- ifelse(therapy.per.pateint$DaysFromDxStart[k] - therapy.per.pateint$DaysFromDxStart[k - 1] < 14,
                                                                          therapy.per.pateint$DaysFromDxStartByLine[k - 1], therapy.per.pateint$DaysFromDxStart[k])
                }
            }
            line <- data.frame(DaysFromDxStartByLine = unique(therapy.per.pateint$DaysFromDxStartByLine),
                              Line = seq(from = 1, to = length(unique(therapy.per.pateint$DaysFromDxStartByLine))))

            # Compute duration of therapy
            line$Duration <- c(line$DaysFromDxStartByLine[-1], NA) - line$DaysFromDxStartByLine
            therapy.per.line <- merge(therapy.per.pateint, line)
            line.duration <- rbind(line.duration, therapy.per.line)
        }
    line.duration$DaysFromDxStartByLine <- NULL
    line.duration$DaysFromDxStart <- NULL
    data <- line.duration
    }
    return(data)
}
