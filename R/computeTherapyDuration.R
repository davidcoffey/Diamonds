#' Compute duration per line of therapy
#'
#' Adds duration per line of therapy to medical therapy extracted from the Caisis database.
#'
#' @param connection ODBC connection object returned by dbConnect.
#' @param medicalTherapy A data frame of medical therapy from the Caisis database.
#' @return Returns a data frame with medical therapy from the Caisis database with
#' the addition of line and duration of therapy.  A line of therapy is defined as drugs
#' initiated within 14 days of each other.  The duration is defined as the time in
#' days between two lines of therapy.
#' @export
#' @import DBI
computeTherapyDuration <- function(connection, medicalTherapy) {
    # Sort therapy by patient then by start day
    medicalTherapy <- medicalTherapy[order(medicalTherapy$PatientMRN, medicalTherapy$MedTxDate, decreasing = FALSE), ]

    # Create empty dataframes
    line.duration <- data.frame()

    # Create a vector of patients to index on
    patients <- as.character(unique(medicalTherapy$PatientMRN))

    i <- 1
    for(i in 1:length(patients)) {
        # Determine line of therapy
        therapy.per.pateint <- medicalTherapy[medicalTherapy$PatientMRN == patients[i], ]

        # Recalculate line of therapy by line of therapy (defined as drugs given within 14 days of each)
        k <- 1
        for(k in 1:nrow(therapy.per.pateint)) {
            if(k == 1){
                therapy.per.pateint$DaysFromDxStartByLine[k] <- therapy.per.pateint$MedTxDate[k]
            } else {
                therapy.per.pateint$DaysFromDxStartByLine[k] <- ifelse(therapy.per.pateint$MedTxDate[k] - therapy.per.pateint$MedTxDate[k - 1] < 14,
                                                                       therapy.per.pateint$DaysFromDxStartByLine[k - 1], therapy.per.pateint$MedTxDate[k])
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
    medicalTherapy <- line.duration
return(medicalTherapy)
}
