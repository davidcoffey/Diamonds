#' Extract surivival information
#'
#' Extracts surival information from the Caisis and Diamonds databases and returns a data frame.
#'
#' @param connection ODBC connection object returned by dbConnect.
#' @param patients A character vector of patient medical record numbers.  If no
#' limit is desired then set as NULL.
#' @param diagnosis Caisis diagnosis from which to extract the date of diagnoisis.  Survival
#' length in months will be computed from this date of diagnosis.
#' @param group Disease group from which to extract the last known alive date.
#' @return Returns a data frame with PatientMRN, date of birth, last known alive date, last lab date,
#' and surival status and death dates from both Caisis and Diamonds databases.
#' From this information, the most recent date ('LastDate') is determined and the survival in
#' months is computed between this date and the date of diagnosis ('SurvivalMonths').
#' The surival status is the must up-to-date status from both Caisis and Diamonds databses.
#' The patient is assumed alive if the status is not dead and the CaisisLastAliveDate or
#' the LastLabDate is within the past 6 months.
#' @export
#' @import DBI
extractSurvival <- function(connection, patients = NULL, diagnosis = "Multiple Myeloma", group = "Plasma Cell Neoplasm"){
    status <- extractDiseaseStatus(connection = connection, patients = patients)
    AliveDate <- status[status$StatusDisease == group & status$Status == "Alive", c("PatientMRN", "StatusDate")]
        names(AliveDate)[2] <- "CaisisLastAliveDate"
    DiagnosisDate <- status[status$StatusDisease == diagnosis & status$Status == "Diagnosis Date", c("PatientMRN", "StatusDate")]
        names(DiagnosisDate)[2] <- "DiagnosisDate"
    CaisisDeathStatus <- unique(status[, c("PatientMRN", "PatientDeathIndicator", "PatientDeathDate", "PtDeathType")])
        names(CaisisDeathStatus)[2:3] <- c("CaisisDeathStatus", "CaisisDeathDate")

    demographics <- extractDemographics(connection = connection, patients = unique(status$PatientMRN))
    DiamondsDeathStatus <- demographics[,c("PatientMRN", "PatientDateOfBirth", "PatientDeathIndicator", "PatientDeathDate")]
        names(DiamondsDeathStatus)[3:4] <- c("DiamondsDeathStatus", "DiamondsDeathDate")

    status.patients <- paste("IN ('", paste(status$PatientMRN, collapse = "', '"), "')", sep = "")
    LastLabDate <- DBI::dbGetQuery(connection, paste("SELECT
                                                      PatientMRN,
                                                      MAX(ObservationDate) AS 'LastLabDate'
                                                      FROM
                                                      FH_clinicalDW.Heme.vExam
                                                      INNER JOIN FH_clinicalDW.Heme.vFactDiagnosticExam ON FH_clinicalDW.Heme.vExam.ExamKey = FH_clinicalDW.Heme.vFactDiagnosticExam.ExamKey
                                                      INNER JOIN FH_clinicalDW.Heme.vPatient ON FH_clinicalDW.Heme.vFactDiagnosticExam.PatientKey = FH_clinicalDW.Heme.vPatient.PatientKey
                                                      INNER JOIN FH_clinicalDW.Heme.vObservationDate ON FH_clinicalDW.Heme.vObservationDate.ObservationDateKey = FH_clinicalDW.Heme.vFactDiagnosticExam.ObservationDateKey
                                                      WHERE FH_clinicalDW.Heme.vPatient.PatientMRN ", status.patients, "GROUP BY(PatientMRN)", sep = ""))

    LastLabDate$LastLabDate <- as.Date(LastLabDate$LastLabDate, format = "%Y-%m-%d")
    survival <- unique(Reduce(function(x, y) merge(x, y, all=TRUE), list(DiagnosisDate, AliveDate, DiamondsDeathStatus, CaisisDeathStatus, LastLabDate)))

    survival$LastDate <- pmax(survival$LastLabDate, survival$CaisisLastAliveDate, survival$CaisisDeathDate, survival$DiamondsDeathDate, na.rm = TRUE)
    survival$Status <- ifelse(survival$DiamondsDeathStatus == "Dead" | survival$CaisisDeathStatus == "Dead", "Dead",
                             ifelse((Sys.Date() - survival$LastLabDate) < 180 | (Sys.Date() - survival$CaisisLastAliveDate) < 180, "Alive", "Unknown"))
    survival$SurvivalMonths <- round((survival$LastDate - survival$DiagnosisDate)/30, digits = 2)
    return(survival)
}
