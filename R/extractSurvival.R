#' Extract surivival information
#'
#' Extracts surival information from the Caisis and Diamonds databases and returns a data frame.
#'
#' @param connection ODBC connection object returned by dbConnect.
#' @param patients A character vector of patient medical record numbers.  If no
#' limit is desired then set as NULL.
#' @param CaisisDiagnosis Caisis diagnosis from which to extract the date of diagnoisis.  Survival
#' length in months will be computed from the earliest date of diagnosis between Caisis and Diamonds.
#' @param DiamondsDiagnosis Diamonds ICD codes from which to extract the date of diagnoisis.  Survival
#' length in months will be computed from the earliest date of diagnosis between Caisis and Diamonds.
#' @param CaisisGroup Caisis disease group from which to extract the last known alive date.
#' @return Returns a data frame with PatientMRN, date of birth, last known alive date, last lab date,
#' and surival status and death dates from both Caisis and Diamonds databases.
#' From this information, the most recent date ('LastDate') is determined and the survival in
#' months is computed between this date and the date of earliest date of diagnosis reported in
#' Caisis and Diamonds ('SurvivalMonths'). The surival status is the must up-to-date status from
#' both Caisis and Diamonds databses. The patient is assumed alive if the status is not dead and the
#' CaisisLastAliveDate or the LastLabDate is within the past 6 months.
#' @export
#' @import DBI
extractSurvival <- function(connection, patients = NULL, CaisisDiagnosis = "Multiple Myeloma", CaisisGroup = "Plasma Cell Neoplasm", DiamondsDiagnosis = c("C90.00", "C90.01", "C90.03", "203.0", "203.00", "203.01", "203.02")){
    DiamondsDiagnoses <- extractDiagnoses(connection = connection, patients = patients, diagnoses = DiamondsDiagnosis, format = "raw")
    DiamondsDiagnosisDate <- aggregate(data = DiamondsDiagnoses, ContactDate~PatientMRN, FUN = min)
    names(DiamondsDiagnosisDate)[2] <- "DiamondsDiagnosisDate"

    status <- extractDiseaseStatus(connection = connection, patients = patients)
    AliveDate <- status[status$StatusDisease == CaisisGroup & status$Status == "Alive", c("PatientMRN", "StatusDate")]
    names(AliveDate)[2] <- "CaisisLastAliveDate"
    CaisisDiagnosisDate <- status[status$StatusDisease == CaisisDiagnosis & status$Status == "Diagnosis Date", c("PatientMRN", "StatusDate")]
    names(CaisisDiagnosisDate)[2] <- "CaisisDiagnosisDate"
    CaisisDeathStatus <- unique(status[, c("PatientMRN", "PatientDeathIndicator", "PatientDeathDate", "PtDeathType")])
    names(CaisisDeathStatus)[2:3] <- c("CaisisDeathStatus", "CaisisDeathDate")

    demographics <- extractDemographics(connection = connection, patients = unique(status$PatientMRN))
    DiamondsDeathStatus <- demographics[,c("PatientMRN", "PatientDateOfBirth", "PatientDeathIndicator", "PatientDeathDate")]
    names(DiamondsDeathStatus)[3:4] <- c("DiamondsDeathStatus", "DiamondsDeathDate")

    status.patients <- paste("IN ('", paste(patients, collapse = "', '"), "')", sep = "")
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
    survival <- unique(Reduce(function(x, y) merge(x, y, all=TRUE), list(DiamondsDiagnosisDate, CaisisDiagnosisDate, AliveDate, DiamondsDeathStatus, CaisisDeathStatus, LastLabDate)))
    survival$EarliestDiagnosisDate <- pmin(survival$CaisisDiagnosisDate, survival$DiamondsDiagnosisDate, na.rm = TRUE)
    survival$LastDate <- pmax(survival$LastLabDate, survival$CaisisLastAliveDate, survival$CaisisDeathDate, survival$DiamondsDeathDate, na.rm = TRUE)
    survival$Status <- ifelse((survival$DiamondsDeathStatus == "Dead" & !(is.na(survival$DiamondsDeathStatus))) | (survival$CaisisDeathStatus == "Dead" & !(is.na(survival$CaisisDeathStatus))), "Dead",
                              ifelse((Sys.Date() - survival$LastLabDate) < 180 | ((Sys.Date() - survival$CaisisLastAliveDate) < 180 & !(is.na(survival$CaisisLastAliveDate))), "Alive", "Unknown"))
    survival$SurvivalMonths <- round((survival$LastDate - survival$EarliestDiagnosisDate)/30, digits = 2)
    return(survival)
}
