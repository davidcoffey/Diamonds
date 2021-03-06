#' Extract pathology reports
#'
#' Extracts pathology reports from the Caisis database and returns a data frame.
#'
#' @param connection ODBC connection object returned by dbConnect.
#' @param patients A character vector of patient medical record numbers.  If no
#' limit is desired then set as NULL.
#' @param n Number of records to retrieve.  Use n = -1 to retrieve all records.
#' @param parsePlasmacells A Boolean indicating whether unabstracted plasma cell
#' precentrages should be abstracted from PathNotes column.
#' @return Returns a data frame with pathology reports from the Caisis database.
#' @details For columns reporting the precentatage of plasma cells, a new column
#' is created with the numeric value only.  Less or greater than symbols are
#' removed and the median is reported for ranges.  N/As are converted to 0.
#' @export
#' @import DBI stringr
#' @importFrom stats median
extractPathology <- function(connection, patients = NULL, parsePlasmacells = FALSE, n = -1) {
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
                                              CaisisProd.dbo.vDatasetMyelomaPath.PathBMAFinaldiagnosis,
                                              CaisisProd.dbo.vDatasetPathology.PathNotes,
                                              CaisisProd.dbo.vDatasetMyelomaPath.PathBMAPlasmacells,
                                              CaisisProd.dbo.vDatasetMyelomaPath.PathBMAPlasmacellsenumerated,
                                              CaisisProd.dbo.vDatasetMyelomaPath.PathBMAAnaplasticorPlasmablasticMorphology,
                                              CaisisProd.dbo.vDatasetMyelomaPath.PathBMALightchainrestrictionbyIHC,
                                              CaisisProd.dbo.vDatasetMyelomaPath.PathSoftTissueTissueType,
                                              CaisisProd.dbo.vDatasetMyelomaPath.PathSoftTissueLightchainrestrictionbyIHC,
                                              CaisisProd.dbo.vDatasetMyelomaPath.pathSoftTissuePlasmaCells,
                                              CaisisProd.dbo.vDatasetMyelomaPath.pathSoftTissueFinalDiagnosis,
                                              CaisisProd.dbo.vDatasetMyelomaPath.PathFlowCytometrySpecimenType,
                                              CaisisProd.dbo.vDatasetMyelomaPath.PathFlowCytometryLightchainrestriction,
                                              CaisisProd.dbo.vDatasetMyelomaPath.PathFlowCytometryAbnormalPlasmaCells,
                                              CaisisProd.dbo.vDatasetMyelomaPath.PathFlowCytometryNormalPlasmaCells,
                                              CaisisProd.dbo.vDatasetMyelomaPath.EnteredTime
                                              FROM CaisisProd.dbo.vDatasetPatients
                                              FULL JOIN CaisisProd.dbo.vDatasetPathology ON CaisisProd.dbo.vDatasetPatients.PatientId = CaisisProd.dbo.vDatasetPathology.PatientId
                                              FULL JOIN CaisisProd.dbo.vDatasetProcedures ON CaisisProd.dbo.vDatasetPathology.ProcedureId = CaisisProd.dbo.vDatasetProcedures.ProcedureId
                                              FULL JOIN CaisisProd.dbo.vDatasetMyelomaPath ON CaisisProd.dbo.vDatasetMyelomaPath.PathologyId = CaisisProd.dbo.vDatasetPathology.PathologyId
                                              WHERE CaisisProd.dbo.vDatasetPatients.PtMRN ", patients, sep = ""), n)
    un.abstracted <- data[is.na(data$EnteredTime),]
    data <- data[!is.na(data$EnteredTime),]
    data$PatientMRN <- as.factor(data$PatientMRN)
    data$PathDate <- as.Date(data$PathDate, format = "%Y-%m-%d")
    data$ProcDate <- as.Date(data$ProcDate, format = "%Y-%m-%d")
    data$EnteredTime <- as.Date(data$EnteredTime, format = "%Y-%m-%d")

    columns <- c("PathBMAPlasmacells", "PathFlowCytometryAbnormalPlasmaCells", "PathFlowCytometryNormalPlasmaCells")
    i <- 1
    for(i in 1:length(columns)){
        # Remove symbols
        columnNumeric <- paste(columns[i], "Numeric", sep = "")
        data[,columnNumeric] <- data[,columns[i]]
        data[,columnNumeric] <- gsub(data[,columnNumeric], pattern = "%|<|>|~|:", replacement = "")

        # Convert N/A to 0
        data[,columnNumeric] <- ifelse(grepl(data[,columnNumeric], pattern = "N|A|O", ignore.case = TRUE), 0, data[,columnNumeric])
        data[,columnNumeric] <- ifelse(is.na(data[,columnNumeric]), 0, data[,columnNumeric])

        # Turn bilateral measurments into range
        data[,columnNumeric] = gsub(data[,columnNumeric], pattern = " Left| Right", replacement = "-")
        data[,columnNumeric] = gsub(data[,columnNumeric], pattern = "Left|Right", replacement = "")

        # Calculate median value for ranges
        data[,columnNumeric] <- ifelse(grepl(data[,columnNumeric], pattern = "-"),
                                       as.numeric(lapply(stringr::str_split(data[,columnNumeric], "-"), function(x) stats::median(as.numeric(x)))),
                                       data[,columnNumeric])
        data[,columnNumeric] <- as.numeric(data[,columnNumeric])
    }

    if(parsePlasmacells == TRUE){
        #un.abstracted$PathBMAPlasmacellsNumeric <- unlist(lapply(list(un.abstracted$PathNotes), FUN = parsePlasmacells))
        parsedPlasmaCells <- suppressWarnings(parsePlasmacellsDataFrame(un.abstracted))
        parsedPlasmaCells <- parsedPlasmaCells[parsedPlasmaCells$boneMarrow == TRUE & !is.na(parsedPlasmaCells$PlasmaCell),]
        names(parsedPlasmaCells)[7] <- "PathBMAPlasmacellsNumeric"
        parsedPlasmaCells$Abstraction <- rep("Parsed", nrow(parsedPlasmaCells))
        un.abstracted <- merge(un.abstracted, parsedPlasmaCells[,c("PathNum", "PatientMRN", "ProcDate", "PathBMAPlasmacellsNumeric", "Abstraction")], all = TRUE)
        data <- merge(data, un.abstracted, all = TRUE)
        data$Abstraction <- ifelse(is.na(data$Abstraction), "Manual", data$Abstraction)
    }
    return(data)
}
