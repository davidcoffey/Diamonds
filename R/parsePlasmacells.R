#' Parse pathology reports for \% plasma cells
#'
#' Parses pathology reports for \% plasma cells
#'
#' @param data A data frame with columns named "PatientMRN", "ReportId",
#' "PathNotes", and "ObservationDate".
#' @return Returns a data frame with extracted \% plasma cells from pathology reports
#' @details
#' \describe{
#' \item{pattern1}{"\\\\d+\\.*\\\\d*\%plasmacells[^:0-9]"}
#' \item{pattern2}{"plasmacells:\\\\d+\\\\.*\\\\d*\%|plasmacells\\\\d+\\\\.*\\\\d*\%"}
#' \item{pattern3}{".\{50\}CD138.\{50\}|.\{50\}plasma.\{50\}|.\{50\}abnormal.\{50\}"}
#' }
#' @export
#' @importFrom stringr str_extract_all str_extract str_replace_all
parsePlasmacells <- function(data){
    pathnotes <- str_replace_all(data$PathNotes, pattern = "[[:space:]]{1,5}", "")
    plasmaCells <- data.frame("ReportId" = data$ReportId)
    i <- 1
    for(i in 1:length(pathnotes)){
        pattern1 <- "\\d+\\.*\\d*%plasmacells[^:0-9]"
        pattern2 <- "plasmacells:\\d+\\.*\\d*%|plasmacells\\d+\\.*\\d*%"
        pattern3 <- ".{50}CD138.{50}|.{50}plasma.{50}|.{50}abnormal.{50}"
        if(str_detect(pathnotes[i], pattern = regex(pattern1, ignore_case = TRUE))){

            # Extract % Plasma cells (not Plasma cells: or Plasma cells #)
            plasmacells1 <- str_extract_all(pathnotes[i], pattern = regex(pattern1, ignore_case = TRUE))
            percent1 <- str_extract_all(plasmacells1, "\\d+\\.*\\d*%")
            percent1 <- ifelse(identical(percent1[[1]], character(0)), NA, max(as.numeric(gsub(unlist(percent1), pattern = "%", replacement = ""))))
            plasmaCells$boneMarrow[i] <- grepl(pathnotes[i], pattern = "bonemarrow", ignore.case = TRUE)
            plasmaCells$pattern[i] <- "Pattern 1"
            plasmaCells$string[i] <- plasmacells1
            plasmaCells$PlasmaCell[i] <- percent1
        } else {
            if(str_detect(pathnotes[i], pattern = regex(pattern2, ignore_case = TRUE))){

                # Extract Plasma cells: % or Plasma cells %
                plasmacells2 <- str_extract_all(pathnotes[i], pattern = regex(pattern2, ignore_case = TRUE))
                percent2 <- str_extract_all(plasmacells2, "\\d+\\.*\\d*%")
                percent2 <- ifelse(identical(percent2[[1]], character(0)), NA, max(as.numeric(gsub(unlist(percent2), pattern = "%", replacement = ""))))
                plasmaCells$boneMarrow[i] <- grepl(pathnotes[i], pattern = "bonemarrow", ignore.case = TRUE)
                plasmaCells$pattern[i] <- "Pattern 2"
                plasmaCells$string[i] <- plasmacells2
                plasmaCells$PlasmaCell[i] <- percent2
            } else {

                # Extract % near CD138 or plasma or abnormal
                plasmacells3 <- str_extract_all(pathnotes[i], pattern = regex(pattern3, ignore_case = TRUE))
                percent3 <- str_extract_all(plasmacells3, "\\d+\\.*\\d*%")
                percent3 <- ifelse(identical(percent3[[1]], character(0)), NA, max(as.numeric(gsub(unlist(percent3), pattern = "%", replacement = ""))))
                plasmaCells$boneMarrow[i] <- grepl(pathnotes[i], pattern = "bonemarrow", ignore.case = TRUE)
                plasmaCells$pattern[i] <- "Pattern 3"
                plasmaCells$string[i] <- plasmacells3
                plasmaCells$PlasmaCell[i] <- percent3
            }
        }
    }
    plasmaCells <- merge(data[,c("PatientMRN", "ReportId", "ObservationDate")], plasmaCells)
    return(plasmaCells)
}
