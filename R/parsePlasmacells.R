#' Parse pathology reports for \% plasma cells
#'
#' Parses pathology reports for \% plasma cells
#'
#' @param data A character string.
#' @return Returns a \% plasma cells
#' @details
#' \describe{
#' \item{pattern1}{"\\\\d+\\.*\\\\d*\%plasmacells[^:0-9]"}
#' \item{pattern2}{"plasmacells:\\\\d+\\\\.*\\\\d*\%|plasmacells\\\\d+\\\\.*\\\\d*\%"}
#' \item{pattern3}{".\{1,50\}CD138.\{1,50\}|.\{1,50\}plasma.\{1,50\}|.\{1,50\}abnormal.\{1,50\}"}
#' }
#' @export
#' @importFrom stringr str_extract_all str_extract str_replace_all
parsePlasmacells <- function(data){
    data <- str_replace_all(data, pattern = "[[:space:]]{1,5}", "")
    pattern1 <- "\\d+\\.*\\d*%plasmacells[^:0-9]"
    pattern2 <- "plasmacells:\\d+\\.*\\d*%|plasmacells\\d+\\.*\\d*%"
    pattern3 <- ".{1,50}CD138.{1,50}|.{1,50}plasma.{1,50}|.{1,50}abnormal.{1,50}"
    if(grepl(data, pattern = "bonemarrow", ignore.case = TRUE)){
        if(str_detect(data, pattern = regex(pattern1, ignore_case = TRUE))){

            # Extract % Plasma cells (not Plasma cells: or Plasma cells #)
            plasmacells1 <- str_extract_all(data, pattern = regex(pattern1, ignore_case = TRUE))
            percent <- str_extract_all(plasmacells1, "\\d+\\.*\\d*%")
            percent <- ifelse(identical(percent[[1]], character(0)), NA, max(as.numeric(gsub(unlist(percent), pattern = "%", replacement = ""))))
        }
        if(str_detect(data, pattern = regex(pattern2, ignore_case = TRUE))){

            # Extract Plasma cells: % or Plasma cells %
            plasmacells2 <- str_extract_all(data, pattern = regex(pattern2, ignore_case = TRUE))
            percent <- str_extract_all(plasmacells2, "\\d+\\.*\\d*%")
            percent <- ifelse(identical(percent[[1]], character(0)), NA, max(as.numeric(gsub(unlist(percent), pattern = "%", replacement = ""))))
        }
        if(str_detect(data, pattern = regex(pattern3, ignore_case = TRUE))) {

            # Extract % near CD138 or plasma or abnormal
            plasmacells3 <- str_extract_all(data, pattern = regex(pattern3, ignore_case = TRUE))
            percent <- str_extract_all(unlist(plasmacells3), "\\d+\\.*\\d*%")
            percent <- ifelse(identical(percent[[1]], character(0)), NA, max(as.numeric(gsub(unlist(percent), pattern = "%", replacement = ""))))
       } else {
        percent = NA
       }
    }
    return(percent)
}
