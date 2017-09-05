#' Parse FISH karyotype strings
#'
#' Parses FISH karyotype strings into individual clones.
#'
#' @param data A data frame with columns named "PatientMRN", "ReportId",
#' "Source", "ObservationDate", and "Karyotype".
#' @param probes A data frame with columns named "probe" and "chr" that defines
#' the gene name and chromosomal location of the gene (e.g. 17p)
#' @return Returns a data frame where each row represents a clone from the
#' FISH karyotype string.
#' @details The five patterns below are recognized.  Pattern matching is case insensative.  Matched patterns are exluded if they do not contain a known probe.
#' \describe{
#' \item{pattern1}{(...)x#[...]}
#' \item{pattern2}{(...)[...]}
#' \item{pattern3}{(...)x#(...)[...]}
#' \item{pattern4}{(...)(...)x#[...]}
#' \item{pattern5}{(...)(...)[...]}
#' }
#' @export
#' @importFrom plyr ldply
#' @importFrom stringr str_extract_all str_extract
#' @importFrom stats na.omit
parseKaryotype <- function(data, probes) {
    # Create regular expression pattern to matchstrings between parentheses
    pattern.clone <- "\\([^()]+\\)x[0-9]\\[[^()]{1,9}\\]|\\([^()]+\\)\\[[^()]+]|\\([^()]+\\)x[0-9]\\([^()]+\\)\\[[^()]{1,9}]|\\([^()]+\\)\\([^()]+\\)x[0-9]\\[[^()]{1,9}]|\\([^()]+\\)\\([^()]+\\)\\[[^()]{1,9}]"

    # Extract clones (FISH results between parentheses) using pattern and save as a list
    clones.list <- stringr::str_extract_all(string = data$Karyotype, pattern = regex(pattern.clone, ignore_case = TRUE))
    names(clones.list) <- data$ReportId

    # Extract probe specific clones
    clones <- data.frame()
    i <- 1
    for(i in 1:nrow(probes)){
        # Seperate clones into rows
        clone <- plyr::ldply(lapply(clones.list, function(x) grep(x, pattern = probes$probe[i], value = TRUE)), cbind)
        names(clone) <- c("ReportId", "Clone")
        clone$Gene <- rep(probes$probe[i], nrow(clone))

        # Parse copy number and clonal frequency
        pattern.copy.number <- paste("\\)x[[:digit:]]", paste("IGH ?con ?", probes$probe[i], " ?x[[:digit:]]", sep = ""), paste0(probes$probe[i],c("\\)x[[:digit:]]", "x[[:digit:]]", "X[[:digit:]]", " ?con ?IGH ?x[[:digit:]]"), collapse = "|"), sep = "|")
        clone$Copy <- unlist(stringr::str_extract(clone$Clone, pattern = pattern.copy.number))
        clone$Copy <- unlist(stringr::str_extract(clone$Copy, pattern = "x[[:digit:]]|X[[:digit:]]"))
        clone$Count <- stringr::str_extract(clone$Clone, pattern = "[0-9]{1,4}/[0-9]{1,4}|\\[[0-9]{1,4}\\]")
        clone$Count <- gsub(clone$Count, pattern = "\\[|\\]", replacement = "")
        clone$Frequency <- ifelse(grepl(clone$Count, pattern = "\\/"), sapply(clone$Count, function(x) eval(parse(text=x))), 1)

        # Define copy number abnormalities
        clone$Abnormality <- ifelse(clone$Copy %in% c("x0","x1"), paste(probes$chr[i], "-", sep = ""),
                                    ifelse(clone$Copy == "x2", "None", paste(probes$chr[i], "+", sep = "")))

        # Define translocations
        clone$Abnormality <- ifelse(grepl(clone$Clone, pattern = "con", ignore.case = TRUE) & grepl(clone$Clone, pattern = "CCND1"), "t(11;14)",
                                    ifelse(grepl(clone$Clone, pattern = "con", ignore.case = TRUE) & grepl(clone$Clone, pattern = "FGFR3"), "t(4;14)",
                                           ifelse(grepl(clone$Clone, pattern = "con", ignore.case = TRUE) & grepl(clone$Clone, pattern = "MYC"), "t(8;14)",
                                                  ifelse(grepl(clone$Clone, pattern = "con", ignore.case = TRUE) & grepl(clone$Clone, pattern = "MAF"), "t(14;16)",
                                                         ifelse(grepl(clone$Clone, pattern = "con|sep", ignore.case = TRUE) & grepl(clone$Clone, pattern = "5'IGH|5' IGH"), "IGH rearrangement",
                                                                ifelse(grepl(clone$Clone, pattern = "sep", ignore.case = TRUE) & grepl(clone$Clone, pattern = "MYC"), "MYC rearrangement", clone$Abnormality))))))

        # Merge all clones with metadata
        metadata <- data[,c("PatientMRN", "ReportId", "Source", "ObservationDate")]
        clone <- merge(metadata, clone, all = FALSE)
        clones <- rbind(clones, clone)
    }

    # Remove +14q and IGH rearreangment if there is a chr 14 translocation
    reportID = levels(as.factor(clones$ReportId))
    reports <- data.frame()
    i <- 1
    for(i in 1:length(reportID)){
        report <- clones[clones$ReportId == reportID[i], ]
        report$Abnormality = ifelse(grepl(report$Abnormality, pattern = "14q\\+") & any(grepl(report$Abnormality, pattern = "t")), "None", report$Abnormality)

        # Merge all reports
        reports = rbind(report, reports)
    }

    # Remove duplicates
    NAs <- reports[is.na(reports$Copy),]
    reports <- na.omit(reports)
    reports <- reports[(!duplicated(reports[,c("ReportId", "Clone", "Abnormality")])),]
    return(reports)
}
