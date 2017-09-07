#' Parse FISH karyotype strings
#'
#' Parses FISH karyotype strings into individual clones.
#'
#' @param data A data frame with columns named "PatientMRN", "ReportId",
#' "Source", "ObservationDate", and "Karyotype".
#' @param probes A data frame with columns named "probe" and "chr" that defines
#' the gene name and chromosomal location of the gene (e.g. 17p)
#' @param unmatched A Boolean indicating if unmatched reports should be included in the output.
#' If TRUE, then reports for which no FISH strings are detected or pattern matching cannot be performed are included in the output.
#' @param karyotype A Boolean indicating if the original karyotype string should be included in the output.
#' If TRUE, then the karyotype string is included.
#' @param deduplicate A Boolean indicating if duplicate clones with the same abnormality should be removed.  If TRUE, then duplicate
#' clones are removed and the columns for Gene and Copy number of omited from the output.  Be aware that when this option is set to TRUE, it will increase the computation time.
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
parseKaryotype <- function(data, probes, unmatched = TRUE, karyotype = TRUE, deduplicate = TRUE) {
    # Create regular expression pattern to match strings between brackets and parentheses
    pattern.clone <- "\\([^()]+\\)x[[:digit:]]\\[[^()]{1,9}\\]|\\([^()]+\\)\\[[^()]+]|\\([^()]+\\)x[[:digit:]]\\([^()]+\\)\\[[^()]{1,9}]|\\([^()]+\\)\\([^()]+\\)x[[:digit:]]\\[[^()]{1,9}]|\\([^()]+\\)\\([^()]+\\)\\[[^()]{1,9}]"

    # Extract clones using pattern and convert to a list
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
        pattern.copy.number.one.digit <- paste("\\)x[[:digit:]]", paste("IGH ?con ?", probes$probe[i], " ?x[[:digit:]]", sep = ""), paste0(probes$probe[i],c("\\)x[[:digit:]]", "x[[:digit:]]", " ?con ?IGH ?x[[:digit:]]"), collapse = "|"), sep = "|")
        pattern.copy.number.ranges1 <- paste("\\)x[[:digit:]]~[[:digit:]]", paste("IGH ?con ?", probes$probe[i], " ?x[[:digit:]]~[[:digit:]]", sep = ""), paste0(probes$probe[i],c("\\)x[[:digit:]]~[[:digit:]]", "x[[:digit:]]~[[:digit:]]", " ?con ?IGH ?x[[:digit:]]~[[:digit:]]"), collapse = "|"), sep = "|")
        pattern.copy.number.ranges2 <- paste("\\)x[[:digit:]]-[[:digit:]]", paste("IGH ?con ?", probes$probe[i], " ?x[[:digit:]]-[[:digit:]]", sep = ""), paste0(probes$probe[i],c("\\)x[[:digit:]]-[[:digit:]]", "x[[:digit:]]-[[:digit:]]", " ?con ?IGH ?x[[:digit:]]-[[:digit:]]"), collapse = "|"), sep = "|")
        pattern.copy.number = paste(pattern.copy.number.ranges1, pattern.copy.number.ranges2, pattern.copy.number.one.digit, sep = "|")
        clone$Copy <- unlist(stringr::str_extract(clone$Clone, pattern = regex(pattern.copy.number, ignore_case = TRUE)))
        clone$Copy <- stringr::str_extract(clone$Copy, pattern = regex(pattern = "x[[:digit:]]~[[:digit:]]|x[[:digit:]]-[[:digit:]]|x[[:digit:]]", ignore_case = TRUE))
        clone$Count <- stringr::str_extract(clone$Clone, pattern = "[0-9]{1,4}/[0-9]{1,4}|\\[[0-9]{1,4}\\]")
        clone$Count <- gsub(clone$Count, pattern = "\\[|\\]", replacement = "")
        clone$Frequency <- ifelse(grepl(clone$Count, pattern = "\\/"), sapply(clone$Count, function(x) eval(parse(text=x))), 1)

        # Define copy number abnormalities
        clone$Abnormality <- ifelse(grepl(clone$Copy, pattern = "x0|x1|x1~2|x1-2"), paste(probes$chr[i], "-", sep = ""),
                                    ifelse(clone$Copy == "x2", "None", paste(probes$chr[i], "+", sep = "")))

        # Define translocations
        clone$Abnormality <- ifelse(grepl(clone$Clone, pattern = "con", ignore.case = TRUE) & grepl(clone$Clone, pattern = "CCND1"), "t(11;14)",
                                    ifelse(grepl(clone$Clone, pattern = "con", ignore.case = TRUE) & grepl(clone$Clone, pattern = "FGFR3"), "t(4;14)",
                                           ifelse(grepl(clone$Clone, pattern = "con", ignore.case = TRUE) & grepl(clone$Clone, pattern = "MYC"), "t(8;14)",
                                                  ifelse(grepl(clone$Clone, pattern = "con", ignore.case = TRUE) & grepl(clone$Clone, pattern = "MAF"), "t(14;16)",
                                                         ifelse(grepl(clone$Clone, pattern = "con|sep", ignore.case = TRUE) & grepl(clone$Clone, pattern = "5'IGH|5' IGH"), "IGH rearrangement",
                                                                ifelse(grepl(clone$Clone, pattern = "sep", ignore.case = TRUE) & grepl(clone$Clone, pattern = "MYC"), "MYC rearrangement", clone$Abnormality))))))

        # Merge all clones with metadata
        metadata <- unique(data[,c("PatientMRN", "ReportId", "Source", "ObservationDate")])
        clone <- merge(metadata, clone, all = FALSE)
        clones <- rbind(clones, clone)
    }
    clones <- na.omit(clones)

    # Omit +14q and IGH rearrangements if there is a chr 14 translocation
    reportID <- levels(as.factor(clones$ReportId))
    reports <- data.frame()
    i <- 1
    for(i in 1:length(reportID)){
        report <- clones[clones$ReportId == reportID[i], ]
        report$Abnormality <- ifelse(grepl(report$Abnormality, pattern = "14q32.3\\+|IGH rearrangement") & any(grepl(report$Abnormality, pattern = "t\\(")), "None", report$Abnormality)

        # Merge all reports
        reports <- rbind(report, reports)
    }

    # Collapse duplicates
    if(deduplicate == TRUE) {
        dedup.reports <- data.frame()
        i <- 1
        for(i in 1:length(unique(reports$ReportId))){
            report <- reports[reports$ReportId == unique(reports$ReportId)[i],]
            unique.clones <- data.frame()
            j <- 1
            for(j in 1:length(unique(report$Clone))){
                unique.clone <- report[report$Clone == unique(report$Clone)[j], c("ReportId", "PatientMRN", "Source", "ObservationDate", "Clone", "Count", "Frequency", "Abnormality" )]
                if(nrow(unique.clone) > 1 & all(unique.clone$Abnormality == "None")){
                    unique.clone <- unique(unique.clone)
                }
                if(nrow(unique.clone) > 1 & any(unique.clone$Abnormality == "None")){
                    unique.clone <- unique.clone[unique.clone$Abnormality != "None", ]
                }
                unique.clones <- rbind(unique.clone, unique.clones)
            }
            dedup.reports <- rbind(unique.clones, dedup.reports)
        }
        reports <- dedup.reports
    }

    # Add unmatched reports
    if(unmatched == TRUE){
        unmatched.reports <- data[data$ReportId %in% setdiff(unique(data$ReportId), unique(reports$ReportId)), ]
        unmatched.reports$Clone <- ifelse(grepl(unmatched.reports$Karyotype, pattern = "nuc ish"), "Possible FISH detected, but pattern matching not possible", "FISH string not detected")
        reports <- merge(reports, unmatched.reports[,c("ReportId", "PatientMRN", "Source", "ObservationDate", "Clone")], all = TRUE)
    }

    # Include original karyotype string
    if(karyotype == TRUE){
        data.aggregate <- aggregate(data = data, Karyotype~ReportId, FUN = function(x) paste(unique(x), collapse="; "))
        reports <- merge(reports, data.aggregate[,c("ReportId", "Karyotype")])
    }

    # Sort by MRN, date, and clone
    reports = reports[order(reports$PatientMRN, reports$ObservationDate, reports$Clone), ]
    rownames(reports) = NULL
    return(reports)
}
