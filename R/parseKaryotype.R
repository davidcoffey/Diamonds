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
#' @export
#' @importFrom plyr ldply
#' @importFrom stringr str_extract_all str_extract
#' @importFrom stats na.omit
parseKaryotype <- function(data, probes) {
    # Create regular expression pattern to matchstrings between parentheses
    pattern.clone <- "\\([^()]+\\)x[0-9]\\[[0-9]{1,4}/[0-9]{1,4}\\]|\\([^()]+\\)X[0-9]\\[[0-9]{1,4}/[0-9]{1,4}\\]|\\([^()]+\\)\\[[0-9]{1,4}/[0-9]{1,4}\\]|\\([^()]+\\)\\[[0-9]{1,4}\\]|\\([^()]+\\)x[0-9]\\[[0-9]{1,4}\\]"

    # Extract clones (FISH results between parentheses) using pattern and save as a list
    clones.list <- stringr::str_extract_all(string = data$Karyotype, pattern = pattern.clone)
    names(clones.list) <- data$ReportId

    # Extract probe specific clones
    merged <- data.frame()
    i <- 1
    for(i in 1:nrow(probes)){
        # Seperate clones into rows
        probe <- plyr::ldply(lapply(clones.list, function(x) grep(x, pattern = probes$probe[i], value = TRUE)), cbind)
        names(probe) <- c("ReportId", "Clone")
        probe$Gene <- rep(probes$probe[i], nrow(probe))

        # Parse copy number and clonal frequency
        pattern.copy.number <- paste("\\)x[[:digit:]]", paste("IGH ?con ?", probes$probe[i], " ?x[[:digit:]]", sep = ""), paste0(probes$probe[i],c("\\)x[[:digit:]]", "x[[:digit:]]", "X[[:digit:]]", " ?con ?IGH ?x[[:digit:]]"), collapse = "|"), sep = "|")
        probe$Copy <- unlist(stringr::str_extract(probe$Clone, pattern = pattern.copy.number))
        probe$Copy <- unlist(stringr::str_extract(probe$Copy, pattern = "x[[:digit:]]|X[[:digit:]]"))
        probe$Count <- stringr::str_extract(probe$Clone, pattern = "[0-9]{1,4}/[0-9]{1,4}|\\[[0-9]{1,4}\\]")
        probe$Count <- gsub(probe$Count, pattern = "\\[|\\]", replacement = "")
        probe$Frequency <- ifelse(grepl(probe$Count, pattern = "\\/"), sapply(probe$Count, function(x) eval(parse(text=x))), 1)

        # Define copy number abnormalities
        probe$Abnormality <- ifelse(probe$Copy %in% c("x0","x1"), paste(probes$chr[i], "-", sep = ""),
                                    ifelse(probe$Copy == "x2", "None", paste(probes$chr[i], "+", sep = "")))

        # Define translocations
        probe$Abnormality <- ifelse(grepl(probe$Clone, pattern = "con", ignore.case = TRUE) & grepl(probe$Clone, pattern = "CCND1"), "t(11;14)",
                                    ifelse(grepl(probe$Clone, pattern = "con", ignore.case = TRUE) & grepl(probe$Clone, pattern = "FGFR3"), "t(4;14)",
                                           ifelse(grepl(probe$Clone, pattern = "con", ignore.case = TRUE) & grepl(probe$Clone, pattern = "MYC"), "t(8;14)",
                                                  ifelse(grepl(probe$Clone, pattern = "con", ignore.case = TRUE) & grepl(probe$Clone, pattern = "MAF"), "t(14;16)",
                                                         ifelse(grepl(probe$Clone, pattern = "con", ignore.case = TRUE) & grepl(probe$Clone, pattern = "5'"), "IGH rearrangement", probe$Abnormality)))))

        # Merge all probes with metadata
        metadata <- data[,c("PatientMRN", "ReportId", "Source", "ObservationDate")]
        merge <- merge(metadata, probe, all = FALSE)
        merged <- rbind(merged, merge)
    }

    # Remove duplicates
    NAs <- merged[is.na(merged$Copy),]
    merged <- na.omit(merged)
    merged <- merged[(!duplicated(merged[,c("ReportId", "Clone", "Abnormality")])),]
    return(merged)
}
