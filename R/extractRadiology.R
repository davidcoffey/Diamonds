#' Extract radiology reports
#'
#' Extracts radiology reports from the Caisis database and returns a data frame.
#'
#' @param connection ODBC connection object returned by dbConnect.
#' @param patients A character vector of patient medical record numbers.  If no
#' limit is desired then set as NULL.
#' @param n Number of records to retrieve.  Use n = -1 to retrieve all records.
#' @return Returns a data frame with radiology reports from the Caisis database.
#' @export
#' @import DBI
extractRadiology <- function(connection, patients = NULL, n = -1) {
    if(is.null(patients)){
        patients <- "LIKE '%'"
    } else {
        patients <- paste("IN ('", paste(patients, collapse = "', '"), "')", sep = "")
    }
    data <- DBI::dbGetQuery(connection, paste("SELECT
                                              PtMRN as 'PatientMRN',
                                              CaisisProd.dbo.vDatasetDiagnostics.DxDataSource,
                                              CaisisProd.dbo.vDatasetDiagnostics.DxType,
                                              CaisisProd.dbo.vDatasetDiagnostics.DxAcc,
                                              CaisisProd.dbo.vDatasetDiagnostics.DxDate,
                                              CaisisProd.dbo.vDatasetDiagnostics.DxIndication,
                                              CaisisProd.dbo.vDatasetDiagnostics.DxNotes,
                                              CaisisProd.dbo.vDatasetDxImageMyeloma.ImgNumofFocalBonyLesions_DD,
                                              CaisisProd.dbo.vDatasetDxImageMyeloma.ImgTotalNumberOfBonyLesions,
                                              CaisisProd.dbo.vDatasetDxImageMyeloma.ImgSiteFocalBonyLesions,
                                              CaisisProd.dbo.vDatasetDxImageMyeloma.ImgMaxSizeofFocalBonyLesions_DD,
                                              CaisisProd.dbo.vDatasetDxImageMyeloma.ImgMaxSizeofFocalBonyLesions,
                                              CaisisProd.dbo.vDatasetDxImageMyeloma.ImgMaxSUVofFocalBonyLesions_DD,
                                              CaisisProd.dbo.vDatasetDxImageMyeloma.ImgMaxSUVofFocalBonyLesions,
                                              CaisisProd.dbo.vDatasetDxImageMyeloma.ImgMaxSizeofEnlargedLymphNodes_DD,
                                              CaisisProd.dbo.vDatasetDxImageMyeloma.ImgMaxSizeofEnlargedLymphNodes,
                                              CaisisProd.dbo.vDatasetDxImageMyeloma.ImgMaxSUVofEnlargedLymphNodes_DD,
                                              CaisisProd.dbo.vDatasetDxImageMyeloma.ImgMaxSUVofEnlargedLymphNodes,
                                              CaisisProd.dbo.vDatasetDxImageMyeloma.ImgSiteofMassLesions,
                                              CaisisProd.dbo.vDatasetDxImageMyeloma.ImgMaxSizeofMassLesions_DD,
                                              CaisisProd.dbo.vDatasetDxImageMyeloma.ImgMaxSizeofMassLesions,
                                              CaisisProd.dbo.vDatasetDxImageMyeloma.ImgMaxSUVofMassLesions_DD,
                                              CaisisProd.dbo.vDatasetDxImageMyeloma.ImgMaxSUVofMassLesions,
                                              CaisisProd.dbo.vDatasetDxImageMyeloma.ImgNumofLyticLesions_DD,
                                              CaisisProd.dbo.vDatasetDxImageMyeloma.ImgTotalNumofLyticLesions,
                                              CaisisProd.dbo.vDatasetDxImageMyeloma.ImgNumofLucentLesions_DD,
                                              CaisisProd.dbo.vDatasetDxImageMyeloma.ImgNumofLucentLesions,
                                              CaisisProd.dbo.vDatasetDxImageMyeloma.ImgSiteofLyticLesions,
                                              CaisisProd.dbo.vDatasetDxImageMyeloma.ImgSiteofLucentLesions,
                                              CaisisProd.dbo.vDatasetDxImageMyeloma.ImgSiteofFractures
                                              FROM CaisisProd.dbo.vDatasetPatients
                                              INNER JOIN CaisisProd.dbo.vDatasetDiagnostics ON CaisisProd.dbo.vDatasetPatients.PatientId = CaisisProd.dbo.vDatasetDiagnostics.PatientId
                                              FULL JOIN CaisisProd.dbo.vDatasetDxImageMyeloma ON CaisisProd.dbo.vDatasetDxImageMyeloma.DiagnosticId = CaisisProd.dbo.vDatasetDiagnostics.DiagnosticId
                                              WHERE CaisisProd.dbo.vDatasetPatients.PtMRN ", patients, sep = ""), n=-1)
    data$PatientMRN = as.factor(data$PatientMRN)
    data$DxDate = as.Date(data$DxDate, format = "%Y-%m-%d")
    return(data)
}
