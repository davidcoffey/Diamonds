function(connection, patients){
    demographics = extractDemographics(connection = connection, patients = patients)
    status = extractDiseaseStatus(connection = connection, patients = patients)
    pathology = extractPathology(connection = connection, patients = patients)
    cytogenetics = extractCytogenetics(connection = connection, patients = patients)
    protocols = extractProtocols(connection = connection, patients = patients)
    radiology = extractRadiology(connection = connection, patients = patients)
    radiationTherapy = extractRadiationTherapy(connection = connection, patients = patients)
    medicalTherapy = extractMedicalTherapy(connection = connection, patients = patients)
    encounters = extractEncounters(connection = connection, patients = patients)
}
