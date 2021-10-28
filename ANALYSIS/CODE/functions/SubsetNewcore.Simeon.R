# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Project: parrot life expectancy
# Date started: 12-01-2021
# Date last modified: 12-01-2021
# Author: Simeon Smeele
# Description: Function from Fernando where I removed the part that loads the newcore. This such that I can 
# taxize the data before running the function. 
# Arguments: see original function
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>


SubsetNewcore.Simeon = function (taxa, ZIMSdir, byTaxon = "binSpecies", byTaxonLevs = NULL, 
          byInst = NULL, byInstLevs = NULL, exactMatch = FALSE, elimOutliers = FALSE, 
          outlierLev = "99.9", extractToFile = FALSE, fileName = NA, 
          fileFormat = "csv", ...) 
{
  if (!byTaxon %in% colnames(newcore)) {
    stop("Wrong taxon column specified.\n", call. = FALSE)
  }
  if (byTaxon == "binSpecies") {
    byLabel <- "Species"
  }
  else if (byTaxon == "common") {
    byLabel <- "Common name"
  }
  else {
    byLabel <- byTaxon
  }
  if (!is.null(byInst)) {
    LoadRawZIMStables(ZIMSdir = ZIMSdir, taxa = taxa, type = "institution", 
                      silent = TRUE)
    if (!byInst %in% colnames(.institution)) {
      stop("Wrong 'byInst' specification, correct values are:\n 'AnonInstitutionID', 'Continent', 'Country', and 'Associations'.", 
           call. = FALSE)
    }
    if (byInst == "Associations") {
      idInst <- sort(unique(unlist(sapply(byInstLevs, function(asc) {
        idasc <- which(grepl(sprintf("[[:blank:]]{1}%s", 
                                     asc), as.character(.institution$Associations)) | 
                         grepl(sprintf("^%s[[:punct:]]{1}", asc), as.character(.institution$Associations)))
        return(idasc)
      }))))
    }
    else {
      idInst <- sort(unique(unlist(sapply(byInstLevs, function(asc) {
        idasc <- suppressWarnings(grep(asc, as.character(.institution[[byInst]])))
        return(idasc)
      }))))
    }
    if (length(idInst) == 0) {
      warning("No values provided for 'byInstLevs'. All available records will be returned.\n", 
              call. = FALSE)
      insts <- unique(newcore$lastInst)
    }
    else {
      insts <- as.character(.institution$AnonInstitutionID)[idInst]
    }
  }
  else {
    insts <- unique(newcore$lastInst)
  }
  if (is.null(byTaxonLevs)) {
    stop("No values provided for 'byTaxonLevs'.\n", call. = FALSE)
  }
  else if (byTaxonLevs[1] == "All" | is.na(byTaxonLevs[1])) {
    idBy <- which(newcore$lastInst %in% insts)
    allBy <- newcore[[byTaxon]]
    lenBy <- length(unique(allBy[idBy]))
  }
  else {
    lenBy <- length(byTaxonLevs)
    allBy <- newcore[[byTaxon]]
    if (exactMatch) {
      idBy <- which(allBy %in% byTaxonLevs & newcore$lastInst %in% 
                      insts)
    }
    else {
      idByList <- sapply(byTaxonLevs, function(name) {
        which(grepl(name, allBy, useBytes = TRUE) & newcore$lastInst %in% 
                insts)
      })
      idBy <- unlist(idByList)
    }
  }
  if (length(idBy) == 0) {
    stop(sprintf("%s not found in newcore. Verify names in 'byTaxonLevs'.\n", 
                 byLabel, tolower(byLabel)), call. = FALSE)
  }
  else {
    unIdbyTaxonLevs <- unique(allBy[idBy])
    lenUnIdBy <- length(unIdbyTaxonLevs)
    if (length(lenUnIdBy == lenBy)) {
      cat(sprintf("All %s values found.\n", tolower(byLabel)))
    }
    else {
      if (lenUnIdBy > 5) {
        cat(sprintf("Found data for %s species.\n", lenUnIdBy))
      }
      else {
        cat(sprintf("Found data for %s.\n", unIdbyTaxonLevs))
      }
    }
  }
  coresubset <- droplevels(newcore[idBy, ])
  if (elimOutliers) {
    if (outlierLev %in% c("95", "99", "99.9")) {
      idkeep <- which(coresubset[[sprintf("above%s", outlierLev)]] == 
                        0)
      coresubset <- droplevels(coresubset[idkeep, ])
    }
    else {
      warning("Wrong outlierLev arguments, values should be '95', '99', or '99.9'. Outliers were not eliminated.", 
              call. = FALSE)
    }
  }
  if (extractToFile) {
    if (fileFormat == "csv") {
      write.csv(coresubset, file = fileName, ...)
    }
    else if (fileFormat == "txt") {
      write.table(coresubset, file = fileName, ...)
    }
    else if (fileFormat == "csv2") {
      write.csv2(coresubset, file = fileName, ...)
    }
    else {
      warning("File could not be saved. Incorrect fileFormat.\n", 
              "Available values are: 'csv', 'csv2', or 'txt'.\n", 
              call. = FALSE)
    }
  }
  return(coresubset)
}