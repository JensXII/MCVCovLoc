#' Check, combine and prepare input data.
#'
#' @param wdir Working directory.
#' @param StatusDate Date of status calculation.
#' @param IndividualPopulationFile Individual input ;-separated population file
#' @param IndividualMCVFile Individual input ;-separated vaccination file
#' @param AggregatedPopulationFile Aggregated input ;-separated population file
#' @param AggregatedMCVFile Aggregated input ;-separated vaccination file
#' @param AdministrativeMCVFile Administrative input ;-separated vaccination file
#' @import data.table
#' @return Create a subdirectory in \code{wdir}
#' @export
InputData <- function(wdir, StatusDate,
                      IndividualPopulationFile = NA,
                      IndividualMCVFile = NA,
                      AggregatedPopulationFile = NA,
                      AggregatedMCVFile = NA,
                      AdministrativeMCVFile = NA) {

  # wdir = 'S:/Data/MCVCovLoc_wdir'
  # StatusDate = '2020-02-01'
  # IndividualPopulationFile = "DKIndPop20200201.csv"
  # IndividualMCVFile = "DKIndVac20200201.csv"
  # AggregatedPopulationFile = "DKAggPop20200201.csv"
  # AggregatedMCVFile = "DKAggVac20200201.csv"
  # AdministrativeMCVFile = NA

  nuts <- birthyear <- birthdate <- vacdate <- doserecorded <- . <- count <- NULL

  if (!is.na(IndividualPopulationFile) & !is.na(IndividualMCVFile)) {
    IndPop <- IndPopData(wdir, StatusDate, IndividualPopulationFile)
    AggPop <- data.table::setDT(IndPop)[, .(N = .N), keyby = .(nuts, birthyear)]
    IndVac <- IndVacData(wdir, StatusDate, IndividualMCVFile)
    IndVac <- merge(IndVac, IndPop[, c('personid', 'nuts')], by = 'personid', all = FALSE)
    AggVac <- data.table::setDT(IndVac)[, .(count = .N),
                                        keyby = .(nuts, birthyear, vacagemonth = TimeMonth(birthdate, vacdate), doserecorded)]
  } else {
    if (!is.na(IndividualPopulationFile) & !is.na(AggregatedMCVFile)) {
      IndPop <- IndPopData(wdir, StatusDate, IndividualPopulationFile)
      AggPop <- data.table::setDT(IndPop)[, .(N = .N), keyby = .(nuts, birthyear)]
      AggVac <- AggVacData(wdir, StatusDate, AggregatedMCVFile)
    } else {
      if (!is.na(AggregatedPopulationFile) & !is.na(AggregatedMCVFile)) {
        AggPop <- AggPopData(wdir, StatusDate, AggregatedPopulationFile)
        AggVac <- AggVacData(wdir, StatusDate, AggregatedMCVFile)
      } else {
        stop("No input data")
      }
    }
  }

  x <- "n"
  if (dir.exists(paste0(wdir, "/Status_", gsub("-", "", StatusDate)))) {
    x <- readline(paste0("Directory ", wdir, "/Status_", gsub("-", "", StatusDate), " already exist. Overwrite? (Y/N) "))
  }
  if ((tolower(x) == "y") | !dir.exists(paste0(wdir, "/Status_", gsub("-", "", StatusDate)))) {

    unlink(paste0(wdir, "/Status_", gsub("-", "", StatusDate)), recursive = TRUE)

    if (!dir.exists(paste0(wdir, "/Status_", gsub("-", "", StatusDate)))) {
      dir.create(paste0(wdir, "/Status_", gsub("-", "", StatusDate)))
    }
    if (!dir.exists(paste0(wdir, "/Status_", gsub("-", "", StatusDate), "/InputData"))) {
      dir.create(paste0(wdir, "/Status_", gsub("-", "", StatusDate), "/InputData"))
    }
    for (f in c(IndividualPopulationFile, IndividualMCVFile, AggregatedPopulationFile, AggregatedMCVFile, AdministrativeMCVFile)) {
      if ((!is.na(f)) & (file.exists(paste0(wdir, "/data/", f)))) {
        file.copy(from = paste0(wdir, "/data/", f), to = paste0(wdir, "/Status_", gsub("-", "", StatusDate), "/InputData/", f),
                  overwrite = TRUE, recursive = FALSE, copy.mode = TRUE)
      }
    }
    if (!dir.exists(paste0(wdir, "/Status_", gsub("-", "", StatusDate), "/Data"))) {
      dir.create(paste0(wdir, "/Status_", gsub("-", "", StatusDate), "/Data"))
    }
    if (!dir.exists(paste0(wdir, "/Status_", gsub("-", "", StatusDate), "/Output"))) {
      dir.create(paste0(wdir, "/Status_", gsub("-", "", StatusDate), "/Output"))
    }

    ### Coverage data by NUTS, BirthYear, doserecorded, month of age at vaccination ###
    NUTSVacMonth <- cbind(StatusDate,
                          merge(AggPop, AggVac, by = c('nuts', 'birthyear'), all = TRUE))

    saveRDS(NUTSVacMonth, file = paste0(wdir, "/Status_", gsub("-", "", StatusDate), "/Data/NUTSVacMonth.RDS"))
  }
}


#' Read and prepare individual population input.
#'
#' @param wdir Working directory.
#' @param StatusDate Date of status calculation.
#' @param IndividualPopulationFile Individual input ;-separated population file
#' @return IndPop
IndPopData <- function(wdir, StatusDate, IndividualPopulationFile) {
  if (!file.exists(paste0(wdir, "/data/", IndividualPopulationFile))) {
    # Check if exist
    stop(paste0("NOT FOUND: Individual population file ", wdir, "/data/", IndividualPopulationFile))
  }
  # Read
  IndPop <- utils::read.csv(paste0(wdir, "/data/", IndividualPopulationFile), header = TRUE, sep = ';', as.is = TRUE)
  # Change all column names to lower case
  colnames(IndPop) <- tolower(colnames(IndPop))
  if (is.numeric(IndPop$personid)) { IndPop$personid <- sprintf("%010.0f", IndPop$personid) }

  if (sum(colnames(IndPop) %in% c("personid", "birthdate", "nuts")) != 3) {
    stop(paste0("ERROR: The variables PersonID, BirthDate or NUTS not in Individual population file: ", paste(colnames(IndPop), collapse = ", ")))
  }

  # Set all variables to character
  for (v in colnames(IndPop)) {
    if (!is.character(IndPop[, v])) { IndPop[, v] <- as.character(IndPop[, v]) }
  }

  if (sum(is.na(IndPop$personid)) > 0) {
    stop("ERROR: Missing PersonID")
  }

  if (sum(is.na(as.Date(IndPop$birthdate))) >0) {
    stop("ERROR: In Birthdate")
  }
  if (sum(IndPop$birthdate > StatusDate) > 0) {
    stop("ERROR: Birthdate after StatusDate")
  }
  IndPop$birthyear <- format(as.Date(IndPop$birthdate),'%Y')
  if (sum(as.numeric(IndPop$birthyear) < 2005) > 0) {
    stop("ERROR: BirthYear before 2005")
  }
  if (nrow(IndPop[duplicated(IndPop[, c("personid", "birthdate")]),]) > 0) {
    stop("ERROR: Duplicated PersonID, BirthDate")
  }

  if (sum(!(IndPop$nuts %in% readRDS(paste0(system.file("ShapeFile", package="MCVCovLoc"), "/NutsNames.RDS"))$NUTS_ID)) > 0) {
    stop("ERROR: Unknown NUTS")
  }
  if (min(nchar(IndPop$nuts)) != max(nchar(IndPop$nuts))) {
    IndPop$nuts <- substr(IndPop$nuts, 1, min(nchar(IndPop$nuts)))
  }

  return(IndPop)
}

#' Read and prepare individual vaccination input.
#'
#' @param wdir Working directory.
#' @param StatusDate Date of status calculation.
#' @param IndividualMCVFile Individual input ;-separated vaccination file
#' @import data.table
#' @return IndVac
IndVacData <- function(wdir, StatusDate, IndividualMCVFile) {
  personid <- vacdate <- doserecorded <- NULL

  if (!file.exists(paste0(wdir, "/data/", IndividualMCVFile))) {
    stop(paste0("NOT FOUND: Individual vaccination file ", wdir, "/data/", IndividualMCVFile))
  }

  # Read
  IndVac <- utils::read.csv(paste0(wdir, "/data/", IndividualMCVFile), header = TRUE, sep = ';', as.is = TRUE)
  # Change all column names to lower case
  colnames(IndVac) <- tolower(colnames(IndVac))
  if (is.numeric(IndVac$personid)) { IndVac$personid <- sprintf("%010.0f", IndVac$personid) }

  if (sum(colnames(IndVac) %in% c("personid", "birthdate", "vacdate", "doserecorded")) != 4) {
    stop(paste0("ERROR: The variables PersonID, BirthDate, VacDate or DoseRecorded not in Individual vaccination file: ", paste(colnames(IndVac), collapse = ", ")))
  }

  # Set all variables to character
  for (v in colnames(IndVac)) {
    if (!is.character(IndVac[, v])) { IndVac[, v] <- as.character(IndVac[, v]) }
  }

  if (sum(is.na(IndVac$personid)) > 0) {
    stop("ERROR: Missing PersonID")
  }

  if (sum(is.na(as.Date(IndVac$birthdate))) >0) {
    stop("ERROR: In Birthdate")
  }
  if (sum(IndVac$birthdate > StatusDate) > 0) {
    stop("ERROR: Birthdate after StatusDate")
  }
  IndVac$birthyear <- format(as.Date(IndVac$birthdate),'%Y')
  if (sum(as.numeric(IndVac$birthyear) < 2005) > 0) {
    stop("ERROR: BirthYear before 2005")
  }

  if (nrow(IndVac[!(IndVac$doserecorded %in% cbind("1", "2")),]) > 0) {
    stop("ERROR: Wrong or missing doserecorded")
  }
  if (nrow(IndVac[duplicated(IndVac[, c('personid', 'doserecorded')]),]) > 0) {
    stop("ERROR: The same recorded dose more times")
  }

  IndVac$vacdate <- as.Date(IndVac$vacdate)
  if (nrow(IndVac[is.na(IndVac$vacdate),]) > 0) {
    stop("ERROR: Wrong or missing VacDate")
  }
  if (nrow(IndVac[(IndVac$birthdate > IndVac$vacdate),]) > 0) {
    stop("ERROR: VacDate before BirthDate")
  }
  if (nrow(IndVac[(IndVac$vacdate > StatusDate),]) > 0) {
    stop("ERROR: VacDate after StatusDate")
  }

  if (nrow(data.table::setDT(IndVac)[order(personid, vacdate), .SD[2], by = personid][doserecorded == 'D1']) > 0) {
    stop("ERROR: Dose 1 after dose 2")
  }

  return(IndVac)
}

#' Read and prepare aggregated population input.
#'
#' @param wdir Working directory.
#' @param StatusDate Date of status calculation.
#' @param AggregatedPopulationFile Aggregated input ;-separated population file
#' @return AggPop
AggPopData <- function(wdir, StatusDate, AggregatedPopulationFile) {
  birthyear <- NULL
  if (!file.exists(paste0(wdir, "/data/", AggregatedPopulationFile))) {
    stop(paste0("NOT FOUND: Aggregated population file ", wdir, "/data/", AggregatedPopulationFile))
  }

  # Read
  AggPop <- utils::read.csv(paste0(wdir, "/data/", AggregatedPopulationFile), header = TRUE, sep = ';', as.is = TRUE)
  # Change all column names to lower case
  colnames(AggPop) <- tolower(colnames(AggPop))

  if (sum(colnames(AggPop) %in% c("nuts", "birthyear", "count")) != 3) {
    stop(paste0("ERROR: The variables NUTS, BirthYear or Count not in Aggregated population file: ", paste(colnames(AggPop), collapse = ", ")))
  }

  # Set all variables to character
  for (v in colnames(AggPop)) {
    if (!is.character(AggPop[, v])) { AggPop[, v] <- as.character(AggPop[, v]) }
  }

  if (sum(AggPop$birthyear < 2005) > 0) {
    stop("ERROR: BirthYear before 2005")
  }
  if (sum(AggPop$birthyear > as.numeric(format(as.Date(StatusDate),'%Y')))) {
    warning("Warning: BirthYear after year of StatusDate. These are excluded")
    AggPop <- subset(AggPop, birthyear <= as.numeric(format(as.Date(StatusDate),'%Y')))
  }

  if (sum(!(AggPop$nuts %in% readRDS(paste0(system.file("ShapeFile", package="MCVCovLoc"), "/NutsNames.RDS"))$NUTS_ID)) > 0) {
    stop("ERROR: Unknown NUTS")
  }
  if (min(nchar(AggPop$nuts)) != max(nchar(AggPop$nuts))) {
    AggPop$nuts <- substr(AggPop$nuts, 1, min(nchar(AggPop$nuts)))
  }

  return(AggPop)
}

#' Read and prepare aggregated vaccination input.
#'
#' @param wdir Working directory.
#' @param StatusDate Date of status calculation.
#' @param AggregatedMCVFile Aggregated input ;-separated vaccination file
#' @return AggVac
AggVacData <- function(wdir, StatusDate, AggregatedMCVFile) {
  birthyear <- VacPop <- NULL

  if (!file.exists(paste0(wdir, "/data/", AggregatedMCVFile))) {
    stop(paste0("NOT FOUND: aggregated vaccination file ", wdir, "/data/", AggregatedMCVFile))
  }

  # Read
  AggVac <- utils::read.csv(paste0(wdir, "/data/", AggregatedMCVFile), header = TRUE, sep = ';', as.is = TRUE)
  # Change all column names to lower case
  colnames(AggVac) <- tolower(colnames(AggVac))

  if (sum(colnames(AggVac) %in% c("nuts", "birthyear", "vacagemonth", "doserecorded", "count")) != 5) {
    stop(paste0("ERROR: The variables NUTS, BirthYear, VacAgeMonth, DoseRecorded or Count not in Aggregated vaccination file: ", paste(colnames(AggVac), collapse = ", ")))
  }

  # Set all variables to character
  for (v in colnames(AggVac)) {
    if (!is.character(AggVac[, v])) { AggVac[, v] <- as.character(AggVac[, v]) }
  }

  if (nrow(AggVac[!(AggVac$doserecorded %in% cbind("1", "2")),]) > 0) {
    stop("ERROR: Wrong or missing doserecorded")
  }

  if (sum(AggVac$birthyear < 2005) > 0) {
    stop("ERROR: BirthYear before 2005")
  }
  if (sum(AggVac$birthyear > as.numeric(format(as.Date(StatusDate),'%Y'))) > 0) {
    warning("Warning: BirthYear after year of StatusDate. These are excluded")
    AggVac <- subset(VacPop, birthyear <= as.numeric(format(as.Date(StatusDate),'%Y')))
  }

  if (sum(!(AggVac$nuts %in% readRDS(paste0(system.file("ShapeFile", package="MCVCovLoc"), "/NutsNames.RDS"))$NUTS_ID)) > 0) {
    stop("ERROR: Unknown NUTS")
  }
  if (min(nchar(AggVac$nuts)) != max(nchar(AggVac$nuts))) {
    AggVac$nuts <- substr(AggVac$nuts, 1, min(nchar(AggVac$nuts)))
  }

  return(AggVac)
}

#' Calculate time-difference in month.
#'
#' @param StartDate Starting date
#' @param EndDate End date
#' @return time-diff in month
TimeMonth <- function(StartDate, EndDate) {
  StartDate <- as.Date(StartDate)
  EndDate <- as.Date(EndDate)
  m <- 12*(as.numeric(format(EndDate, '%Y')) - as.numeric(format(StartDate, '%Y'))) +
    (as.numeric(format(EndDate, '%m')) - as.numeric(format(StartDate, '%m'))) -
    (as.numeric(format(EndDate, '%d')) < as.numeric(format(StartDate, '%d')))
  return(m)
}

#' Coverage graphs by birth cohort.
#'
#' @param wdir Working directory.
#' @param StatusDate Date of status calculation.
#' @param NUTS NUTS Id
#' @param dose First or second dose
#' @param save Should the graphs be saved to disk or returned
#' @import data.table
#' @import ggplot2
#' @return Coverage graph on \code{NUTS} by Birth cohort
#' @export
VacCovBC <- function(wdir, StatusDate, NUTS, dose, save = FALSE) {

  # wdir = 'S:/Data/MCVCovLoc_wdir'
  # StatusDate = '2020-01-01'
  # NUTS = 'DK01'
  # dose = 1
  # save = TRUE

  . <- N <- count <- birthyear <- nuts <- Cov <- NUTS_ID <- doserecorded <- NULL
  if (!dir.exists(paste0(wdir, "/Status_", gsub("-", "", StatusDate), "/Data"))) {
    stop(paste0("ERROR: Directory don't exist: ", wdir, "/Status_", gsub("-", "", StatusDate), "/Data"))
  }
  if (!file.exists(paste0(wdir, "/Status_", gsub("-", "", StatusDate), "/Data/NUTSVacMonth.RDS"))) {
    stop(paste0("ERROR: Data don't exist: ", wdir, "/Status_", gsub("-", "", StatusDate), "/Data/NUTSVacMonth.RDS"))
  }
  VacMonth <- readRDS(paste0(wdir, "/Status_", gsub("-", "", StatusDate), "/Data/NUTSVacMonth.RDS"))[(doserecorded == dose) & (substr(nuts, 1, nchar(NUTS)) == NUTS),]
  VacMonth <- data.table::setDT(VacMonth)[, .(N = mean(N), Cov = sum(count)), keyby = .(birthyear, nuts)]
  VacMonth <- data.table::setDT(VacMonth)[, .(N = sum(N), Cov = sum(Cov)), keyby = birthyear]
  VacMonth$Cov <- 100*VacMonth$Cov/VacMonth$N

  ### Coverage by BirthYear ###
  VacMonth$birthyear <- as.factor(VacMonth$birthyear)
  text <- subset(readRDS(paste0(system.file("ShapeFile", package="MCVCovLoc"), "/NutsNames.RDS")), NUTS_ID == NUTS)$NUTS_NAME
  # library(ggplot2)
  CovBirthCohort <- ggplot(VacMonth) +
    geom_bar(aes(x = birthyear, y = Cov), stat = "identity") +
    xlab("Birth year") + ylab("Percent vaccinated") +
    ggtitle(paste0(text, "(", NUTS,") - dose", dose, " coverage")) +
    theme(plot.title = element_text(hjust = 0.5)) +
    labs(caption = paste("Status", StatusDate))
  if (save) {
    suppressWarnings(
      ggsave(filename = paste0(wdir, "/Status_", gsub("-", "", StatusDate), "/Output/CovBirthCohortD", dose, "_", NUTS, ".png"),
             CovBirthCohort, width = 30, height = 21, units = "cm")
    )
  } else {
    return(CovBirthCohort)
  }
}

#' Coverage graphs by age of vaccination.
#'
#' @param wdir Working directory.
#' @param StatusDate Date of status calculation.
#' @param NUTS NUTS Id
#' @param dose First or second dose
#' @param save Should the graphs be saved to disk or returned
#' @import data.table
#' @import ggplot2
#' @return Coverage graph on \code{NUTS} by age at vaccination
#' @export
VacCovAge <- function(wdir, StatusDate, NUTS, dose, save = FALSE) {
  doserecorded <- Cov <- count <- . <- birthyear <- nuts <- N <- NUTS_ID <- vacagemonth <- NULL
  if (!dir.exists(paste0(wdir, "/Status_", gsub("-", "", StatusDate), "/Data"))) {
    stop(paste0("ERROR: Directory don't exist: ", wdir, "/Status_", gsub("-", "", StatusDate), "/Data"))
  }
  if (!file.exists(paste0(wdir, "/Status_", gsub("-", "", StatusDate), "/Data/NUTSVacMonth.RDS"))) {
    stop(paste0("ERROR: Data don't exist: ", wdir, "/Status_", gsub("-", "", StatusDate), "/Data/NUTSVacMonth.RDS"))
  }
  VacMonth <- readRDS(paste0(wdir, "/Status_", gsub("-", "", StatusDate), "/Data/NUTSVacMonth.RDS"))[(doserecorded == dose) & (substr(nuts, 1, nchar(NUTS)) == NUTS),]
  VacMonth <- data.table::setDT(VacMonth)[, Cov := cumsum(count), keyby = .(birthyear, nuts)]
  VacMonth <- data.table::setDT(VacMonth)[, .(N = sum(N), Cov = sum(Cov)), keyby = .(birthyear, vacagemonth)]
  VacMonth$Cov <- 100*VacMonth$Cov/VacMonth$N

  ### Coverage by vac-age in months and BirthYear ###
  VacMonth$birthyear <- as.factor(VacMonth$birthyear)
  text <- subset(readRDS(paste0(system.file("ShapeFile", package="MCVCovLoc"), "/NutsNames.RDS")), NUTS_ID == NUTS)$NUTS_NAME
  # library(ggplot2)
  CovMonth <- ggplot(subset(VacMonth, vacagemonth <= 150), aes(x = vacagemonth)) +
    geom_line(aes(y = Cov, group = birthyear, color = birthyear)) +
    scale_fill_brewer(palette="Set1") +
    ggtitle(paste0(text, "(", NUTS,") - dose", dose, " coverage")) + theme(plot.title = element_text(hjust = 0.5)) +
    xlab("Age in months") + ylab("Percent vaccinated") +
    theme(legend.position = "bottom", axis.text.x = element_text(angle = 60, hjust = 1, size = 7)) +
    scale_x_continuous(breaks = c(seq(0, 150, by = 6))) +
    labs(caption = paste("Status", StatusDate))

  if (save) {
    suppressWarnings(
      ggsave(filename = paste0(wdir, "/Status_", gsub("-", "", StatusDate), "/Output/CovVacAgeD", dose, "_", NUTS, ".png"), CovMonth, width = 30, height = 21, units = "cm")
    )
  } else {
    return(CovMonth)
  }
}

#' Create coverage map at NUTS-level by Birth Cohort.
#'
#' @param wdir Working directory.
#' @param StatusDate Date of status calculation.
#' @param NUTS NUTS Id
#' @param BirthCohort Birth year of cohort
#' @param dose First or second dose
#' @param save Should the map be saved to disk or returned
#' @import data.table
#' @import sf
#' @import raster
#' @import tmap
#' @return map
#' @export
MapCov <- function(wdir, StatusDate, NUTS, BirthCohort, dose, save = FALSE) {

  # wdir = 'S:/Data/MCVCovLoc_wdir_DK'
  # StatusDate = '2019-10-01'
  # BirthCohort = 2015
  # NUTS = 'DK'
  # dose = 2
  # save = FALSE
  # library(sf)
  # library(raster)
  # library(tmap)

  doserecorded <- count <- birthyear <- . <- N <- nuts <- NULL
  if (!dir.exists(paste0(wdir, "/Status_", gsub("-", "", StatusDate), "/Data"))) {
    stop(paste0("ERROR: Directory don't exist: ", wdir, "/Status_", gsub("-", "", StatusDate), "/Data"))
  }
  if (!file.exists(paste0(wdir, "/Status_", gsub("-", "", StatusDate), "/Data/NUTSVacMonth.RDS"))) {
    stop(paste0("ERROR: Data don't exist: ", wdir, "/Status_", gsub("-", "", StatusDate), "/Data/NUTSVacMonth.RDS"))
  }
  X <- readRDS(paste0(wdir, "/Status_", gsub("-", "", StatusDate), "/Data/NUTSVacMonth.RDS"))[(birthyear == BirthCohort) & (doserecorded == dose) & (substr(nuts, 1, nchar(NUTS)) == NUTS),]
  # X <- readRDS(paste0(wdir, "/Status_", gsub("-", "", StatusDate), "/Data/NUTSVacMonth.RDS"))
  X <- data.table::setDT(X)[, .(N = mean(N), Cov = sum(count)), by = nuts]
  X$Coverage <- 100*X$Cov/X$N

  tmap_options(show.messages = FALSE)
  NUTS_spdf <- rgdal::readOGR(system.file("ShapeFile", package="MCVCovLoc"), "NUTS_RG_01M_2016_4326", verbose = FALSE)
  # map1 <- merge(NUTS_spdf, X, by.x = "NUTS_ID", by.y = "nuts", duplicateGeoms = TRUE) # duplicateGeoms to include area without data
  map1 <- merge(NUTS_spdf, X, by.x = "NUTS_ID", by.y = "nuts", all = FALSE)

  tmap_mode("plot") # Fixed map
  # tmap_mode("view") # Zoomable map

  map <- tm_shape(map1) +
    tm_borders() +
    tm_fill(col = "Coverage", breaks = seq(0, 100, by = 5), palette = "Spectral") +
    tm_text("NUTS_NAME", size = 0.7) +
    tm_layout(title = paste("Birth cohort", BirthCohort, "\nDose", dose)) +
    tm_layout(bg.color = "lightblue", earth.boundary = TRUE, space.color="grey90") +
    tm_credits(paste("Status", StatusDate), align = "left")

  if (save) {
    tmap_save(map, filename = paste0(wdir, "/Status_", gsub("-", "", StatusDate), "/Output/map_BirthCohort", BirthCohort, "_D", dose, ".png"),
            width = 1920, height = 1080, asp = 0)
  } else {
    return(map)
  }
}
