## ----eval = FALSE-------------------------------------------------------------
#  MCVCovLoc::InputData(
#    wdir = "S:/Data/MCVCovLoc_wdir_DK",
#    StatusDate = "2019-10-01",
#    IndividuelPopulationFile = "DKIndPop20191001.csv",
#    IndividuelMCVFile = "DKIndVac20191001.csv",
#    AggregatedPopulationFile = NA,
#    AggregatedMCVFile = NA,
#    AdministrativeMCVFile = NA
#  )

## ----eval = FALSE-------------------------------------------------------------
#  MCVCovLoc::VacCovBd(
#    wdir = "S:/Data/MCVCovLoc_wdir_DK",
#    StatusDate = "2019-10-01",
#    NUTS = "DK01",
#    dose = 1,
#    save = TRUE
#  )

## ----eval = FALSE-------------------------------------------------------------
#  MCVCovLoc::VacCovAge(
#    wdir = "S:/Data/MCVCovLoc_wdir",
#    StatusDate = "2019-10-01",
#    NUTS = "DK",
#    dose = 1,
#    save = TRUE
#  )

## ----eval = FALSE-------------------------------------------------------------
#  MCVCovLoc::MapCov(
#    wdir = 'S:/Data/MCVCovLoc_wdir_DK',
#    StatusDate = '2019-12-01',
#    BirthCohort = 2005,
#    NUTS = 'DK01',
#    dose = 2,
#    save = TRUE
#  )

## ----eval = FALSE-------------------------------------------------------------
#  MCVCovLoc::ShowCov(
#    wdir = "S:/Data/MCVCovLoc_wdir_DK"
#  )

