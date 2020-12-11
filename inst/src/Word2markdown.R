# require(rmarkdown)
# require(devtools)

### Vignette ###

### Convert Word document to Markdown
# rmarkdown::pandoc_convert("S:/Data/MCVCovLoc/inst/src/Users_guide.docx", to = "markdown", output = "Users_guide.rmd", options=c("--extract-media=."))

### Create and setup initial vignette
# usethis::use_vignette("install_guide", "Installation_guide")

devtools::build_vignettes()

devtools::install(build_vignettes = TRUE)

### README ###

### Create and setup initial vignette
# usethis::use_readme_rmd()

devtools::install_github("JensXII/MCVCovLoc", build_vignettes = TRUE, force = TRUE)
devtools::install_github("JensXII/MCVCovLoc@version2", build_vignettes = TRUE, force = TRUE)

devtools::install_github("JensXII/MCVCovLoc@version2", build_opts = c("--no-resave-data", "--no-manual"), build_vignettes = TRUE, force = TRUE)
