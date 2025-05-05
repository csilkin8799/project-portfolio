#### Set working directory ####
# setwd("minfi-streamlining")

#### Load in helper functions I made ####
source("modernized_versions_of_minfi_functions_csilkin.R")

#### Load/install packages ####

# install.packages("BiocManager")
# install.packages("devtools")
# 
# # Install dependencies for tutorial (force = TRUE forces install)
# BiocManager::install(
#   c("minfi", "minfiData", "sva", "FlowSorted.Blood.450k", "Gviz", "shinyMethyl", "shinyMethylData"),
#   force = TRUE
# )
# 
# # Install minfi tutorial
# # Link to tutorial: https://www.bioconductor.org/help/course-materials/2015/BioC2015/methylation450k.html
# devtools::install_github("hansenlab/tutorial.450k")

library(minfi)
library(minfiData)
library(sva)

#### Load in example dataset ####

baseDir <- system.file("extdata", package="minfiData")
targets <- minfi::read.metharray.sheet(baseDir)

#### Read in iDAT files ####

RGSet <- minfi::read.metharray.exp(targets = targets)

#### Create PDF report of Quality Control using minfi helper functions I made ####

new_DNAm_qc_report(
  rgSet = RGSet,
  pdf_filename = "example_minfi_DNAm_QC_report_csilkin.pdf",
  qcPlotTitle = "Pass/Fail Plot for Minfi Tutorial Data",
  SexAtBirthPlotTitle = "Sex at Birth Prediction Plot for Minfi Tutorial Data",
  sampNames = RGSet$Sample_Name,
  sampGroups = RGSet$Sample_Group,
  actual_sex = RGSet$sex
)
