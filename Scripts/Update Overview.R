st <- Sys.time()

#Input year of data being added to TPM

current_year <- 2020

#Project Location
setwd("C:/Users/DavidMcCullough/Documents/TPM_files")

#Data Location
TPM_filepath <- "Data/TPM Teacher Data.xlsx"
SPM_filepath <- "Data/SPM Student Data.xlsx"

#Updates TPM
source('Scripts/Model Updating Script AG.r')

et <- Sys.time()

print(paste("Updating took",round(as.numeric(et-st),2),"minutes"))
