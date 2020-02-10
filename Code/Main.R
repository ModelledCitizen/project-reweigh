# Load Packages -----------------------------------------------------------

library(readstata13) # cps data are stata13
library(acs) # state fips crosswalks
library(foreign) # exit poll data are spss
library(dplyr) # manipulation
library(mice) # multiple imputations
library(noncensus) # zip-code crosswalk
library(survey) # survey stratification / raking
library(mitools) # multiple imputation tools
library(foreach) # multicore looping
library(doParallel) # multicore looping
library(rmarkdown) # html output
library(kableExtra) # custom html output

# Working directory is automatically set if you load from the project file 
switch(Sys.info()[['sysname']], Windows = {
  setwd(paste0("C:/Users/", Sys.info()[['login']],"/Box/Project Reweigh/"))
}, Darwin = {
  setwd(paste0("/Users/", Sys.info()[['login']],"/Box/Project Reweigh/"))
})


# Load Custom Functions ---------------------------------------------------

source("Code/Library.R")


# Process Data ------------------------------------------------------------

for (y in c(2004, 2008, 2012, 2016)) {
    yr <- as.character(y)
    
    ## CPS Recode
    source("Code/CPS.R")
    
    ## Election Returns
    source("Code/Returns.R")
    
    ## Exit Poll
    source("Code/Exit Poll.R")
}


# Reweigh -----------------------------------------------------------------

marg.vars <- # Set raking variables & margin generation method
    c(
        GROUP1 = "STATE",
        CPS = "SEX",
        CPS = "AGE3",
        CPS = "METHOD",
        GROUP2 = "CREGION",
        CPS = "RACEXED",
        CPS = "AGE8",
        COUNTY = "NCHS",
        RETURN = "PRES"
    )

for (y in c(2004, 2008, 2012, 2016)) {
    yr <- as.character(y)
    source("Code/Comparison.R")
}


# Generate Output Tables --------------------------------------------------

vars <-
    c("METHOD",
      "SEX",
      "AGE4",
      "ATTEND",
      "INCOME",
      "EDUCWHITE",
      "EDUC3",
      "RACE4",
      "CREGION",
      "SIZEPLAC",
      "LANGUAGE")

for (y in c(2004, 2008, 2012, 2016)) {
    yr <- as.character(y)
    source("Code/Assessment.R")
    source("Code/Tables.R")
}

rmarkdown::render("Results.Rmd")
