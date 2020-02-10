#=========================================================================#
# Comparing Raking Methodologies
#     1: Only national data, no imputation
#     2: Only national data, imputed missings
#     3: State and national data, imputed missings
#     A: Weighted using Candidate Support
#     B: Weighted without Candidate Support
#=========================================================================#

# Load / Install Packages -------------------------------------------------

library(foreign)
library(survey)
library(dplyr)
library(mitools)


# Load Preprocessed Data --------------------------------------------------

if (!exists("cps")) {
    if ("CPS.Rdata" %in% list.files("Data")) {
        load("Data/CPS.Rdata")
    } else {
        stop("Processed CPS data not found.")
    }
} 
if (!exists("returns")) {
    if ("Returns.Rdata" %in% list.files("Data")) {
        load("Data/Returns.Rdata")
    } else {
        stop("Processed Election Returns data not found.")
    }
} 
if (!exists("exitpoll")) {
    if ("Exit Poll.Rdata" %in% list.files("Data")) {
        load("Data/Exit Poll.Rdata")
    } else {
        stop("Processed Exit Poll data not found.")
    }
} 


# Startup -----------------------------------------------------------------

# Check if resuming
if (!exists("raked")) {
    if ("Raked Designs.Rdata" %in% list.files("Data")) {
        load("Data/Raked Designs.Rdata")
    } else {
        raked <- list()
    }
} 
# Check if a year is set in the environment
if (!exists("yr")) {
    y <- as.integer(readline(prompt = "Enter the year: "))
    yr <- as.character(y)
}
# Make sure there's space for new objects
if (is.null(raked[[yr]])) {
    raked[[yr]] <- list()
}

# Let the user know what we're up to
message("Raking exit poll data to benchmarks for ",
        yr,
        ":")

# Rake Method 1 -----------------------------------------------------------

samp.frame <- exitpoll[[yr]][["1"]]

samp.frame <- samp.frame[complete.cases(samp.frame[, marg.vars]), ]

pop.margins <- margin.gen(varset = marg.vars, samp = samp.frame)

margin.check(varset = marg.vars, method = 1, pop.margins)

design <- svydesign(ids = ~ CLUSTER, strata = ~ STATE, data = samp.frame)

message("    Raking national data with no imputations...")
cat("    ...using candidate support")
raked[[yr]][["1A"]] <- rake(
    design = design,
    sample.margins = list(
        SEX = ~ STATE:SEX,
        AGE3 = ~ STATE:AGE3,
        METHOD = ~ STATE:METHOD,
        RACEXED = ~ CREGION:RACEXED,
        AGE8 =  ~ CREGION:AGE8,
        NCHS = ~ CREGION:NCHS,
        PRES = ~ CREGION:PRES
    ),
    population.margins = pop.margins,
    control = list(maxit = 300)
)
cat(" ✓\n")

cat("    ...without candidate support")
raked[[yr]][["1B"]] <- rake(
    design = design,
    sample.margins = list(
        SEX = ~ STATE:SEX,
        AGE3 = ~ STATE:AGE3,
        METHOD = ~ STATE:METHOD,
        RACEXED = ~ CREGION:RACEXED,
        AGE8 =  ~ CREGION:AGE8,
        NCHS = ~ CREGION:NCHS
    ),
    population.margins = pop.margins[1:6],
    control = list(maxit = 300)
)
cat(" ✓\n")

rm(samp.frame, design, pop.margins)


# Rake Method 2 -----------------------------------------------------------

samp.frame <- imputationList(exitpoll[[yr]][["2"]])

pop.margins <- margin.gen(varset = marg.vars, samp = samp.frame)

margin.check(varset = marg.vars, method = 2, pop.margins)

design <- svydesign(ids = ~ CLUSTER, strata = ~ STATE, data = samp.frame)

message("    Raking national data with imputations...")
cat("    ...using candidate support")
raked[[yr]][["2A"]] <- design
raked[[yr]][["2A"]]$designs <- with(
    design,
    rake(
        sample.margins = list(
            SEX = ~ STATE:SEX,
            AGE3 = ~ STATE:AGE3,
            METHOD = ~ STATE:METHOD,
            RACEXED = ~ CREGION:RACEXED,
            AGE8 =  ~ CREGION:AGE8,
            NCHS = ~ CREGION:NCHS,
            PRES = ~ CREGION:PRES
        ),
        population.margins = pop.margins,
        control = list(maxit = 300)
    ),
    multicore = T
)
cat(" ✓\n")

cat("    ...without candidate support")
raked[[yr]][["2B"]] <- design
raked[[yr]][["2B"]]$designs <- with(
    design,
    rake(
        sample.margins = list(
            SEX = ~ STATE:SEX,
            AGE3 = ~ STATE:AGE3,
            METHOD = ~ STATE:METHOD,
            RACEXED = ~ CREGION:RACEXED,
            AGE8 =  ~ CREGION:AGE8,
            NCHS = ~ CREGION:NCHS
        ),
        population.margins = pop.margins[1:6],
        control = list(maxit = 300)
    ),
    multicore = T
)
cat(" ✓\n")

rm(samp.frame, design, pop.margins)

# Rake Method 3 -----------------------------------------------------------

samp.frame <- imputationList(exitpoll[[yr]][["3"]])

pop.margins <- margin.gen(varset = marg.vars, samp = samp.frame)

margin.check(varset = marg.vars, method = 3, pop.margins)

design <- svydesign(ids = ~ CLUSTER, strata = ~ STATE, data = samp.frame)

message("    Raking state and national data with imputations...")
cat("    ...using candidate support")
raked[[yr]][["3A"]] <- design
raked[[yr]][["3A"]]$designs <- with(
    design,
    rake(
        sample.margins = list(
            SEX = ~ STATE:SEX,
            AGE3 = ~ STATE:AGE3,
            METHOD = ~ STATE:METHOD,
            RACEXED = ~ CREGION:RACEXED,
            AGE8 =  ~ CREGION:AGE8,
            NCHS = ~ CREGION:NCHS,
            PRES = ~ CREGION:PRES
        ),
        population.margins = pop.margins,
        control = list(maxit = 300)
    ),
    multicore = T
)
cat(" ✓\n")

cat("    ...without candidate support")
raked[[yr]][["3B"]] <- design
raked[[yr]][["3B"]]$designs <- with(
    design,
    rake(
        sample.margins = list(
            SEX = ~ STATE:SEX,
            AGE3 = ~ STATE:AGE3,
            METHOD = ~ STATE:METHOD,
            RACEXED = ~ CREGION:RACEXED,
            AGE8 =  ~ CREGION:AGE8,
            NCHS = ~ CREGION:NCHS
        ),
        population.margins = pop.margins[1:6],
        control = list(maxit = 300)
    ),
    multicore = T
)
cat(" ✓\n")

# Clean & Save ------------------------------------------------------------

rm(samp.frame, design, pop.margins)

cat("Saving raked designs...")
save(raked, file = "Data/Raked Designs.Rdata")
cat(" ✓\n")
