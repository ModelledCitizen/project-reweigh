# Initialize --------------------------------------------------------------

# Load the readstata13 package to import the CPS data
library(readstata13)

# Check if a year is set in the environment
if (!exists("yr")) {
    y <- as.integer(readline(prompt = "Enter the year: "))
    yr <- as.character(y)
}

# Load Supplementary Data -------------------------------------------------

# List of states by FIPS code
data(fips.state, package = "acs")
fips.state <- fips.state[1:51, ]

# List of census regions by State FIPS
regions <- read.csv("Data/Census FIPS Codes.csv", fileEncoding="UTF-8-BOM")
regions <- regions[, c("State..FIPS.", "Region", "Division")]
names(regions) <- c("StateFIPS", "CREGION", "DIVISION")


# Import Stata File & Keep Only Voters ------------------------------------

message("Importing and recoding CPS data for ",
        yr,
        ":")

# Import the CPS data for the year in question
cpss <-
    read.dta13(paste0("Data/CPS ", y, " Nov.dta"), convert.factors = FALSE)

# Subset to people who voted
cpss <- cpss[cpss$pes1 == 1,]

# Recode Variables --------------------------------------------------------

# Rename the weight variable to match
cpss$WEIGHT <- as.numeric(cpss$pwsswgt)

# Label states with factor labels and FIPS levels
cpss$STATE <-
    factor(cpss$gestfips,
           levels = fips.state$STATE,
           labels = fips.state$STATE_NAME)

# Rename the age variable in the dataframe
names(cpss)[grep("[^f|x]age", names(cpss))[1]] <- "XAGE"

# Use the cut function to break continuous age into categorical
cpss$AGE8 <-
    cut(
        cpss$XAGE,
        breaks = c(17, 24, 29, 39, 44, 49, 59, 64, 100),
        labels = c(
            "18-24",
            "25-29",
            "30-39",
            "40-44",
            "45-49",
            "50-59",
            "60-64",
            "65+"
        ),
        ordered_result = T
    )

# Break AGE8 into AGE3
cpss$AGE3 <-
    factor(cpss$AGE8,
           labels = c(rep("18-29", 2), rep("30-59", 4), rep("60+", 2)),
           ordered = TRUE)

# Relevel sex factor to match exit poll
cpss$SEX <- factor(cpss$pesex, labels = c("Male", "Female"))

# Code for early voting based on supplementary questions
if (y == 2000) {
    cpss$METHOD <- NA
    cpss$METHOD[cpss$pes4 %in% 2:3] <- 1
    cpss$METHOD[cpss$pes4 == 1] <- 2
} else {
    cpss$METHOD <- NA
    cpss$METHOD[cpss$pes5 == 2 | cpss$pes6 == 2] <- 1
    cpss$METHOD[cpss$pes5 == 1 & cpss$pes6 == 1] <- 2
}
cpss$METHOD <- factor(cpss$METHOD, labels = c("Early", "EDay"))

# Ensure the FIPS code is an integer
cpss$StateFIPS <- as.integer(cpss$gestfips)

# Merge in the census region / division
cpss <- merge(cpss, regions, by = "StateFIPS", all.x = TRUE)

# Label census regions
cpss$CREGION <-
    factor(
        cpss$CREGION,
        levels = 1:4,
        labels = c("Northeast", "Midwest", "South", "West")
    )

# Label census divisions
cpss$DIVISION <-
    factor(
        cpss$DIVISION,
        levels = 1:9,
        labels = c(
            "NewEngland",
            "MiddleAtlantic",
            "EastNorthCentral",
            "WestNorthCentral",
            "SouthAtlantic",
            "EastSouthCentral",
            "WestSouthCentral",
            "Mountain",
            "Pacific"
        )
    )

# Recode race into four categories
if (y == 2000) {
    cpss$RACE4 <- NA
    cpss$RACE4[cpss$perace == 1 & cpss$prhspnon == 2] <- 1
    cpss$RACE4[cpss$perace == 2] <- 2
    cpss$RACE4[cpss$perace != 2 & cpss$prhspnon == 1] <- 3
    cpss$RACE4[cpss$perace %in% 3:4 & cpss$prhspnon == 2] <- 4
} else {
    black <- c(2, 6, 10, 11, 12, 16, 17, 18, 22, 23)
    cpss$RACE4 <- NA
    cpss$RACE4[cpss$ptdtrace == 1 & cpss$pehspnon == 2] <- 1
    cpss$RACE4[cpss$ptdtrace %in% black] <- 2
    cpss$RACE4[cpss$ptdtrace %in% setdiff(1:26, black) &
                  cpss$pehspnon == 1] <- 3
    cpss$RACE4[cpss$ptdtrace != 1 &
                  cpss$ptdtrace %in% setdiff(1:26, black) & cpss$pehspnon == 2] <- 4
    rm(black)
}
cpss$RACE4 <-
    factor(
        cpss$RACE4,
        levels = 1:4,
        labels = c("White", "Black", "Hispanic", "Other")
    )

# Cut education into three categories
cpss$EDUC3 <-
    cut(
        cpss$peeduca,
        breaks = c(30, 39, 42, 46),
        labels = c("HSLess", "SomeCollege", "College"),
        ordered_result = T
    )

# Interact Race and Education
cpss$RACEXED <- cpss$RACE4:cpss$EDUC3

rm(fips.state, regions)

# Subset & Save -----------------------------------------------------------

# Remove unused variables
cpss <-
    cpss[, c(
        "CREGION",
        "DIVISION",
        "STATE",
        "METHOD",
        "SEX",
        "AGE8",
        "AGE3",
        "RACE4",
        "EDUC3",
        "RACEXED",
        "WEIGHT"
    )]

if (exists("cps")) {
    cps[[as.character(y)]] <- cpss
} else if ("CPS.Rdata" %in% list.files("Data")) {
    load("Data/CPS.Rdata")
    cps[[as.character(y)]] <- cpss
} else {
    cps <- list()
    cps[[as.character(y)]] <- cpss
}

save(cps, file = "Data/CPS.Rdata")
cat("CPS data for", yr, "recoded.\n")

rm(cpss)
