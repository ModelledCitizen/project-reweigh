# Initialize --------------------------------------------------------------

# Load packages for import, recode, impute
library(foreign)
library(dplyr)
library(mice)

# Import functions from library
#source("Code/Library.R")

# Check if the file already exists
if (!exists("exitpoll")) {
    if ("Exit Poll.Rdata" %in% list.files("Data")) {
        load("Data/Exit Poll.Rdata")
    } else {
        exitpoll <- list()
    }
}
# Check if a year is set in the environment
if (!exists("yr")) {
    y <- as.integer(readline(prompt = "Enter the year: "))
    yr <- as.character(y)
}
# Make sure there's space for new objects
if (is.null(exitpoll[[yr]])) {
    exitpoll[[yr]] <- list()
}

# Let the user know what we're up to
message("Importing, extracting, imputing, and recoding exit poll data for ",
        yr,
        ":")

# Pre-set variables of interest
vars <-
    c(
        "ID",
        "ZIPCODE",
        "PRECINCT",
        "STATE",
        "REGION",
        "METHOD",
        "PRES",
        "SEX",
        "RACE",
        "XAGE",
        "PARTYID",
        "EDUC",
        "INCOME",
        "ATTEND",
        "SIZEPLAC",
        "LANGUAGE",
        "EDISON"
    )

# Load Supplementary Data -------------------------------------------------

# List of states by FIPS code
data(fips.state, package = "acs")
fips.state <- fips.state[1:51, ]

# List of census regions and divisions
regions <- read.csv("Data/Census FIPS Codes.csv", fileEncoding="UTF-8-BOM")
regions <- regions[, c("State..FIPS.", "Region", "Division")]
names(regions) <- c("StateFIPS", "CREGION", "DIVISION")

# List of zip code-county crosswalks
data(zip_codes, package = "noncensus")
zip_codes <- setNames(zip_codes[, c("zip", "fips")], c("ZIPCODE", "CountyFIPS"))

# List of NCHS county type codes by county FIPS
nchscodes <- read.csv("Data/NCHS Codes.csv", fileEncoding="UTF-8-BOM")
nchscodes <- nchscodes[, c("FIPS.code",
                                    if (y >= 2013)
                                        "X2013.code"
                                    else
                                        "X2006.code")]
names(nchscodes) <- c("CountyFIPS", "NCHS")


# Import National Results (1-2) -------------------------------------------

cat("    1. National data without imputations...\n")

cat("    ... importing")
exit <-
    read.spss(paste0("Data/Exit Poll ", y, ".sav"), to.data.frame = TRUE)
cat(" ✓\n")

cat("    ... extracting variables")
# Relabel improper missings
exit[exit == "-1"] <- NA

# Rename variables for consistency across years
names(exit)[names(exit) == "STANUM"] <- "STATE"
names(exit)[names(exit) == "TELEPOLL"] <- "METHOD"
names(exit)[grep("AGE[89]", names(exit))] <- "XAGE"
names(exit)[grep("EDUC", names(exit))[1]] <- "EDUC"
names(exit)[grep("INCOME", names(exit))[1]] <- "INCOME"
names(exit)[grep("ATTEND", names(exit))[1]] <- "ATTEND"
names(exit)[grep("SPANISHQ", names(exit))] <- "LANGUAGE"
names(exit)[grep("WE?I?GH?T", names(exit))] <- "EDISON"

# Extract the less missing of the two if both exist
if (any(grepl("PRS", names(exit)))) {
    exit$PRES <- exit[[grep("PRS", names(exit))]]
} else {
    names(exit)[grep("PRE?S", names(exit))[1]] <- "PRES"
}

# Generate variable if missing and ensure consistent levels
if (!"METHOD" %in% names(exit)) {
    exit$METHOD <- 2
    exit$METHOD <-
        factor(exit$METHOD,
               levels = 1:2,
               labels = c("Early", "EDay"))
} else {
    exit$METHOD <-
        factor(exit$METHOD,
               levels = levels(exit$METHOD),
               labels = c("Early", "EDay"))
}

# Deal with special cases of states
if (nlevels(exit$STATE) == 2) {
    levels(exit$STATE)[1] <- levels(exit$STATE)[2]
    exit$STATE <- factor(exit$STATE)
}

# Deal with special cases of regions
if (! "REGION" %in% names(exit)) {
    exit$REGION <- NA
    exit$REGION[exit$STATE %in% c(
        "Connecticut",
        "Delaware",
        "District of Columbia",
        "Maine",
        "Maryland",
        "Massachusetts",
        "New Hampshire",
        "New Jersey",
        "New York",
        "Pennsylvania",
        "Rhode Island",
        "Vermont"
    )] <- 1
    exit$REGION[exit$STATE %in% c(
        "Illinois",
        "Indiana",
        "Iowa",
        "Kansas",
        "Michigan",
        "Minnesota",
        "Missouri",
        "Nebraska",
        "North Dakota",
        "Ohio",
        "South Dakota",
        "Wisconsin"
    )] <- 2
    exit$REGION[exit$STATE %in% c(
        "Alabama",
        "Arkansas",
        "Florida",
        "Georgia",
        "Kentucky",
        "Louisiana",
        "Mississippi",
        "North Carolina",
        "Oklahoma",
        "South Carolina",
        "Tennessee",
        "Texas",
        "Virginia"
    )] <- 3
    exit$REGION[exit$STATE %in% c(
        "Alaska",
        "Arizona",
        "California",
        "Colorado",
        "Hawaii",
        "Idaho",
        "Montana",
        "Nevada",
        "New Mexico",
        "Oregon",
        "Utah",
        "Washington",
        "Wyoming"
    )] <- 4
    exit$REGION <-
        factor(
            exit$METHOD,
            levels = 1:4,
            labels = c("East", "Midwest", "South", "West")
        )
}

# Recode presidential vote variable
if (y == 2000) {
    exit$PRES <-
        recode(
            exit$PRES,
            "GORE" = 1,
            "BUSH" = 2,
            .default = 3
        )
} else if (y == 2004) {
    exit$PRES <-
        recode(
            exit$PRES,
            "Kerry" = 1,
            "Bush" = 2,
            "Nader" = 3,
            "Other" = 3
        )
} else if (y == 2008) {
    exit$PRES <-
        recode(
            exit$PRES,
            "Barack Obama" = 1,
            "John McCain" = 2,
            "Other" = 3
        )
} else if (y == 2012) {
    exit$PRES <-
        recode(
            exit$PRES,
            "Barack Obama" = 1,
            "Mitt Romney" = 2,
            "Other" = 3
        )
} else if (y == 2016) {
    exit$PRES <-
        recode(
            exit$PRES,
            "Hillary Clinton" = 1,
            "Donald Trump" = 2,
            "Other" = 3,
            "Jill Stein" = 3,
            "Gary Johnson" = 3
        )
}
exit$PRES <-
    factor(
        exit$PRES,
        levels = 1:3,
        labels = c("Democrat", "Republican", "Other")
    )

# Set ordinal variable
if ("INCOME" %in% names(exit)) {
  exit$INCOME <- factor(exit$INCOME, exclude = c("Omit", NA))
  if (y==2004) {
    exit$INCOME <- factor(
      exit$INCOME,
      labels = c(
        "Under $50,000",
        "Under $50,000",
        "Under $50,000",
        "$50,000 - $100,000",
        "$50,000 - $100,000",
        "$100,000 or more",
        "$100,000 or more",
        "$100,000 or more"
      ),
      ordered = TRUE
    )
  } else if (y == 2008) {
    exit$INCOME <- factor(
      exit$INCOME,
      labels = c(
        "Under $50,000",
        "Under $50,000",
        "Under $50,000",
        "$50,000 - $100,000",
        "$50,000 - $100,000",
        "$100,000 or more",
        "$100,000 or more",
        "$100,000 or more"
      ),
      ordered = TRUE
    )
  } else if (y == 2012) {
    exit$INCOME <- factor(
      exit$INCOME,
      labels = c(
        "Under $50,000",
        "Under $50,000",
        "$50,000 - $100,000",
        "$100,000 or more",
        "$100,000 or more",
        "$100,000 or more"
      ),
      ordered = TRUE
    )
  } else if (y == 2016) {
    exit$INCOME <- factor(
      exit$INCOME,
      labels = c(
        "Under $50,000",
        "Under $50,000",
        "$50,000 - $100,000",
        "$100,000 or more",
        "$100,000 or more",
        "$100,000 or more"
      ),
      ordered = TRUE
    )
  }
}


# Relabel and set ordinal variable
if ("ATTEND" %in% names(exit)) {
    exit$ATTEND <- factor(exit$ATTEND, exclude = c("Omit", NA))
    if (nlevels(exit$ATTEND) == 5) {
        levels(exit$ATTEND)[1] <- levels(exit$ATTEND)[2]
    }
    exit$ATTEND <- factor(
        exit$ATTEND,
        labels = c(
            "Once a week or more",
            "A few times a month",
            "A few times a year",
            "Never"
        ),
        ordered = TRUE
    )
}

# Set missing languages to english
if ("LANGUAGE" %in% names(exit)) {
    exit$LANGUAGE[is.na(exit$LANGUAGE)] <- "English"
}

# Remove blank rows and empty labels
exit <- exit[!(is.na(exit$STATE) & is.na(exit$METHOD)), vars]
exit <- droplevels(exit)

# Trim factor names to remove leading and trailing whitespace
exit$STATE <-
    factor(
        exit$STATE,
        levels = levels(exit$STATE),
        labels = trimws(levels(exit$STATE), which = "both")
    )

# Set labels and levels for consistency across years
exit$STATE <-
    factor(exit$STATE,
           levels = fips.state$STATE_NAME,
           labels = fips.state$STATE_NAME)

# Set ordinal variable
exit$XAGE <- factor(exit$XAGE, ordered = TRUE)

# Set ordinal variable
exit$EDUC <- factor(exit$EDUC, ordered = TRUE)

exit$StateFIPS <-
    factor(exit$STATE,
           levels = fips.state$STATE_NAME,
           labels = fips.state$STATE)

# Label state FIPS from state name
exit$StateFIPS <- as.integer(levels(exit$StateFIPS))[exit$StateFIPS]

# Reformat FIPS as an integer
exit$ZIPCODE <- sprintf("%05s", exit$ZIPCODE)

# Create cluster labels, combining state and precinct/zip code
exit$CLUSTER <-
    ifelse(
        is.na(exit$PRECINCT),
        paste(exit$StateFIPS, exit$ZIPCODE, sep = "-"),
        paste(exit$StateFIPS, exit$PRECINCT, sep = "-")
    )

# Assign regions and divisions by FIPS
exit <- merge(exit, regions, by = "StateFIPS", all.x = TRUE)

# Assign county FIPS by zip
exit <-
    merge(exit,
          zip_codes,
          by = "ZIPCODE",
          all.x = T,
          sort = F)

# Assign NCHS code by county FIPS
exit <-
    merge(exit,
          nchscodes,
          by = "CountyFIPS",
          all.x = T,
          sort = F)

# Label census regions
exit$CREGION <-
    factor(
        exit$CREGION,
        levels = 1:4,
        labels = c("Northeast", "Midwest", "South", "West")
    )

# Label census divisions
exit$DIVISION <-
    factor(
        exit$DIVISION,
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

# Label NCHS codes
exit$NCHS <-
    factor(
        exit$NCHS,
        levels = 1:6,
        labels = c(
            "LargeCentralMetro",
            "LargeFringeMetro",
            "MediumMetro",
            "SmallMetro",
            "Micropolitan",
            "Noncore"
        ),
        ordered = TRUE
    )

# Remove unneeded variables
exit$CountyFIPS <- NULL
exit$ZIPCODE <- NULL
exit$StateFIPS <- NULL
exit$PRECINCT <- NULL
cat(" ✓\n")

cat("    ... no imputation to be done ✓\n")


# Relevel, Collapse, Integrate (1) ----------------------------------------

cat("    ... recoding")
exitpoll[[yr]][["1"]] <- new.vars(exit)
cat(" ✓\n")

cat("    ... interstitial save")
save(exitpoll, file = "Data/Exit Poll.Rdata")
cat(" ✓\n")


# Impute Missings (2) -----------------------------------------------------

cat("    2. National data with imputations...\n")
cat("    ... importing ✓\n")
cat("    ... extracting variables ✓\n")
cat("    ... imputing missing data")

# Select variables that should not be imputed
exclude <- c("ID", "STATE", "REGION", "CLUSTER", "CREGION", "LANGUAGE", "EDISON")
exclude <- which(names(exit) %in% exclude)

# Subset to relevant variables
data <- exit[,-exclude]

# Impute missings with fixed conditional specification
exit.mids <- parlmice(data, m = 6, cluster.seed = 42, cl.type = "PSOCK")

# Turn set of imputations into list of dataframes
exit.mild <- complete(exit.mids, action = "all")

# Return removed variables
exit.impd <- lapply(exit.mild, base::cbind, exit[,exclude])

# Clean up workspace
rm(exclude, exit, data, exit.mids, exit.mild)
cat(" ✓\n")


# Relevel, Collapse, Integrate (2) ----------------------------------------

cat("    ... recoding")
exitpoll[[yr]][["2"]] <- lapply(exit.impd, new.vars)
cat(" ✓\n")

cat("    ... interstitial save")
save(exitpoll, file = "Data/Exit Poll.Rdata")
cat(" ✓\n")

rm(exit.impd)


# Import National and State Results (3) -----------------------------------

cat("    3. State and national data with imputations...\n")

cat("    ... importing")
# Repeat through all files in yearly directory
for (fi in list.files(paste0("Data/Exit Poll ", y), full.names = T)) {
    ex <- read.spss(fi, to.data.frame = TRUE)

    # Relabel improper missings
    ex[ex == "-1"] <- NA

    # Rename variables for consistency across years
    names(ex)[names(ex) == "STANUM"] <- "STATE"
    names(ex)[names(ex) == "TELEPOLL"] <- "METHOD"
    names(ex)[grep("PRE?S", names(ex))[1]] <- "PRES"
    names(ex)[grep("AGE[89]", names(ex))] <- "XAGE"
    names(ex)[grep("EDUC", names(ex))[1]] <- "EDUC"
    names(ex)[grep("INCOME", names(ex))[1]] <- "INCOME"
    names(ex)[grep("ATTEND", names(ex))[1]] <- "ATTEND"
    names(ex)[grep("SPANISHQ", names(ex))] <- "LANGUAGE"
    names(ex)[grep("WE?I?GH?T", names(ex))] <- "EDISON"

    # Extract the less missing of the two if both exist
    if (any(grepl("PRS", names(ex)))) {
        ex$PRES <- ex[[grep("PRS", names(ex))]]
    } else {
        names(ex)[grep("PRE?S", names(ex))[1]] <- "PRES"
    }

    # Generate variable if missing and ensure consistent levels
    if (!"METHOD" %in% names(ex)) {
        ex$METHOD <- 2
        ex$METHOD <-
            factor(ex$METHOD,
                   levels = 1:2,
                   labels = c("Early", "EDay"))
    } else {
        ex$METHOD <-
            factor(ex$METHOD,
                   levels = levels(ex$METHOD),
                   labels = c("Early", "EDay"))
    }

    # Deal with special cases of states
    if (nlevels(ex$STATE) == 2) {
        levels(ex$STATE)[1] <- levels(ex$STATE)[2]
        ex$STATE <- factor(ex$STATE)
    }

    # Deal with special cases of regions
    if (! "REGION" %in% names(ex)) {
        ex$REGION <- NA
        ex$REGION[ex$STATE %in% c(
            "Connecticut",
            "Delaware",
            "District of Columbia",
            "Maine",
            "Maryland",
            "Massachusetts",
            "New Hampshire",
            "New Jersey",
            "New York",
            "Pennsylvania",
            "Rhode Island",
            "Vermont"
        )] <- 1
        ex$REGION[ex$STATE %in% c(
            "Illinois",
            "Indiana",
            "Iowa",
            "Kansas",
            "Michigan",
            "Minnesota",
            "Missouri",
            "Nebraska",
            "North Dakota",
            "Ohio",
            "South Dakota",
            "Wisconsin"
        )] <- 2
        ex$REGION[ex$STATE %in% c(
            "Alabama",
            "Arkansas",
            "Florida",
            "Georgia",
            "Kentucky",
            "Louisiana",
            "Mississippi",
            "North Carolina",
            "Oklahoma",
            "South Carolina",
            "Tennessee",
            "Texas",
            "Virginia"
        )] <- 3
        ex$REGION[ex$STATE %in% c(
            "Alaska",
            "Arizona",
            "California",
            "Colorado",
            "Hawaii",
            "Idaho",
            "Montana",
            "Nevada",
            "New Mexico",
            "Oregon",
            "Utah",
            "Washington",
            "Wyoming"
        )] <- 4
        ex$REGION <-
            factor(
                ex$METHOD,
                levels = 1:4,
                labels = c("East", "Midwest", "South", "West")
            )
    }

    # Recode presidential vote variable
    if (y == 2000) {
        ex$PRES <-
            recode(
                ex$PRES,
                "GORE" = 1,
                "BUSH" = 2,
                .default = 3
            )
    } else if (y == 2004) {
        ex$PRES <-
            recode(
                ex$PRES,
                "Kerry" = 1,
                "Bush" = 2,
                "Nader" = 3,
                "Other" = 3
            )
    } else if (y == 2008) {
        ex$PRES <-
            recode(
                ex$PRES,
                "Barack Obama" = 1,
                "John McCain" = 2,
                "Other" = 3
            )
    } else if (y == 2012) {
        ex$PRES <-
            recode(
                ex$PRES,
                "Barack Obama" = 1,
                "Mitt Romney" = 2,
                "Other" = 3
            )
    } else if (y == 2016) {
        ex$PRES <-
            recode(
                ex$PRES,
                "Hillary Clinton" = 1,
                "Donald Trump" = 2,
                "Other" = 3,
                "Jill Stein" = 3,
                "Gary Johnson" = 3
            )
    }
    ex$PRES <-
        factor(
            ex$PRES,
            levels = 1:3,
            labels = c("Democrat", "Republican", "Other")
        )

    # Set ordinal variable
    if ("INCOME" %in% names(ex)) {
      ex$INCOME <- factor(ex$INCOME, exclude = c("Omit", NA))
      if (y==2004) {
        ex$INCOME <- factor(
          ex$INCOME,
          labels = c(
            "Under $50,000",
            "Under $50,000",
            "Under $50,000",
            "$50,000 - $100,000",
            "$50,000 - $100,000",
            "$100,000 or more",
            "$100,000 or more",
            "$100,000 or more"
          ),
          ordered = TRUE
        )
      } else if (y == 2008) {
        ex$INCOME <- factor(
          ex$INCOME,
          labels = c(
            "Under $50,000",
            "Under $50,000",
            "Under $50,000",
            "$50,000 - $100,000",
            "$50,000 - $100,000",
            "$100,000 or more",
            "$100,000 or more",
            "$100,000 or more"
          ),
          ordered = TRUE
        )
      } else if (y == 2012) {
        ex$INCOME <- factor(
          ex$INCOME,
          labels = c(
            "Under $50,000",
            "Under $50,000",
            "$50,000 - $100,000",
            "$100,000 or more",
            "$100,000 or more",
            "$100,000 or more"
          ),
          ordered = TRUE
        )
      } else if (y == 2016) {
        ex$INCOME <- factor(
          ex$INCOME,
          labels = c(
            "Under $50,000",
            "Under $50,000",
            "$50,000 - $100,000",
            "$100,000 or more",
            "$100,000 or more",
            "$100,000 or more"
          ),
          ordered = TRUE
        )
      }
    }

    # Relabel and set ordinal variable
    if ("ATTEND" %in% names(ex)) {
        ex$ATTEND <- factor(ex$ATTEND, exclude = c("Omit", NA))
        if (nlevels(ex$ATTEND) == 5) {
            levels(ex$ATTEND)[1] <- levels(ex$ATTEND)[2]
        }
        ex$ATTEND <- factor(
            ex$ATTEND,
            labels = c(
                "Once a week or more",
                "A few times a month",
                "A few times a year",
                "Never"
            ),
            ordered = TRUE
        )
    }

    # Reformat zip codes into string len 5
    ex$ZIPCODE <- sprintf("%05s", ex$ZIPCODE)

    # Sort by ID and include only wanted variables that exist
    ex <- ex[order(ex$ID), intersect(vars, names(ex))]

    # If not the first file, run through all repeated entries
    if (exists("exit")) {
        # if there are matches in the existing file and the current import
        if (sum(ex$ID %in% exit$ID) != 0) {
            # create a subset of the existing file that is just those matches
            ey <- ex[ex$ID %in% exit$ID,]
            # create a subset of the imported file that is just those matches
            ez <- exit[exit$ID %in% ex$ID,]
            # for each matching row
            for (i in 1:nrow(ey)) {
                # for each column in the combined file
                for (j in colnames(ey)) {
                    # if there is a discrepancy
                    if (!identical(ey[i, j], ez[i, j])) {
                        # if one is NA, take the value of the other
                        if (is.na(ey[i, j])) {
                            ey[i, j] <- ez[i, j]
                        } else if (is.na(ez[i, j])) {
                            ez[i, j] <- ey[i, j]
                        }
                    }
                    rm(j)
                }
                rm(i)
            }
            # return the subset to the imported file
            ex[ex$ID %in% exit$ID,] <- ey
            # return the subset to the existing file
            exit[exit$ID %in% ex$ID,] <- ez
            rm(ey, ez)
        }
        # merge, now without duplicated rows
        exit <- merge(exit, ex, all = T)
        # sort by ID so we can proceed in a known sequence when we want to
        exit <- exit[order(exit$ID),]
    } else {
        exit <- ex
    }
    rm(ex, fi)
}
cat(" ✓\n")

cat("    ... extracting variables")
# Set missing languages to english
if ("LANGUAGE" %in% names(exit)) {
    exit$LANGUAGE[is.na(exit$LANGUAGE)] <- "English"
}

# Remove blank rows and empty labels
exit <- exit[!(is.na(exit$STATE) & is.na(exit$METHOD)), ]
exit <- droplevels(exit)

# Trim factor names to remove leading and trailing whitespace
exit$STATE <-
    factor(
        exit$STATE,
        levels = levels(exit$STATE),
        labels = trimws(levels(exit$STATE), which = "both")
    )

# Set labels and levels for consistency across years
exit$STATE <-
    factor(exit$STATE,
           levels = fips.state$STATE_NAME,
           labels = fips.state$STATE_NAME)

# Set ordinal variable
exit$XAGE <- factor(exit$XAGE, ordered = TRUE)

# Set ordinal variable
exit$EDUC <- factor(exit$EDUC, ordered = TRUE)

# Label state FIPS from state name
exit$StateFIPS <-
    factor(exit$STATE,
           levels = fips.state$STATE_NAME,
           labels = fips.state$STATE)

# Reformat FIPS as an integer
exit$StateFIPS <- as.integer(levels(exit$StateFIPS))[exit$StateFIPS]

# Create cluster labels, combining state and precinct/zip code
exit$CLUSTER <-
    ifelse(
        is.na(exit$PRECINCT),
        paste(exit$StateFIPS, exit$ZIPCODE, sep = "-"),
        paste(exit$StateFIPS, exit$PRECINCT, sep = "-")
    )

# Assign regions and divisions by FIPS
exit <- merge(exit, regions, by = "StateFIPS", all.x = TRUE)

# Assign county FIPS by zip
exit <-
    merge(exit,
          zip_codes,
          by = "ZIPCODE",
          all.x = T,
          sort = F)

# Assign NCHS code by county FIPS
exit <-
    merge(exit,
          nchscodes,
          by = "CountyFIPS",
          all.x = T,
          sort = F)

# Label census regions
exit$CREGION <-
    factor(
        exit$CREGION,
        levels = 1:4,
        labels = c("Northeast", "Midwest", "South", "West")
    )

# Label census divisions
exit$DIVISION <-
    factor(
        exit$DIVISION,
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

# Label NCHS codes
exit$NCHS <-
    factor(
        exit$NCHS,
        levels = 1:6,
        labels = c(
            "LargeCentralMetro",
            "LargeFringeMetro",
            "MediumMetro",
            "SmallMetro",
            "Micropolitan",
            "Noncore"
        ),
        ordered = TRUE
    )

# Remove unneeded variables
exit$CountyFIPS <- NULL
exit$ZIPCODE <- NULL
exit$StateFIPS <- NULL
exit$PRECINCT <- NULL
cat(" ✓\n")

# Clean up workspace
rm(fips.state, nchscodes, regions, zip_codes, vars)


# Impute Missings (3) -----------------------------------------------------

cat("    ... imputing missing data")
# Select variables that should not be imputed
exclude <- c("ID", "STATE", "REGION", "CLUSTER", "CREGION", "LANGUAGE", "EDISON")
exclude <- which(names(exit) %in% exclude)

# Subset to relevant variables
data <- exit[,-exclude]

# Impute missings with fixed conditional specification
exit.mids <- parlmice(data, m = 6, cluster.seed = 42, cl.type = "PSOCK")

# Turn set of imputations into list of dataframes
exit.mild <- complete(exit.mids, action = "all")

# Return removed variables
exit.impd <- lapply(exit.mild, base::cbind, exit[,exclude])

# Clean up workspace
rm(exclude, data, exit, exit.mids, exit.mild)
cat(" ✓\n")

# Relevel and Collapse (3) ------------------------------------------------

cat("    ... recoding")
exitpoll[[yr]][["3"]] <- lapply(exit.impd, new.vars)
cat(" ✓\n")

cat("    ... interstitial save")
save(exitpoll, file = "Data/Exit Poll.Rdata")
cat(" ✓\n")

rm(exit.impd)
