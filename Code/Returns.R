# Initialize --------------------------------------------------------------

# Check if a year is set in the environment
if (!exists("yr")) {
    y <- as.integer(readline(prompt = "Enter the year: "))
    yr <- as.character(y)
}


# Load Supplementary Data -------------------------------------------------

# List of states by FIPS code
data(fips.state, package = "acs")
fips.state <- fips.state[1:51, ]

# List of regions by State FIPS
regions <- read.csv("Data/Census FIPS Codes.csv", fileEncoding="UTF-8-BOM")
regions <- regions[, c("State..FIPS.", "Region", "Division")]
names(regions) <- c("StateFIPS", "CREGION", "DIVISION")


# Import & Relabel Vote Counts --------------------------------------------

message("Importing and recoding election returns data for ",
        yr,
        ":")

# Read official results
rtn <- read.csv(paste0("Data/Election Returns ", y, ".csv"), fileEncoding="UTF-8-BOM")

# Select only neccessary columns
rtn <- rtn[, c("State", "County", "Dem..Votes", "Rep..Votes", "Other.Votes")]

# Rename columns
names(rtn) <- c("STATE", "COUNTY", "DEM", "REP", "OTHER")

# Calculate Total column
rtn$TOTAL <- rowSums(rtn[, c("DEM", "REP", "OTHER")], na.rm = TRUE)


# Interpolate County Returns for Alaska -----------------------------------

# Import Alaska crosswalk file
if (y >= 2014) {
    alaska <- read.csv("Data/Alaska 2014.csv")
    alaska <- alaska[, c(1:2, 4, 6:7)]
} else if (y >= 2012) {
    alaska <- read.csv("Data/Alaska 2012.csv")
    alaska <- alaska[, c(1:2, 4, 6:7)]
} else if (y >= 2010) {
    alaska <- read.csv("Data/Alaska 2010.csv")
    alaska <- alaska[, c(1:2, 4, 6:7)]
} else {
    alaska <- read.csv("Data/Alaska 2000.csv")
    alaska <- alaska[, c(1:3, 5:6)]
}

# Rename columns for consistency & merge
names(alaska) <- c("FIPS", "DISTRICT", "COUNTY", "Corr1", "Corr2")

# Perform initial name substitutions
alaska$COUNTY <- sub(" AK", "", alaska$COUNTY, fixed = TRUE)
alaska$COUNTY <- sub(" (D)", "", alaska$COUNTY, fixed = TRUE)

# Rename the districts to match "county" names in returns
alaska$DISTRICT <- paste("Election District", alaska$DISTRICT)

# Merge appropriate returns columns with alaska columns
alaska <-
    merge(alaska,
          rtn[rtn$STATE == "AK",],
          by.x = "DISTRICT",
          by.y = "COUNTY",
          all.x = TRUE)

# Distribute votes according to population share
alaska$DEM <- alaska$DEM * alaska$Corr2
alaska$REP <- alaska$REP * alaska$Corr2
alaska$OTHER <- alaska$OTHER * alaska$Corr2
alaska$TOTAL <- alaska$TOTAL * alaska$Corr2

# Aggregate and recombine alaska rows with the returns
rtn <-
    base::rbind(aggregate(
        alaska[, 7:10],
        by = list(STATE = alaska$STATE, COUNTY = alaska$COUNTY),
        FUN = sum
    ), rtn[rtn$STATE != "AK", ])

rm(alaska)


# Add NCHS County Types ---------------------------------------------------

# Import county type labels
counties <- read.csv("Data/NCHS Codes.csv")

# Select relevant columns
counties <-
    counties[, c("FIPS.code",
                 "State.Abr.",
                 "County.name",
                 if (y >= 2013)
                     "X2013.code"
                 else
                     "X2006.code")]

# Rename columns to be used
names(counties) <- c("FIPS", "STATE", "COUNTY", "NCHS")

# Substitute elements of county names to match and merge
counties$COUNTY <-
    toupper(
        gsub(
            "CITY AND BOROUGH|BOROUGH|CITY|MUNICIPALITY|COUNTY|CENSUS|AREA|PARISH|[[:punct:]]|[[:space:]]",
            "",
            counties$COUNTY,
            ignore.case = TRUE
        )
    )
    
# Substitute elements of county names to match and merge
rtn$COUNTY <-
    toupper(
        gsub(
            "CITY AND BOROUGH|BOROUGH|CITY|MUNICIPALITY|COUNTY|CENSUS|AREA|PARISH|[[:punct:]]|[[:space:]]",
            "",
            rtn$COUNTY,
            ignore.case = TRUE
        )
    )

# Merge returns with county codes
rtn <- merge(rtn, counties, all.x = TRUE)

# Remove duplicated rows with different FIPS
rtn <- rtn[!duplicated(rtn[, 1:6]), ]

# Label NCHS county types
rtn$NCHS <-
    factor(
        rtn$NCHS,
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

rm(counties)

# Assign Regions & Divisions ----------------------------------------------

# Extract state FIPS code
rtn$StateFIPS <- as.integer(substr(rtn$FIPS, 1, nchar(rtn$FIPS) - 3))

# Relabel state names
rtn$STATE <-
    factor(rtn$StateFIPS,
           levels = fips.state$STATE,
           labels = fips.state$STATE_NAME)

# Merge with census region and divisions
rtn <- merge(rtn, regions, by = "StateFIPS", all.x = TRUE)

# Label census regions
rtn$CREGION <-
    factor(
        rtn$CREGION,
        levels = 1:4,
        labels = c("Northeast", "Midwest", "South", "West")
    )

# Label census divisions
rtn$DIVISION <-
    factor(
        rtn$DIVISION,
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

# Remove unnecessary columns
rtn$StateFIPS <- NULL
rtn$FIPS <- NULL

rm(regions, fips.state)

# Save and Integrate ------------------------------------------------------

if (exists("returns")) {
    returns[[as.character(y)]] <- rtn
} else if ("Returns.Rdata" %in% list.files("Data")) {
    load("Data/Returns.Rdata")
    returns[[as.character(y)]] <- rtn
} else {
    returns <- list()
    returns[[as.character(y)]] <- rtn
}

save(returns, file = "Data/Returns.Rdata")
cat("Election returns for", yr, "recoded.\n")

rm(rtn)
