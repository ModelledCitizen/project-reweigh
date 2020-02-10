library(foreign)
library(dplyr)
library(survey)

cps2 <- cps[["2016"]]
cps2$FLORIDA <- NA
cps2$FLORIDA[cps2$STATE == "Florida"] <- "FLORIDA"
cps2$FLORIDA[cps2$STATE != "Florida"] <- "OTHER STATES"

returns2 <- returns[["2016"]]
returns2$FLORIDA <- NA
returns2$FLORIDA[returns2$STATE == "Florida"] <- "FLORIDA"
returns2$FLORIDA[returns2$STATE != "Florida"] <- "OTHER STATES"

data(fips.state, package = "acs")
fips.state <- fips.state[1:51, ]

# List of census regions and divisions
regions <- read.csv("Data/Census FIPS Codes.csv")
regions <- regions[, c("State..FIPS.", "Region", "Division")]
names(regions) <- c("StateFIPS", "CREGION", "DIVISION")

# List of zip code-county crosswalks
data(zip_codes, package = "noncensus")
zip_codes <- setNames(zip_codes[, c("zip", "fips")], c("ZIPCODE", "CountyFIPS"))

# List of NCHS county type codes by county FIPS
nchscodes <- read.csv("Data/NCHS Codes.csv")
nchscodes <- nchscodes[, c("FIPS.code", "X2013.code")]
names(nchscodes) <- c("CountyFIPS", "NCHS")


exit <- read.spss("Data/Exit Poll 2016/fl.sav", to.data.frame = TRUE)

exit[exit == "-1"] <- NA

names(exit)[names(exit) == "STANUM"] <- "STATE"
names(exit)[names(exit) == "TELEPOLL"] <- "METHOD"
names(exit)[grep("AGE[89]", names(exit))] <- "XAGE"
names(exit)[grep("EDUC", names(exit))[1]] <- "EDUC"
names(exit)[grep("INCOME", names(exit))[1]] <- "INCOME"
names(exit)[grep("ATTEND", names(exit))[1]] <- "ATTEND"
names(exit)[grep("SPANISHQ", names(exit))] <- "LANGUAGE"

if (any(grepl("PRS", names(exit)))) {
    exit$PRES <- exit[[grep("PRS", names(exit))]]
} else {
    names(exit)[grep("PRE?S", names(exit))[1]] <- "PRES"
}

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

if (nlevels(exit$STATE) == 2) {
    levels(exit$STATE)[1] <- levels(exit$STATE)[2]
    exit$STATE <- factor(exit$STATE)
}

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

exit$PRES <-
    recode(
        exit$PRES,
        "Hillary Clinton" = 1,
        "Donald Trump" = 2,
        .default = 3
    )

exit$PRES <-
    factor(
        exit$PRES,
        levels = 1:3,
        labels = c("Democrat", "Republican", "Other")
    )

if ("INCOME" %in% names(exit)) {
    exit$INCOME <- factor(exit$INCOME, ordered = TRUE)
}

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
        "LANGUAGE"
    )

exit$STATE <-
    factor(
        exit$STATE,
        levels = levels(exit$STATE),
        labels = trimws(levels(exit$STATE), which = "both")
    )

exit$STATE <-
    factor(exit$STATE,
           levels = fips.state$STATE_NAME,
           labels = fips.state$STATE_NAME)

exit$XAGE <- factor(exit$XAGE, ordered = TRUE)

exit$EDUC <- factor(exit$EDUC, ordered = TRUE)

exit$StateFIPS <-
    factor(exit$STATE,
           levels = fips.state$STATE_NAME,
           labels = fips.state$STATE)

exit$StateFIPS <- as.integer(levels(exit$StateFIPS))[exit$StateFIPS]

exit$ZIPCODE <- sprintf("%05s", exit$ZIPCODE)

exit$CLUSTER <-
    ifelse(
        is.na(exit$PRECINCT),
        paste(exit$StateFIPS, exit$ZIPCODE, sep = "-"),
        paste(exit$StateFIPS, exit$PRECINCT, sep = "-")
    )

exit <- merge(exit, regions, by = "StateFIPS", all.x = TRUE)

exit <-
    merge(exit,
          zip_codes,
          by = "ZIPCODE",
          all.x = T,
          sort = F)

exit <-
    merge(exit,
          nchscodes,
          by = "CountyFIPS",
          all.x = T,
          sort = F)

exit$CREGION <-
    factor(
        exit$CREGION,
        levels = 1:4,
        labels = c("Northeast", "Midwest", "South", "West")
    )

exit$FLORIDA <- NA
exit$FLORIDA[exit$STATE == "Florida"] <- "FLORIDA"
exit$FLORIDA[exit$STATE != "Florida"] <- "OTHER STATES"

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


exit$CountyFIPS <- NULL
exit$ZIPCODE <- NULL
exit$StateFIPS <- NULL
exit$PRECINCT <- NULL

rm(vars)

y <- 2016
exit <- new.vars(exit)
rm(y)

samp.frame <- exit

samp.frame <-
    samp.frame[complete.cases(samp.frame[, c(
        "STATE",
        "SEX",
        "AGE3",
        "METHOD",
        "CREGION",
        "DIVISION",
        "RACEXED",
        "AGE8",
        "NCHS",
        "PRES"
    )]),]

design <- svydesign(ids = ~ CLUSTER, strata = ~ STATE, data = samp.frame)

pop.margins <-
    c(
        margin.gen(FLORIDA, c("SEX", "AGE3", "METHOD"), pop = cps2, samp = samp.frame, ret = returns2),
        list(
            RACEXED = cps.margin(FLORIDA, RACEXED, data = cps2, ret = returns2),
            AGE8 = cps.margin(FLORIDA, AGE8, data = cps2, ret = returns2),
            NCHS = county.margin(FLORIDA, NCHS, data = returns2),
            PRES = return.margin(FLORIDA, data = returns2)
        )
    )


pop.margins$RACEXED[2,] <- 0
pop.margins$AGE8[2,] <- 0
pop.margins$NCHS[2,] <- 0
pop.margins$PRES[2,] <- 0

raked2 <- rake(
    design = design,
    sample.margins = list(
        SEX = ~ STATE:SEX,
        AGE3 = ~ STATE:AGE3,
        METHOD = ~ STATE:METHOD,
        RACEXED = ~ FLORIDA:RACEXED,
        AGE8 =  ~ FLORIDA:AGE8,
        NCHS = ~ FLORIDA:NCHS,
        PRES = ~ FLORIDA:PRES
    ),
    population.margins = pop.margins,
    control = list(maxit = 300)
)

vars <-
    c("METHOD",
      "SEX",
      "AGE8",
      "INCOME",
      "EDUCWHITE",
      "EDUC3",
      "RACE4",
      "CREGION",
      "SIZEPLAC",
      "LANGUAGE")

sink("~/Desktop/Florida.txt")
for (var in vars) {
    print(svyby( ~ PRES, formula(paste0("~", var)), raked2, svymean))
}
sink()

svymean(~PRES, raked2)
