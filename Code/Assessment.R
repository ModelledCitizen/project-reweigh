#=========================================================================#
# Comparing Raking Methodologies
#     1: Only national data, no imputation
#     2: Only national data, imputed missings
#     3: State and national data, imputed missings
#     A: Weighted using Candidate Support
#     B: Weighted without Candidate Support
#=========================================================================#

# Load / Install Packages -------------------------------------------------

library(mitools)
library(survey)
library(foreach)
library(doParallel)

# Load Preprocessed Data --------------------------------------------------

if (!exists("raked")) {
    if ("Raked Designs.Rdata" %in% list.files("Data")) {
        load("Data/Raked Designs.Rdata")
    } else {
        stop("Raked designs not found.")
    }
} 
if (!exists("exitpoll")) {
    if ("Exit Poll.Rdata" %in% list.files("Data")) {
        load("Data/Exit Poll.Rdata")
    } else {
        stop("Processed Exit Poll data not found.")
    }
} 

# Check if Resuming -------------------------------------------------------

if (!exists("out")) {
    if ("Result Comparisons.Rdata" %in% list.files("Processed")) {
        load("Processed/Result Comparisons.Rdata")
    } else {
        out <- list()
    }
} 


# Custom Function ---------------------------------------------------------

res.table.maker <- function(num, cs, vlst = vars, year = yr, data = exitpoll, designs = raked) {
    clus <- makeCluster(3)
    registerDoParallel(clus)
    rnm <- paste0(num, cs)
    if (num == 1) {
        output <- foreach(
            var = iter(vlst),
            .combine = base::rbind,
            .packages = c("survey")
        ) %dopar% {
            options(survey.lonely.psu = "certainty")
            frm <- as.formula(paste0("~", var))
            tbl <- svyby( ~ PRES, frm, designs[[year]][[rnm]], svymean)
            nms <- list(levels(data[[year]][[1]][[var]]))
            prp <- matrix(round(100 * coef(tbl), 2),
                          ncol = 3,
                          dimnames = nms)
            ser <- matrix(round(100 * unlist(SE(tbl)), 3),
                          ncol = 3,
                          dimnames = nms)
            base::cbind(
                DEM = prp[, 1],
                SE = ser[, 1],
                REP = prp[, 2],
                SE = ser[, 2],
                OTH = prp[, 3],
                SE = ser[, 3]
            )
        }
        tbl <- svymean(~PRES, designs[[year]][[rnm]])
        tbl <- c(round(100 * coef(tbl), 2), round(100 * SE(tbl), 3))
        tbl <- base::cbind(
            DEM = tbl[1],
            SE = tbl[4],
            REP = tbl[2],
            SE = tbl[5],
            OTH = tbl[3],
            SE = tbl[6]
        )
        rownames(tbl) <- "Candidate"
        output <- base::rbind(tbl, output)
    } else if (num %in% 2:3) {
        output <- foreach(
            var = iter(vlst),
            .combine = base::rbind,
            .packages = c("survey", "mitools")
        ) %dopar% {
            options(survey.lonely.psu = "certainty")
            frm <- as.formula(paste0("~", var))
            tbl <- MIcombine(with(designs[[year]][[rnm]], svyby(~ PRES, frm, svymean), multicore = T))
            nms <- list(levels(data[[year]][[1]][[var]]))
            prp <- matrix(round(100 * coef(tbl), 2),
                          ncol = 3,
                          dimnames = nms)
            ser <- matrix(round(100 * SE(tbl), 3),
                          ncol = 3,
                          dimnames = nms)
            base::cbind(
                DEM = prp[, 1],
                SE = ser[, 1],
                REP = prp[, 2],
                SE = ser[, 2],
                OTH = prp[, 3],
                SE = ser[, 3]
            )
        }
        tbl <- MIcombine(with(designs[[year]][[rnm]], svymean(~PRES)))
        tbl <- c(round(100 * coef(tbl), 2), round(100 * SE(tbl), 3))
        tbl <- base::cbind(
            DEM = tbl[1],
            SE = tbl[4],
            REP = tbl[2],
            SE = tbl[5],
            OTH = tbl[3],
            SE = tbl[6]
        )
        rownames(tbl) <- "Candidate"
        output <- base::rbind(tbl, output)
    }
    stopCluster(clus)
    return(output)
}


# Startup -----------------------------------------------------------------

# Check if a year is set in the environment
if (!exists("yr")) {
    y <- as.integer(readline(prompt = "Enter the year: "))
    yr <- as.character(y)
}
# Make sure there's space for new objects
if (is.null(out[[yr]])) {
    out[[yr]] <- list()
}

options(survey.lonely.psu = "certainty")

# Let the user know what we're up to
message("Generating main result tables for ",
        yr,
        ":")


# Rake Method A -----------------------------------------------------------

cat("    A. Table with candidate support...\n")

## Method 1
cat("    ... 1. national only, without imputations")
out[[yr]][["1A"]] <- res.table.maker(1, "A")
cat(" ✓\n")

## Method 2
cat("    ... 2. national only, with imputations")
out[[yr]][["2A"]] <- res.table.maker(2, "A")
cat(" ✓\n")

## Method 3
cat("    ... 3. state and national data with imputations")
out[[yr]][["3A"]] <- res.table.maker(3, "A")
cat(" ✓\n")

## Combine
cat("    ... merged")
out[[yr]][["A"]] <-
    base::cbind(out[[yr]][["1A"]], out[[yr]][["2A"]], out[[yr]][["3A"]])
cat(" ✓\n")


# Rake Method B -----------------------------------------------------------

cat("    B. Table without candidate support...\n")

## Method 1
cat("    ... 1. national only, without imputations")
out[[yr]][["1B"]] <- res.table.maker(1, "B")
cat(" ✓\n")

## Method 2
cat("    ... 2. national only, with imputations")
out[[yr]][["2B"]] <- res.table.maker(2, "B")
cat(" ✓\n")

## Method 3
cat("    ... 3. state and national data with imputations")
out[[yr]][["3B"]] <- res.table.maker(3, "B")
cat(" ✓\n")

## Combine
cat("    ... merged")
out[[yr]][["B"]] <-
    base::cbind(out[[yr]][["1B"]], out[[yr]][["2B"]], out[[yr]][["3B"]])
cat(" ✓\n")


# Clean & Save ------------------------------------------------------------

cat("Saving output file...")
save(out, file = "Processed/Result Comparisons.Rdata")
cat(" ✓\n")