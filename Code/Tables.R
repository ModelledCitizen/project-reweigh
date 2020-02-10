#=========================================================================#
# Comparing Raking Methodologies
#     A: Weighted using Candidate Support
#     B: Weighted without Candidate Support
#     1: Only national data, no imputation
#     2: Only national data, imputed missings
#     3: State and national data, imputed missings
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

if (!exists("tble")) {
    if ("Output Tables.Rdata" %in% list.files("Processed")) {
        load("Processed/Output Tables.Rdata")
    } else {
        tble <- list()
    }
} 


# Custom Function ---------------------------------------------------------

prp.table.maker <- function(num, cs, vlst = vars, year = yr, data = exitpoll, designs = raked) {
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
            
            tbl1 <- table(data[[year]][[num]][[var]])
            tbl1 <- round(tbl1)
            
            tbl2 <- svytable(formula(paste0("~ ", var)), designs[[year]][[rnm]])
            tbl2 <- prop.table(tbl2)
            tbl2 <- round(tbl2 * 100, 3)
            
            base::cbind(
                COUNT = tbl1,
                PROP = tbl2
            )
        }
        tbl1 <- table(data[[year]][[num]][["PRES"]])
        tbl1 <- round(tbl1)
        
        tbl2 <- svytable(~ PRES, designs[[year]][[rnm]])
        tbl2 <- prop.table(tbl2)
        tbl2 <- round(tbl2 * 100, 3)
        
        tbl <- base::cbind(COUNT = tbl1, PROP = tbl2)
        tbl <- base::rbind(base::cbind(sum(tbl1), 100), tbl)
        rownames(tbl)[1] <- "TOTAL"
        
        output <- base::rbind(tbl, output)
        
    } else if (num %in% 2:3) {
        output <- foreach(
            var = iter(vlst),
            .combine = base::rbind,
            .packages = c("survey")
        ) %dopar% {
            options(survey.lonely.psu = "certainty")
            
            all.tbl1 <- sapply(data[[year]][[num]], function(x) table(x[[var]]))
            tbl1 <- all.tbl1[,1]
            for (i in 1:nrow(all.tbl1)) {
                tbl1[i] <- mean(all.tbl1[i,])
            }
            tbl1 <- round(tbl1)
            
            all.tbl2 <-
                with(designs[[year]][[rnm]], svytable(as.formula(paste0("~", var))), multicore = T)
            all.tbl2 <- sapply(all.tbl2, prop.table)
            tbl2 <- all.tbl2[,1]
            for (i in 1:nrow(all.tbl2)) {
                tbl2[i] <- mean(all.tbl2[i,])
            }
            tbl2 <- round(tbl2 * 100, 3)
            
            base::cbind(
                COUNT = tbl1,
                PROP = tbl2
            )
        }
        all.tbl1 <- sapply(data[[year]][[num]], function(x) table(x[["PRES"]]))
        tbl1 <- all.tbl1[,1]
        for (i in 1:nrow(all.tbl1)) {
            tbl1[i] <- mean(all.tbl1[i,])
        }
        tbl1 <- round(tbl1)
        
        all.tbl2 <-
            with(designs[[year]][[rnm]], svytable( ~ PRES), multicore = T)
        all.tbl2 <- sapply(all.tbl2, prop.table)
        tbl2 <- all.tbl2[,1]
        for (i in 1:nrow(all.tbl2)) {
            tbl2[i] <- mean(all.tbl2[i,])
        }
        tbl2 <- round(tbl2 * 100, 3)
        
        tbl <- base::cbind(COUNT = tbl1, PROP = tbl2)
        tbl <- base::rbind(base::cbind(sum(tbl1), 100), tbl)
        rownames(tbl)[1] <- "TOTAL"
        
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
if (is.null(tble[[yr]])) {
    tble[[yr]] <- list()
}

# Let the user know what we're up to
message("Generating unweighted count / weighted proportion tables for ",
        yr,
        ":")


# Rake Method A -----------------------------------------------------------

cat("    A. Table with candidate support...\n")

## Method 1
cat("    ... 1. national only, without imputations")
tble[[yr]][["1A"]] <- prp.table.maker(1, "A")
cat(" ✓\n")

## Method 2
cat("    ... 2. national only, with imputations")
tble[[yr]][["2A"]] <- prp.table.maker(2, "A")
cat(" ✓\n")

## Method 3
cat("    ... 3. state and national data with imputations")
tble[[yr]][["3A"]] <- prp.table.maker(3, "A")
cat(" ✓\n")

## Combine
cat("    ... merged")
tble[[yr]][["A"]] <-
    base::cbind(tble[[yr]][["1A"]], tble[[yr]][["2A"]], tble[[yr]][["3A"]])
cat(" ✓\n")


# Rake Method B -----------------------------------------------------------

cat("    B. Table without candidate support...\n")

## Method 1
cat("    ... 1. national only, without imputations")
tble[[yr]][["1B"]] <- prp.table.maker(1, "B")
cat(" ✓\n")

## Method 2
cat("    ... 2. national only, with imputations")
tble[[yr]][["2B"]] <- prp.table.maker(2, "B")
cat(" ✓\n")

## Method 3
cat("    ... 3. state and national data with imputations")
tble[[yr]][["3B"]] <- prp.table.maker(3, "B")
cat(" ✓\n")

## Combine
cat("    ... merged")
tble[[yr]][["B"]] <-
    base::cbind(tble[[yr]][["1B"]], tble[[yr]][["2B"]], tble[[yr]][["3B"]])
cat(" ✓\n")


# Clean & Save ------------------------------------------------------------

cat("Saving output file...")
save(tble, file = "Processed/Output Tables.Rdata")
cat(" ✓\n")