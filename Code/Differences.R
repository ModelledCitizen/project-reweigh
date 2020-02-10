#=========================================================================#
# Comparing Raking Methodologies
#     1: Only national data, no imputation
#     2: Only national data, imputed missings
#     3: State and national data, imputed missings
#     A: Weighted using Candidate Support
#     B: Weighted without Candidate Support
#=========================================================================#

# Load / Install Packages -------------------------------------------------

library(survey)
library(mitools)


# Load Preprocessed Data --------------------------------------------------

load("Data/Raked Designs.Rdata")
load("Data/Exit Poll.Rdata")


# Check if Resuming -------------------------------------------------------

if (!exists("diff")) {
    if ("Difference Matrices.Rdata" %in% list.files("Processed")) {
        load("Processed/Difference Matrices.Rdata")
    } else {
        matrices <- list()
    }
} 


# Select ------------------------------------------------------------------

vars <-
    c("METHOD",
      "SEX",
      "AGE8",
      "ATTEND",
      "INCOME",
      "EDUCWHITE",
      "EDUC3",
      "RACE4",
      "CREGION",
      "SIZEPLAC",
      "LANGUAGE")
            
options(survey.lonely.psu = "certainty")


# Rake Method 1 -----------------------------------------------------------

# Method 1A

row <- nlevels(raked[["2016"]][["1A"]]$variables$EDUC3)
mat <- matrix(nrow = row, ncol = 8)

for (counter in 0:3) {
    year <- as.character(2004 + counter * 4)
    design <- raked[[year]][["1A"]]
    for (i in 1:row) {
        subdesign <- subset(design, EDUC3 == levels(design$variables$EDUC3)[i])
        imp.res <- svymean(~ PRES, subdesign)
        varcov <- vcov(imp.res)
        imp.res <- as.data.frame(imp.res)
        varcov <- as.data.frame(varcov)
        diff <- imp.res[1, 1] - imp.res[2, 1]
        serr <- imp.res[1, 2] + imp.res[2, 2] - 2 * varcov[1,2]
        mat[i, 2 * counter + 1] <- diff
        mat[i, 2 * counter + 2] <- serr
        rm(i, subdesign, imp.res, varcov, diff, serr)
    }
    rm(counter, year, design)
}
rm(row)

dimnames(mat) <-
    list(
        levels(raked[["2016"]][["1A"]]$variables$EDUC3),
        c(
            "2004.diff",
            "2004.std.err",
            "2008.diff",
            "2008.std.err",
            "2012.diff",
            "2012.std.err",
            "2016.diff",
            "2016.std.err"
        )
    )

matrices[["EDUC3"]][["1A"]] <- mat
rm(mat)


row <- nlevels(raked[["2016"]][["1A"]]$variables$SEX)
mat <- matrix(nrow = row, ncol = 8)

for (counter in 0:3) {
    year <- as.character(2004 + counter * 4)
    design <- raked[[year]][["1A"]]
    for (i in 1:row) {
        subdesign <- subset(design, SEX == levels(design$variables$SEX)[i])
        imp.res <- svymean(~ PRES, subdesign)
        varcov <- vcov(imp.res)
        imp.res <- as.data.frame(imp.res)
        varcov <- as.data.frame(varcov)
        diff <- imp.res[1, 1] - imp.res[2, 1]
        serr <- imp.res[1, 2] + imp.res[2, 2] - 2 * varcov[1,2]
        mat[i, 2 * counter + 1] <- diff
        mat[i, 2 * counter + 2] <- serr
        rm(i, subdesign, imp.res, varcov, diff, serr)
    }
    rm(counter, year, design)
}
rm(row)

dimnames(mat) <-
    list(
        levels(raked[["2016"]][["1A"]]$variables$SEX),
        c(
            "2004.diff",
            "2004.std.err",
            "2008.diff",
            "2008.std.err",
            "2012.diff",
            "2012.std.err",
            "2016.diff",
            "2016.std.err"
        )
    )

matrices[["SEX"]][["1A"]] <- mat
rm(mat)


row <- nlevels(raked[["2016"]][["1A"]]$variables$AGE8)
mat <- matrix(nrow = row, ncol = 8)

for (counter in 0:3) {
    year <- as.character(2004 + counter * 4)
    design <- raked[[year]][["1A"]]
    for (i in 1:row) {
        subdesign <- subset(design, AGE8 == levels(design$variables$AGE8)[i])
        imp.res <- svymean(~ PRES, subdesign)
        varcov <- vcov(imp.res)
        imp.res <- as.data.frame(imp.res)
        varcov <- as.data.frame(varcov)
        diff <- imp.res[1, 1] - imp.res[2, 1]
        serr <- imp.res[1, 2] + imp.res[2, 2] - 2 * varcov[1,2]
        mat[i, 2 * counter + 1] <- diff
        mat[i, 2 * counter + 2] <- serr
        rm(i, subdesign, imp.res, varcov, diff, serr)
    }
    rm(counter, year, design)
}
rm(row)

dimnames(mat) <-
    list(
        levels(raked[["2016"]][["1A"]]$variables$AGE8),
        c(
            "2004.diff",
            "2004.std.err",
            "2008.diff",
            "2008.std.err",
            "2012.diff",
            "2012.std.err",
            "2016.diff",
            "2016.std.err"
        )
    )

matrices[["AGE8"]][["1A"]] <- mat
rm(mat)


row <- nlevels(raked[["2016"]][["1A"]]$variables$RACE4)
mat <- matrix(nrow = row, ncol = 8)

for (counter in 0:3) {
    year <- as.character(2004 + counter * 4)
    design <- raked[[year]][["1A"]]
    for (i in 1:row) {
        subdesign <- subset(design, RACE4 == levels(design$variables$RACE4)[i])
        imp.res <- svymean(~ PRES, subdesign)
        varcov <- vcov(imp.res)
        imp.res <- as.data.frame(imp.res)
        varcov <- as.data.frame(varcov)
        diff <- imp.res[1, 1] - imp.res[2, 1]
        serr <- imp.res[1, 2] + imp.res[2, 2] - 2 * varcov[1,2]
        mat[i, 2 * counter + 1] <- diff
        mat[i, 2 * counter + 2] <- serr
        rm(i, subdesign, imp.res, varcov, diff, serr)
    }
    rm(counter, year, design)
}
rm(row)

dimnames(mat) <-
    list(
        levels(raked[["2016"]][["1A"]]$variables$RACE4),
        c(
            "2004.diff",
            "2004.std.err",
            "2008.diff",
            "2008.std.err",
            "2012.diff",
            "2012.std.err",
            "2016.diff",
            "2016.std.err"
        )
    )

matrices[["RACE4"]][["1A"]] <- mat
rm(mat)



row <- nlevels(raked[["2016"]][["1A"]]$variables$REGION)
mat <- matrix(nrow = row, ncol = 8)

for (counter in 0:3) {
    year <- as.character(2004 + counter * 4)
    design <- raked[[year]][["1A"]]
    for (i in 1:row) {
        subdesign <- subset(design, REGION == levels(design$variables$REGION)[i])
        imp.res <- svymean(~ PRES, subdesign)
        varcov <- vcov(imp.res)
        imp.res <- as.data.frame(imp.res)
        varcov <- as.data.frame(varcov)
        diff <- imp.res[1, 1] - imp.res[2, 1]
        serr <- imp.res[1, 2] + imp.res[2, 2] - 2 * varcov[1,2]
        mat[i, 2 * counter + 1] <- diff
        mat[i, 2 * counter + 2] <- serr
        rm(i, subdesign, imp.res, varcov, diff, serr)
    }
    rm(counter, year, design)
}
rm(row)

dimnames(mat) <-
    list(
        levels(raked[["2016"]][["1A"]]$variables$REGION),
        c(
            "2004.diff",
            "2004.std.err",
            "2008.diff",
            "2008.std.err",
            "2012.diff",
            "2012.std.err",
            "2016.diff",
            "2016.std.err"
        )
    )

matrices[["REGION"]][["1A"]] <- mat
rm(mat)


save(matrices, file = "Processed/Difference Matrices.Rdata")



