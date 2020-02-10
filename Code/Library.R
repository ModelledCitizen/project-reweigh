message("Loading custom functions...")
cat("... target.fix")
# Subroutine of margin checking function that corrects 
# the supplied targets for missing strata
target.fix <- function(tab, target) {
    # across potential number of zeroes
    for (j in 1:ncol(tab)) {
        # through rows with j zeros
        for (i in which(rowSums(tab) == j)) {
            # get index of rows for logical vector
            k <- which(tab[i,])
            # sum the targets that need to be moved
            val <- sum(target[i, k])
            # set population margins to 0
            target[i, k] <- 0
            # then move the targets into the other levels
            target[i, ] <-
                target[i, ] + prop.table(target[i, ]) * val
            # any 0s errored into NAs return to 0
            target[is.na(target)] <- 0
        }
    }
    return(target)
}
cat(" ✓\n")

cat("...margin.gen")
# Create margins, checking for missing strata in the sample 
# and reassigning the appropriate targets to other areas so
# the math works out
margin.gen <-
    function(varset,
             method = NULL,
             samp = NULL,
             pop = cps[[yr]],
             ret = returns[[yr]],
             ext = exitpoll[[yr]]) {
        message("Margin Generation for Project Reweigh")
        cat("_____________________________________\n")
        cat("Checking input...\n")
        # perform a few checks before going ahead
        if (!exists("yr")) {
            stop("Year must exist in the environment if datasets are not specified.")
        }
        if (class(pop) == "data.frame") {
            message("✓ CPS data OK.")
        }
        if (class(ret) == "data.frame") {
            message("✓ Election returns OK.")
        }
        # if samp is not specified, draw from exit poll (method must be given)
        # if exit poll or samp are imputation list, select an appropriate frame
        if (is.null(samp)) {
            cat("Sample frame unspecified, using exit poll...\n")
            if (is.null(method)) {
                stop("Either sample frame or raking method must be specified.")
            }
            samp <- ext[[method]]
            if (class(samp) == "list") {
                cat("Exit poll has multiple imputations, selecting one at random...\n")
                samp <- samp[[sample(1:6, 1)]]
            } 
        } else if (class(samp) == "imputationList") {
            cat("Sample frame is imputation list, selecting one imputation at random...\n")
            samp <- samp[["imputations"]][[sample(1:6, 1)]]
        }
        if (class(samp) == "data.frame") {
            message("✓ Sample frame OK.")
        }
        cat("_____________________________________\n")
        cat("Identifying groups...\n")
        # process the input varset to extract groups and vars
        groups <- grep("GROUP", names(varset))
        group1 <- varset[groups[1]]
        group2 <- varset[groups[2]]
        vars1 <- varset[(groups[1] + 1):(groups[2] - 1)]
        vars2 <- varset[(groups[2] + 1):length(varset)]
        # Group 1 should always be the finer classification
        # But if it's not, we make it so
        if (nlevels(samp[,group1]) < nlevels(samp[,group2])) {
            temp <- group1
            group1 <- group2
            group2 <- temp
            temp <- vars1
            vars1 <- vars2
            vars2 <- temp
            rm(temp)
        }
        message("Group 1: ", group1, " (", nlevels(samp[,group1]), " levels)")
        for (var in vars1) {
            cat("    ", var, paste0("(", nlevels(samp[,var]), " levels)"), "\n")
        }
        message("Group 2: ", group2, " (", nlevels(samp[,var]), " levels)")
        for (var in vars2) {
            cat("    ", var, paste0("(", nlevels(samp[,var]), " levels)"),"\n")
        }
        # we create the empty list which will hold all of our margins
        margins <- list()
        cat("_____________________________________\n")
        cat(paste0("Working on first group (", group1, ")...\n"))
        # we assume for now that the smaller group is all CPS
        # but we check, just in case.
        # for each variable in the first set...
        for (var in vars1) {
            proc <- names(varset)[varset %in% var]
            cat(paste0(var, ": ", proc))
            if (proc == "CPS") {
                # calculate the initial population margin by first group
                margin <-
                    xtabs(paste("WEIGHT ~", group1, "+", var), data = pop)
                # convert to marginal proportions
                margin <- prop.table(margin, margin = 1)
                # multiply by vote counts within first group for real targets
                margin <-
                    margin * as.numeric(xtabs(paste("TOTAL ~", group1), data = ret))
                # for each level of the second group
                for (grp in levels(pop[, group2])) {
                    # make a subset indicator that is only that level
                    pop$subs <- pop[, group2] == grp
                    # calculate margins of population w/in group 2 by group 1
                    mgn <-
                        xtabs(paste("WEIGHT ~", group1, "+", var),
                              data = pop,
                              subset = subs)
                    # convert to marginal proportions
                    mgn <- prop.table(mgn, margin = 1)
                    # multiply by group1 vote counts
                    mgn <-
                        mgn * as.numeric(xtabs(paste("TOTAL ~", group1), data = ret))
                    # set missings to 0 so we can match them later
                    mgn[is.na(mgn)] <- 0
                    
                    # make a subset indicaor for the group2 level
                    samp$subs <- samp[, group2] == grp
                    # calculate sample margins on the subset
                    tab <-
                        xtabs(paste("~", group1, "+", var),
                              data = samp,
                              subset = subs)
                    # if an entire group1 is missing, set the margins to 0
                    # and move those votes to other group1s in the region
                    # apportioned by the existing ratio of votes
                    i <- which(rowSums(tab) == 0)
                    for (j in 1:nlevels(pop[, var])) {
                        # sum the real targets to be redistributed
                        val <- sum(mgn[i, j])
                        # set temporary population margins to 0
                        mgn[i, j] <- 0
                        # then move the targets into the other levels
                        mgn[, j] <-
                            mgn[, j] + prop.table(mgn[, j]) * val
                    }
                    # move the temporary margins to main list
                    margin[mgn != 0] <- mgn[mgn != 0]
                }
                # recreate sample margins and repeat but only for missing levels
                tab <- xtabs(paste("~", group1, "+", var), data = samp)
                tab <- tab == 0
                # across potential number of zeroes
                for (j in 1:nlevels(pop[, var])) {
                    # through rows with j zeros
                    for (i in which(rowSums(tab) == j)) {
                        # get index of rows for logical vector
                        k <- which(tab[i, ])
                        # sum the real targets to be redistributed
                        val <- sum(margin[i, k])
                        # set population margins to 0
                        margin[i, k] <- 0
                        # then move the targets into the other levels
                        margin[i,] <-
                            margin[i,] + prop.table(margin[i,]) * val
                        # any 0s errored into NAs return to 0 
                        margin[is.na(margin)] <- 0
                    }
                }
                # return to pop margin list
                margins[[var]] <- margin
            } else {
                cat(" ×\n")
                stop("Unknown margin type requested for ", var, ".\nGroup 1 variables must use CPS.")
            }
            cat(" ✓\n")
        }
        message("Group 1 OK.")
        cat("_____________________________________\n")
        cat(paste0("Working on second group (", group2, ")...\n"))
        # now for the second group...
        # many different margin types are possible
        for (var in vars2) {
            proc <- names(varset)[varset %in% var]
            cat(paste0(var, ": ", proc))
            if (proc == "CPS") {
                margin <-
                    xtabs(paste("WEIGHT ~", group2, "+", var), data = pop)
                margin <- prop.table(margin, margin = 1)
                margin <-
                    margin * as.numeric(xtabs(paste("TOTAL ~", group2), data = ret))
                margins[[var]] <- margin
            } else if (proc == "COUNTY") {
                margin <- xtabs(paste("TOTAL ~", group2, "+", var), data = ret)
                margins[[var]] <- margin
            } else if (proc == "RETURN") {
                margin <-
                    base::rbind(
                        Democrat = xtabs(paste("DEM ~", group2), data = ret),
                        Republican = xtabs(paste("REP ~", group2), data = ret),
                        Other = xtabs(paste("OTHER ~", group2), data = ret)
                    )
                names(dimnames(margin)) <- c("PRES", group2)
                margin <- as.table(t(margin))
                margins[[var]] <- margin
            } else {
                cat(" ×\n")
                stop("Unknown margin type requested for ", var, ".")
            }
            cat(" ✓\n")
        }
        message("Group 2 OK.\n")
        message("Margins generated successfully.")
        return(margins)
    }
cat(" ✓\n")

cat("...margin.check")
# function to check for inconsistencies between targets and strata
# in sample, particularly between imputations
# and to fix those inconsistencies systematically by
# adjusting targets or padding imputations.
margin.check <- function(varset, method, margins, ext = samp.frame) {
    message("Margin Repair and Imputation Salting")
    cat("_____________________________________\n")
    cat("Identifying groups & variables...\n")
    # put sample into a basic list format so no special behavior is required for different #'s of imputations
    if (method == 1) {
        ext <- list(ext)
    } else {
        ext <- ext[["imputations"]]
    }
    # find "group" in the names vector, keep numbers and labels
    gind <- grep("GROUP", names(varset))
    groups <- varset[gind]
    varlist <- list(ONE = varset[(gind[1] + 1):(gind[2] - 1)], 
                    TWO = varset[(gind[2] + 1):length(varset)])
    names(varlist) <- groups
    # loop through groups to report out
    for (i in 1:2) {
        message("Group ", i, ": ", groups[i], " (", nlevels(ext[[1]][, groups[i]]), " levels)")
        for (var in varlist[[groups[i]]]) {
            cat("        ", var, paste0("(", nlevels(ext[[1]][, var]), " levels)"), "\n")
        }
    }
    # create objects we'll be using to keep track of errors and store replacements
    replace <- vector("list", length = length(ext))
    replace <- lapply(replace, function(x) list())
    miss <- list()
    # main logic part one: loop through groups
    # first creating objects to compare
    for (group in groups) {
        cat("_____________________________________\n")
        cat("Starting", group, "group...\n")
        assign("vars", varlist[[group]])
        targets <- margins[vars]
        # create table of where strata = 0
        targets0 <- lapply(targets, function(x) x == 0)
        imp <- list()
        imp0 <- list()
        # for each imputation by index
        for (i in 1:length(ext)) {
            temp <- list()
            # for each variable in the group
            # extract the most common level of each variable
            for (var in vars) {
                x <- table(ext[[i]][[var]])
                replace[[i]][[var]] <- names(x[which(x == max(x))])
                temp[[var]] <-
                    xtabs(paste("~", group, "+", var), data = ext[[i]])
            }
            imp[[i]] <- temp
            # create table of where strata = 0
            imp0[[i]] <- lapply(temp, function(x) x == 0)
        }
        match <- list()
        # now for each variable in the group
        for (var in vars) {
            match[[var]] <- vector("logical", length(ext))
            # for each imputation by index
            for (i in 1:length(ext)) {
                # check if it matches the targets = 0
                if (identical(imp0[[i]][[var]], targets0[[var]])) {
                    match[[var]][[i]] <- FALSE
                } else {
                    match[[var]][[i]] <- TRUE
                }
            }
        }
        # once more for each variable in group
        # take results of the first loop and fix errors
        for (var in vars) {
            message("Variable: ", var)
            # if the number of errors matches imputations
            # use subroutine to match targets to imputations
            if (sum(match[[var]]) == length(match[[var]])) {
                cat("...all imputations share error.\n")
                cat("...adjusting targets!\n")
                k <- sample(1:length(imp), 1)
                margins[[var]] <- target.fix(imp0[[k]][[var]], margins[[var]])
            # or if there are errors but fewer than all
            # extract the levels that are absent and store them
            } else if (sum(match[[var]] != 0)) {
                cat("...only some imputations have errors.\n")
                cat("...saving for salting!\n")
                miss[[var]] <- vector("list", length = length(ext))
                # for each imputation by index
                for (i in 1:length(ext)) {
                    # if the imputation doesn't match targets
                    if (!identical(imp0[[i]][[var]], targets0[[var]])) {
                        miss[[var]] <- list()
                        mi <- which(imp0[[i]][[var]] != targets0[[var]], arr.ind = T)
                        miss[[var]][[group]] <- rownames(imp[[i]][[var]])[mi[, 1]]
                        miss[[var]][[var]] <- colnames(imp[[i]][[var]])[mi[, 2]]
                        cmp <- imp0[[i]][[var]] != targets0[[var]]
                        # plus update the targets to match
                        # but only if they're zero
                        # else the existing targets are fine
                        if (margins[[var]][cmp] == 0) {
                            margins[[var]][cmp] <- 1
                            cat("...and updating targets!\n")
                        }
                    }
                }
            } else {
                cat("...no error.\n")
            }
        }   
    }
    cat("_____________________________________\n")
    cat("Salting Sample...\n")
    # if there are any missing strata (group/level combos) stored
    # we need to generate a row with those strata
    if (length(miss) > 0) {
        # for each imputation by index
        for (i in 1:length(ext)) {
            message("Imputation ", i)
            cat("...binding supplemental row.\n")
            ro <- NULL
            # for each variable in the entire sample
            for (n in names(ext[[i]])) {
                # if there is a replacement value stored
                # which it will be for important variables
                if (!is.null(replace[[i]][[n]])) {
                    ro <- base::cbind(ro, replace[[i]][[n]])
                # else we make a replacement for others
                } else if (!n %in% groups) {
                    x <- table(ext[[i]][[n]])
                    x <- names(x[which(x == max(x))])[1]
                    ro <- base::cbind(ro, x)
                # or keep it NA if it is a group
                # (to be replaced next)
                } else {
                    ro <- base::cbind(ro, NA)
                }
            }
            # again for each variable in the entire sample
            for (n in names(ext[[i]])) {
                # if there is a specific replacement stored
                # for an empty stratum (group/level combo)
                if (!is.null(miss[[n]])) {
                    # put the group in the right place
                    m <- names(miss[[n]])[1]
                    p <- which(names(ext[[i]]) == m)
                    ro[p] <- miss[[n]][[m]]
                    # put the level in the right place
                    m <- names(miss[[n]])[2]
                    p <- which(names(ext[[i]]) == m)
                    ro[p] <- miss[[n]][[m]]
                    if (m != n) {
                        warning("m-n error! If you're getting this, something is very wrong.")
                    }
                }
            }
            # store completed row
            ro <- as.data.frame(ro)
            names(ro) <- names(ext[[i]])
            # replace the location-dependent variables
            # with appropriate ones, i.e. match region to state
            # and match cluster to the state
            # first if there's no state, we pick one from the area
            if (is.na(ro[[groups[1]]])) {
                idx <- as.character(ext[[i]][[groups[2]]]) == as.character(ro[[groups[2]]])
                x <- table(ext[[i]][[groups[1]]][idx])
                ro[[groups[1]]] <- names(x[which(x == max(x))])[1]
                x <- table(ext[[i]][["CLUSTER"]][idx])
                ro[["CLUSTER"]] <- names(x[which(x == max(x))])[1]
            }
            # then if there's no region, we use state to fill in
            if (is.na(ro[[groups[2]]])) {
                idx <- as.character(ext[[i]][[groups[1]]]) == as.character(ro[[groups[1]]])
                x <- table(ext[[i]][[groups[2]]][idx])
                ro[[groups[2]]] <- names(x[which(x == max(x))])[1]
                x <- table(ext[[i]][["CLUSTER"]][idx])
                ro[["CLUSTER"]] <- names(x[which(x == max(x))])[1]
                # this makes sure we don't upset zero-targets
                # since method has a lot of those
                x <- table(ext[[i]][["METHOD"]][idx])
                ro[["METHOD"]] <- names(x[which(x == max(x))])[1]
            }
            # add the row to the imputation
            ext[[i]] <- base::rbind(ext[[i]], ro)
        } 
    }
    else {
        cat("...no salt required.\n")
    }
    # return the edited objects to the main environment
    if (method == 1) {
        samp.frame <<- ext[[1]]
    } else {
        samp.frame <<- imputationList(ext)
    }
    pop.margins <<- margins
    message("\nAll operations successful.")
}
cat(" ✓\n")

cat("...new.vars")
# Add new variables to imputed exit poll data frames
new.vars <- function(x) {
    x$AGE8 <-
        factor(x$XAGE,
               labels = c("18-24",
                          "25-29",
                          "30-39",
                          "40-44",
                          "45-49",
                          "50-59",
                          "60-64",
                          "65+",
                          if (y < 2012) "65+"),
               ordered = TRUE)
    
    x$AGE4 <-
      factor(x$AGE8, labels = c(rep("18-29", 2), rep("30-44", 2), rep("45-54", 3), "65+"))
    
    x$AGE3 <-
        factor(x$AGE8,
               labels = c(rep("18-29", 2), rep("30-59", 4), rep("60+", 2)),
               ordered = TRUE)
    x$RACE4 <-
        factor(x$RACE, labels = c("White", "Black", "Hispanic", rep("Other", 2)))
    x$EDUC3 <-
        factor(
            x$EDUC,
            labels = c(if (y != 2016)
                "HSLess", "HSLess", "SomeCollege", rep("College", 2)),
            ordered = TRUE
        )
    x$RACEXED <- x$RACE4:x$EDUC3
    x$RACE2 <-
        factor(
            x$RACE4,
            levels = levels(x$RACE4),
            labels = c("White", rep("Nonwhite", 3))
        )
    x$EDUCWHITE <- x$EDUC3:x$RACE2
    return(x)
}
cat(" ✓\n")
message("Custom functions loaded.")