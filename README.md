# Project Reweigh
Identifying a new weighting scheme for the National Election Pool exit poll.

[Click here for the GitHub repo.](https://github.com/ModelledCitizen/project-reweigh)

## Description
The National Election Poll is a collaborative effort led by major news agencies and conducted by Edison Research. Working under the direction of Prof. Marc Meredith (UPenn PSCI; NBC Decision Desk) with the support of Prof. John Lapinksi (UPenn PSCI & Fox-Fels; Director of Elections Unit, NBC News), I used historical data and national benchmarks to evaluate the performance of a new weighting scheme in matching real election returns. [Read a full description of the project here.](https://www.davidazizi.org/project-reweigh/Project%20Summary/Summary.pdf)

## Results
[This set of tables](https://www.davidazizi.org/project-reweigh/Results.html) allows for a comparison between the unweighted data and the weighted data with or without imputations. As a result of this project, the NEP is now weighted using education.

## Notes
- This was my first major project, and I made a lot of the coding decisions without awareness of best-practices. I hope the code is sufficiently commented to be understandable.
- The `Main.R` file in the `Code` folder runs through every operation needed to actually process the data. The `Results.Rmd` file is where the HTML tables are generated.
- Unfortunately, I am not allowed to publish the exit poll data -- however, some years are available on Roper iPoll. I also cannot publish the raked designs which include the exit poll data. However, I have included the output tables that are an aggregate of the individual data. I have also included all of the public data sources, excepting the large CPS `.dat` files which can be downloaded separately below.

## Data Access
- Please email to request access to data files.

