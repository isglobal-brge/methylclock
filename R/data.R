#' Example clock estimates for a public blood cohort
#'
#' Per-sample epigenetic clock estimates for a public whole-blood methylation
#' series with known chronological age (GEO accession GSE40279, 656 adult blood
#' samples aged 19--101). The raw methylation matrix is not shipped; this table
#' holds only the computed estimates, chronological age and sex, which is what
#' the plotting functions need. It lets the vignette and the plot examples run
#' on real data without downloading anything.
#'
#' @usage data(methylclock_demo)
#' @format A data frame with 656 rows (samples) and columns:
#' \describe{
#'   \item{id}{Sample identifier.}
#'   \item{age}{Chronological age in years.}
#'   \item{sex}{Reported sex, a factor with levels \code{F} and \code{M}.}
#'   \item{ethnicity}{Reported ethnicity, a factor with levels
#'     \code{Caucasian} (426, "Caucasian - European" in the source) and
#'     \code{Hispanic} (230, "Hispanic - Mexican"). It lets the cohort be
#'     used as two population-specific references in
#'     \code{\link{plotReferenceRange}}.}
#'   \item{Horvath, Hannum, Levine, skinHorvath, BLUP, EN, BNN, AltumAge, Lin,
#'     VidalBralo, PedBE, Wu}{Estimated age in years from each clock.}
#'   \item{TL}{Estimated telomere length.}
#' }
#' @source Hannum et al. (2013), GEO accession
#'   \url{https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=GSE40279}.
#' @examples
#' data(methylclock_demo)
#' plotDNAmAge(methylclock_demo, age = methylclock_demo$age)
"methylclock_demo"

#' Clock estimates on the shipped reference cohorts
#'
#' Per-sample clock estimates, chronological age and sex for public blood
#' cohorts shipped as selectable references for
#' \code{\link{plotReferenceRange}} (the raw methylation is not shipped).
#' Each is a single, specific cohort and its bands describe it alone:
#' \code{Swedish (GSE87571)}, 729 adults aged 14--94 from a population-based
#' cohort recorded as \code{disease state: normal} (3 source samples ship no
#' age and are excluded); \code{GENOA (GSE210255)}, 1394 African American
#' adults recruited through hypertensive sibships (enriched for hypertension,
#' with related samples); \code{Han Chinese controls (GSE116379)}, the 79
#' non-schizophrenia controls, all aged 45--51, of a schizophrenia and
#' prenatal-famine study in Changchun; and \code{DRC mothers (GSE224363)},
#' 97 mothers aged 14--42 sampled around delivery in Goma, DR Congo, in a
#' cohort with substantial trauma exposure.
#'
#' @usage data(methylclock_references)
#' @format A data frame with 2299 rows (samples) and columns \code{cohort}
#'   (factor), \code{id}, \code{age}, \code{sex}, and one numeric column per
#'   computed clock.
#' @source GEO accessions
#'   \url{https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=GSE87571},
#'   \url{https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=GSE210255},
#'   \url{https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=GSE116379} and
#'   \url{https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=GSE224363}.
#' @examples
#' data(methylclock_references)
#' table(methylclock_references$cohort)
"methylclock_references"

#' A longitudinal example: children sampled repeatedly from birth to age 3
#'
#' Per-sample clock estimates for a public cohort in which the same children
#' were sampled at several ages --- the shape the longitudinal tools
#' (\code{\link{clockTrajectories}}, \code{\link{trajectoryRates}},
#' \code{\link{plotTrajectories}}) work on. 67 children from Goma, DR Congo,
#' followed from birth to about age 3 (venous blood, EPIC; GEO accession
#' GSE224573), 164 observations in all, technical replicates flagged rather
#' than removed. The cohort carries substantial peripartum trauma exposure,
#' so it illustrates the mechanics of longitudinal analysis; it does not
#' stand for children in general.
#'
#' @usage data(methylclock_longitudinal)
#' @format A data frame with 164 rows (observations) and columns \code{id},
#'   \code{subject} (the child), \code{age} (decimal age at the visit),
#'   \code{sex}, \code{replicate}, and one numeric column per computed clock.
#' @source GEO accession
#'   \url{https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=GSE224573}.
#' @examples
#' data(methylclock_longitudinal)
#' with(methylclock_longitudinal, table(table(subject)))
"methylclock_longitudinal"

#' Clock estimates on two independent validation cohorts
#'
#' Per-sample clock estimates for two public blood cohorts that were not used to
#' train these clocks, so they read the clocks' accuracy out of sample: an adult
#' cohort and a paediatric one. It is what the vignette's validation section runs
#' on. As with \code{\link{methylclock_demo}}, only the estimates and age/sex are
#' shipped, not the methylation matrices.
#'
#' @usage data(methylclock_validation)
#' @format A data frame with 929 rows (samples) and columns:
#' \describe{
#'   \item{cohort}{Which cohort the sample belongs to, a factor:
#'     \code{"adult (GSE132203)"} (795 adults, whole blood, EPIC) or
#'     \code{"pediatric (GSE36054)"} (134 healthy children aged 1--17, blood,
#'     450K).}
#'   \item{id}{Sample identifier.}
#'   \item{age}{Chronological age in years.}
#'   \item{sex}{Reported sex, a factor with levels \code{F} and \code{M} (may be
#'     \code{NA} for the paediatric cohort).}
#'   \item{Horvath, Hannum, Levine, skinHorvath, BLUP, EN, AltumAge, Lin, PedBE,
#'     Wu}{Estimated age in years from each clock.}
#' }
#' @source Grady Trauma Project, GEO accession
#'   \url{https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=GSE132203}; and
#'   Alisch et al. (2012), GEO accession
#'   \url{https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=GSE36054}.
#' @examples
#' data(methylclock_validation)
#' adult <- subset(methylclock_validation, cohort == "adult (GSE132203)")
#' plotDNAmAge(adult, age = adult$age, clocks = c("Horvath", "BLUP"))
"methylclock_validation"

#' Catalogue of the clocks in the package
#'
#' A one-row-per-clock overview meant to be read first: what each clock
#' estimates, which arrays it was built for, how many CpGs it uses, and a short
#' reference to the paper. It is a frozen snapshot of the registry (regenerated
#' when clocks are added), so it can be displayed without resolving any
#' coefficients. Whether a clock can be computed on a given dataset is decided
#' by coverage of the clock's CpGs in the input, not by the array label: a
#' clock is computed whenever enough of its CpGs are present. For the live
#' registry --- to filter or query programmatically --- use
#' \code{\link{clock_list}}.
#'
#' @usage data(clock_catalog)
#' @format A data frame with one row per clock and columns:
#' \describe{
#'   \item{clock}{Clock name (pass to \code{methylclock(clocks = )}).}
#'   \item{estimates}{What the clock reports, with units (e.g. "Age (years)").}
#'   \item{tissue}{The tissue the clock was trained on (whole blood, cord
#'     blood, placenta, buccal, skin, multi-tissue...): a clock reads best
#'     the tissue it came from.}
#'   \item{arrays}{Illumina platforms the clock was built and validated on
#'     (its native platforms). This does not restrict where the clock can be
#'     computed.}
#'   \item{n_cpgs}{Number of CpGs the clock uses.}
#'   \item{generation}{Clock generation, where defined.}
#'   \item{target}{The registry target family the clock belongs to.}
#'   \item{citation}{Citation key for the clock's paper.}
#'   \item{reference}{Short human-readable reference (author and year).}
#' }
#' @examples
#' data(clock_catalog)
#' clock_catalog[clock_catalog$target == "chronological",
#'               c("clock", "estimates", "arrays", "n_cpgs", "reference")]
"clock_catalog"

#' Smoking predictor scores with smoking status
#'
#' Estimates for a public whole-blood cohort with recorded smoking status (464
#' adults, 450K), used to show a non-age (trait) clock at work: the methylation
#' smoking predictor separates never, former and current smokers. As with the
#' other bundled data, only the estimates and phenotype are shipped, not the
#' methylation matrix.
#'
#' @usage data(methylclock_smoking)
#' @format A data frame with 464 rows (samples) and columns:
#' \describe{
#'   \item{id}{Sample identifier.}
#'   \item{smoking}{Smoking status, a factor with levels \code{never},
#'     \code{former} and \code{current}.}
#'   \item{age}{Chronological age in years.}
#'   \item{sex}{Reported sex (\code{F}/\code{M}).}
#'   \item{McCartney.Smoking}{The methylation smoking-predictor score.}
#'   \item{Horvath, Hannum}{Estimated age in years, for context.}
#' }
#' @source Tsaprouni et al. (2014), GEO accession
#'   \url{https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=GSE50660}.
#' @examples
#' data(methylclock_smoking)
#' tapply(methylclock_smoking$McCartney.Smoking,
#'        methylclock_smoking$smoking, median)
"methylclock_smoking"

#' Clock estimates with blood cell proportions
#'
#' Clock estimates together with estimated blood cell proportions for an adult
#' whole-blood cohort (795 samples, EPIC), so the intrinsic (cell-adjusted) age
#' acceleration can be computed live. The cell proportions were estimated with
#' \code{\link{cellCounts}} from the methylation matrix (not shipped).
#'
#' @usage data(methylclock_cells)
#' @format A data frame with 795 rows (samples) and columns:
#' \describe{
#'   \item{id}{Sample identifier.}
#'   \item{age}{Chronological age in years.}
#'   \item{sex}{Reported sex (\code{F}/\code{M}).}
#'   \item{Horvath, Hannum, Levine, skinHorvath, EN}{Estimated age in years.}
#'   \item{Bcell, CD4T, CD8T, Eos, Mono, Neu, NK}{Estimated proportion of each
#'     blood cell type.}
#' }
#' @source Grady Trauma Project, GEO accession
#'   \url{https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=GSE132203}.
#' @examples
#' data(methylclock_cells)
#' cells <- c("Bcell", "CD4T", "CD8T", "Eos", "Mono", "Neu", "NK")
#' head(ageAcceleration(methylclock_cells, age = methylclock_cells$age,
#'                      cell_counts = as.matrix(methylclock_cells[, cells])))
"methylclock_cells"

#' Example methylation beta matrix
#'
#' A small, complete methylation beta matrix (real data) provided so the
#' vignette can demonstrate the \code{impute=} options on an actual matrix:
#' compute the clocks on the full data, blank out a fraction of the values, and
#' compare mean and K-nearest-neighbour imputation against the truth. Unlike the
#' other bundled objects, this one is the model *input* (CpGs and samples), not
#' the estimates. It is a subset of the example matrix shipped with earlier
#' versions of the package, restricted to the CpGs of the clocks it fully covers
#' (\code{Horvath}, \code{Levine}, \code{Wu}) to keep it small and let those
#' clocks run without missing CpGs.
#'
#' @usage data(methylclock_betas)
#' @format A numeric matrix with 919 rows (CpGs, as row names) and 16 columns
#'   (samples). Values are methylation betas in \[0, 1]; there are no missing
#'   values.
#' @source Subset of \code{MethylationDataExample55} from earlier releases of
#'   this package; a public human methylation series.
#' @examples
#' data(methylclock_betas)
#' methylclock_betas[1:3, 1:3]
#' # complete-data estimates, then the effect of imputing blanked-out values
#' truth <- methylclock(methylclock_betas,
#'                      clocks = c("Horvath", "Levine", "Wu"))
"methylclock_betas"
