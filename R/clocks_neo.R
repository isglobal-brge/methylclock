# NEO chronological-age predictors (platform-specific, standardized input).

.register_neo <- function() {
    clock_register("NEOaPMA450K", target = "chronological",
        tissue = "buccal epithelium (preterm infants)",
        estimates = "Post-menstrual age (weeks)",
        resource = "coefNEOaPMA450K", platform = "450K",
        standardize = TRUE, citation = "graw2021neoage")
    clock_register("NEOaPNA450K", target = "chronological",
        tissue = "buccal epithelium (preterm infants)",
        estimates = "Postnatal age (weeks)",
        resource = "coefNEOaPNA450K", platform = "450K",
        standardize = TRUE, citation = "graw2021neoage")
    clock_register("NEOaPMAEPIC", target = "chronological",
        tissue = "buccal epithelium (preterm infants)",
        estimates = "Post-menstrual age (weeks)",
        resource = "coefNEOaPMAEPIC", platform = "EPIC",
        standardize = TRUE, citation = "graw2021neoage")
    clock_register("NEOaPNAEPIC", target = "chronological",
        tissue = "buccal epithelium (preterm infants)",
        estimates = "Postnatal age (weeks)",
        resource = "coefNEOaPNAEPIC", platform = "EPIC",
        standardize = TRUE, citation = "graw2021neoage")
    invisible(NULL)
}
