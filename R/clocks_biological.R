# Biological-state and pace clocks (2nd/3rd generation) plus the telomere clock.

.register_biological <- function() {
    clock_register("Levine", target = "phenotypic", generation = 2L,
        tissue = "whole blood",
        platform = "450K", resource = "coefLevine", citation = "levine2018")
    # INTERNAL: Levine is PhenoAge.

    clock_register("DunedinPACE", target = "pace", generation = 3L,
        tissue = "whole blood",
        platform = c("450K", "EPIC"), resource = "coefDunedinPACE",
        aux = "coefDunedinPACEGS", normalize = "quantile_gs",
        status = "license-risk",
        license = "GPL-3 + TruDiagnostic non-commercial",
        source = "github danbelsky/DunedinPACE", citation = "belsky2022")
    # INTERNAL: DunedinPACE -> D9 (keep+mark); do not vendor weights (sec 10).
    # INTERNAL: quantile-normalize to GS means (coefDunedinPACEGS), then linear.

    clock_register("TL", target = "telomere", generation = NA_integer_,
        tissue = "whole blood",
        estimates = "Telomere length (kb)",
        platform = "450K", resource = "coefTL", citation = "lu2019tl")
    invisible(NULL)
}
