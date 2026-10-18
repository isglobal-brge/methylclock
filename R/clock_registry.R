# Declarative clock registry: one entry per clock. The estimation engine reads
# this registry and dispatches predictions from it.
#
# INTERNAL: replaces the four parallel `if (n %in% method)` dispatch chains of
# INTERNAL: the legacy package (DESIGN.md sec 8). Adding a clock is a single
# INTERNAL: clock_register() call instead of ~18 edits across 6 files.

# Private registry store. Populated by clock_register().
.clock_env <- new.env(parent = emptyenv())

# Controlled vocabularies -----------------------------------------------------

#' Supported prediction engines
#'
#' The predictor families a registered clock can use. A clock's \code{predictor}
#' must be one of these values.
#'
#' \describe{
#'   \item{linear}{intercept plus a weighted sum of features (most clocks).}
#'   \item{pc}{project features onto a PCA rotation, then a linear model on the
#'     principal components.}
#'   \item{surrogate}{two-stage model (methylation surrogates then a combining
#'     model); typically requires chronological age and sex.}
#'   \item{counter}{a mean or quantile over a defined set of CpGs.}
#'   \item{iterative}{a counter estimated per sample by numerical optimisation.}
#'   \item{nn}{a neural network.}
#'   \item{classifier}{a model with categorical output.}
#' }
#' @return A character vector of the supported engine names.
#' @examples
#' clock_predictors
#' @export
clock_predictors <- c(
    "linear", "pc", "surrogate", "counter", "iterative", "nn", "classifier"
)

#' Supported biological targets
#'
#' The quantity a clock estimates. A clock's \code{target} must be one of these.
#' @return A character vector of the supported target names.
#' @examples
#' clock_targets
#' @export
clock_targets <- c(
    "chronological", "phenotypic", "gestational", "pace", "telomere",
    "mitotic", "causal", "postnatal", "trait", "risk"
)

#' Availability status labels
#'
#' A coarse status for a registered clock. \code{"ok"} is fully available;
#' \code{"license-risk"} is usable but carries licensing constraints on its
#' weights; \code{"out-of-scope"} does not apply to the package's target tissue;
#' \code{"not-implementable"} has no usable published model; \code{"todo"} is
#' planned.
#' @return A character vector of the supported status labels.
#' @examples
#' clock_statuses
#' @export
clock_statuses <- c("ok", "license-risk", "out-of-scope", "not-implementable",
                    "todo")

# Registration ----------------------------------------------------------------

# A counter clock's aggregation is either "mean" or an upper quantile given as
# list(fun = "quantile", p = <0..1>). Rejected early so a bad spec fails at
# registration, not at estimation.
.validate_agg <- function(agg) {
    if (identical(agg, "mean")) return(invisible(TRUE))
    ok <- is.list(agg) && identical(agg$fun, "quantile") &&
        is.numeric(agg$p) && length(agg$p) == 1L &&
        !is.na(agg$p) && agg$p >= 0 && agg$p <= 1
    if (ok) return(invisible(TRUE))
    stop('`agg` must be "mean" or list(fun = "quantile", p = <0..1>).')
}

#' Register a clock
#'
#' Adds (or replaces) one clock in the package registry. A registration is a
#' single declarative description of a clock; the estimation engine reads it to
#' compute the clock's value.
#'
#' @param name Clock name (unique key), e.g. \code{"Horvath"}.
#' @param target Biological target; one of \code{\link{clock_targets}}.
#' @param predictor Prediction engine; one of \code{\link{clock_predictors}}.
#'   Defaults to \code{"linear"}.
#' @param generation Clock generation (e.g. 1, 2, 3) or \code{NA}.
#' @param platform Character vector of the clock's native arrays --- the
#'   platform(s) it was built and validated on, e.g. \code{c("450K", "EPIC")}.
#' @param compatible Character vector of additional arrays the clock can be used
#'   with through probe conversion or adaptation, i.e. not its native platform.
#' @param tissue The tissue the clock was trained on (e.g. \code{"whole
#'   blood"}, \code{"buccal epithelium (children)"}, \code{"placenta"},
#'   \code{"multi-tissue"}): a clock reads best the tissue it came from.
#'   Empty by default. Selection and estimation treat these as non-native.
#' @param estimates Optional description of the quantity the clock
#'   estimates, with its units (e.g. \code{"Post-menstrual age (weeks)"}).
#'   When \code{NA}, catalogue displays fall back to a generic label for the
#'   clock's target family.
#' @param resource Resource identifier (resolved by \code{\link{mcd_resource}})
#'   holding the coefficients, e.g. \code{"coefHorvath"}.
#' @param aux Character vector of auxiliary resource identifiers (e.g.
#'   normalization means, a PCA rotation, a cell-type reference), or \code{NULL}.
#' @param coef_col Name of the coefficient column in the resource. Default
#'   \code{"CoefficientTraining"}.
#' @param intercept Logical: is the first coefficient row the intercept?
#' @param standardize Logical: z-score each sample across the CpGs of the input
#'   matrix before applying the coefficients (as some elastic-net clocks
#'   require). Default \code{FALSE}.
#' @param normalize Optional input normalization applied before the
#'   coefficients. \code{"quantile_gs"} quantile-normalizes each sample to a
#'   gold-standard reference distribution taken from the clock's \code{aux}
#'   resource. Default \code{NA} (none).
#' @param agg For \code{"counter"} clocks, how the CpG set is aggregated per
#'   sample: \code{"mean"} (the default) or \code{list(fun = "quantile", p =
#'   <0..1>)} for an upper-quantile summary. Ignored by other engines.
#' @param transform Function applied to the raw prediction (e.g. an
#'   anti-transform). Default \code{identity}.
#' @param inputs Character vector of inputs required beyond the beta matrix
#'   (e.g. \code{c("age", "sex")}). Default none.
#' @param feature_type Feature space of the predictor. Default \code{"cpg"};
#'   kept configurable so the same engine can operate on non-CpG features.
#' @param output_type One of \code{"numeric"}, \code{"probability"},
#'   \code{"category"}. Default \code{"numeric"}.
#' @param license Short license/provenance tag for the weights. Default
#'   \code{NA}.
#' @param source Where the weights come from (URL, package, or resource id).
#' @param citation Short citation key.
#' @param status Availability status; one of \code{\link{clock_statuses}}.
#'   Default \code{"ok"}.
#' @param notes Free-text notes.
#'
#' @return Invisibly, the registry entry (a list of class \code{clock_entry}).
#' @seealso \code{\link{clock_list}}, \code{\link{clock_info}},
#'   \code{\link{mcd_resource}}
#' @examples
#' clock_register("MyClock", target = "chronological", resource = "coefMyClock")
#' clock_info("MyClock")
#' @export
clock_register <- function(name,
                           target,
                           predictor = "linear",
                           generation = NA_integer_,
                           platform = character(0),
                           compatible = character(0),
                           tissue = NA_character_,
                           estimates = NA_character_,
                           resource = NULL,
                           aux = NULL,
                           coef_col = "CoefficientTraining",
                           intercept = TRUE,
                           standardize = FALSE,
                           normalize = NA_character_,
                           agg = "mean",
                           transform = identity,
                           inputs = character(0),
                           feature_type = "cpg",
                           output_type = "numeric",
                           license = NA_character_,
                           source = NA_character_,
                           citation = NA_character_,
                           status = "ok",
                           notes = NA_character_) {
    if (!is.character(name) || length(name) != 1L || is.na(name) || !nzchar(name))
        stop("`name` must be a single non-empty string.")
    predictor <- match.arg(predictor, clock_predictors)
    target <- match.arg(target, clock_targets)
    output_type <- match.arg(output_type, c("numeric", "probability", "category"))
    status <- match.arg(status, clock_statuses)
    if (!is.function(transform))
        stop("`transform` must be a function.")
    .validate_agg(agg)

    entry <- list(
        name = name, target = target, predictor = predictor,
        generation = as.integer(generation), platform = platform,
        compatible = compatible, tissue = as.character(tissue)[1],
        estimates = as.character(estimates)[1],
        resource = resource, aux = aux, coef_col = coef_col,
        intercept = isTRUE(intercept), standardize = isTRUE(standardize),
        normalize = normalize, agg = agg, transform = transform,
        inputs = inputs, feature_type = feature_type,
        output_type = output_type, license = license, source = source,
        citation = citation, status = status, notes = notes
    )
    class(entry) <- "clock_entry"
    assign(name, entry, envir = .clock_env)
    invisible(entry)
}

# Accessors -------------------------------------------------------------------

#' The clock registry
#'
#' @return A named list of registry entries (objects of class
#'   \code{clock_entry}), ordered by name.
#' @seealso \code{\link{clock_register}}, \code{\link{clock_list}}
#' @examples
#' reg <- clock_registry()
#' length(reg)
#' names(reg)[seq_len(5)]
#' reg[["Horvath"]]
#' @export
clock_registry <- function() {
    out <- mget(ls(.clock_env), envir = .clock_env)
    out[order(names(out))]
}

#' Look up one clock's registry entry
#'
#' @param name Clock name.
#' @return The registry entry (class \code{clock_entry}); errors if the clock is
#'   not registered.
#' @seealso \code{\link{clock_list}}
#' @examples
#' clock_info("Horvath")
#' @export
clock_info <- function(name) {
    if (!exists(name, envir = .clock_env, inherits = FALSE))
        stop(sprintf("Unknown clock '%s'. See clock_list().", name))
    get(name, envir = .clock_env)
}

#' List and filter registered clocks
#'
#' @param target Optional filter by biological target.
#' @param generation Optional filter by generation.
#' @param predictor Optional filter by prediction engine.
#' @param platform Optional filter by native platform: keeps clocks whose native
#'   platform includes any of the given arrays (e.g. \code{"450K"}).
#' @param status Optional filter by availability status.
#' @return A data frame with one row per matching clock.
#' @seealso \code{\link{clock_info}}
#' @examples
#' clock_list(target = "gestational")
#' clock_list(platform = "450K")
#' @export
clock_list <- function(target = NULL, generation = NULL,
                       predictor = NULL, platform = NULL, status = NULL) {
    reg <- clock_registry()
    if (length(reg) == 0L)
        return(data.frame())
    if (!is.null(platform))
        reg <- reg[vapply(reg, function(e) any(platform %in% e$platform),
                          logical(1))]
    if (length(reg) == 0L)
        return(data.frame())
    df <- do.call(rbind, lapply(reg, function(e) data.frame(
        name = e$name, target = e$target, generation = e$generation,
        predictor = e$predictor, platform = paste(e$platform, collapse = "/"),
        compatible = paste(e$compatible, collapse = "/"),
        status = e$status, stringsAsFactors = FALSE
    )))
    rownames(df) <- NULL
    if (!is.null(target))     df <- df[df$target == target, , drop = FALSE]
    if (!is.null(generation)) df <- df[df$generation %in% generation, , drop = FALSE]
    if (!is.null(predictor))  df <- df[df$predictor == predictor, , drop = FALSE]
    if (!is.null(status))     df <- df[df$status == status, , drop = FALSE]
    df
}

#' Clear the clock registry
#'
#' Removes all registered clocks. Mainly useful in tests, or when building a
#' registry from scratch: the built-in clocks are registered when the package
#' is loaded, so save them first if you want them back.
#' @return Invisibly \code{NULL}.
#' @seealso \code{\link{clock_register}}, \code{\link{clock_registry}}
#' @examples
#' saved <- clock_registry()
#' clock_reset()
#' length(clock_registry())
#' # put the built-in clocks back
#' for (entry in saved) do.call(clock_register, unclass(entry))
#' length(clock_registry())
#' @export
clock_reset <- function() {
    rm(list = ls(.clock_env), envir = .clock_env)
    invisible(NULL)
}

#' @export
print.clock_entry <- function(x, ...) {
    cat(sprintf("<clock> %s  [%s / gen %s / %s]\n",
                x$name, x$target,
                ifelse(is.na(x$generation), "-", x$generation), x$predictor))
    cat(sprintf("  resource: %s%s\n", x$resource %||% "-",
                if (length(x$aux)) sprintf(" (+aux: %s)", paste(x$aux, collapse = ", ")) else ""))
    cat(sprintf("  platform: %s%s\n",
                if (length(x$platform)) paste(x$platform, collapse = "/") else "-",
                if (length(x$compatible))
                    sprintf(" (compatible: %s)", paste(x$compatible, collapse = "/"))
                else ""))
    if (length(x$inputs))
        cat(sprintf("  needs   : betas + %s\n", paste(x$inputs, collapse = ", ")))
    cat(sprintf("  status  : %s%s\n", x$status,
                if (!is.na(x$license)) sprintf("  (license: %s)", x$license) else ""))
    invisible(x)
}

# Null-coalescing helper.
`%||%` <- function(a, b) if (is.null(a) || length(a) == 0L) b else a
