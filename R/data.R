#' CpGs list
#'
#' CpGs list
#'
#' @docType data
#'
#' @usage data(cpgs.bn)
#'
#' @format Character array with 431 CpGs
#'
"cpgs.bn"


#' Hannum clock coefficients
#'
#' A dataset containing the coefficients and CpGs information from Hannum clock
#'
#' @docType data
#'
#' @usage data(coefHannum)
#'
#' @format A data frame with 71 rows and 6 variables
#' @references \url{https://doi.org/10.1016/j.molcel.2012.10.016}
#' \describe{
#'   \item{CpGMarker}{CpGs id}
#'   \item{Chrom}{Chromosome }
#'   \item{Pos}{Position }
#'   \item{Genes}{Genes}
#'   \item{CpGIsland }{ Is CpG Island }
#'   \item{CoefficientTraining}{Coefficient from training }
#' }
"coefHannum"


#' Horvath clock coefficients
#'
#' A dataset containing the coefficients and CpGs information from Horvath clock
#'
#' @docType data
#'
#' @usage data(coefHorvath)
#'
#' @format A data frame with 354 CpGs and 23 variables
#' @references \url{https://doi.org/10.1371/journal.pone.0014821}
#' \describe{
#'   \item{CpGMarker}{CpGs id}
#'   \item{CoefficientTraining}{Coefficient from training }
#'   \item{CoefficientTrainingShrunk}
#'   \item{varByCpG}
#'   \item{minByCpG}
#'   \item{maxByCpG}
#'   \item{medianByCpG}
#'   \item{medianByCpGYoung}
#'   \item{medianByCpGOld}
#'   \item{Gene_ID}
#'   \item{GenomeBuild}
#'   \item{Chr}{Chromosome }
#'   \item{MapInfo}
#'   \item{SourceVersion}
#'   \item{TSS_Coordinate}
#'   \item{Gene_Strand}
#'   \item{Symbol}
#'   \item{Synonym}
#'   \item{Accession}
#'   \item{GID}
#'   \item{Annotation}
#'   \item{Product}
#'   \item{Marginal.Age.Relationship}
#' }
"coefHorvath"


#' Knight Gestational Age clock coefficients
#'
#' A dataset containing the coefficients and CpGs information from Knight GA clock
#'
#' @docType data
#'
#' @usage data(coefKnightGA)
#' @references \url{https://dx.doi.org/10.1186\%2Fs13059-016-1068-z}
#' @format A data frame with 149 CpGs and 2 variables
#' \describe{
#'   \item{CpGMarker}{CpGs id}
#'   \item{CoefficientTraining}{Training Coefficient}
#' }
"coefKnightGA"


#' Lee Gestational Age clock coefficients
#'
#' A dataset containing the coefficients and CpGs information from Lee GA clock
#'
#' @docType data
#'
#' @usage data(coefLeeGA)
#' @references \url{https://dx.doi.org/10.18632\%2Faging.102049}
#' @format A data frame with 1126 CpGs and 5 variables
#' \describe{
#'   \item{CpGMarker}{CpGs id}
#'   \item{CoefficientTraining}{RPC Coefficient }
#'   \item{Coefficient_CPC}{}
#'   \item{Coefficient_refined_RPC}{ }
#'   \item{Coefficient_sex_classifier}{}
#' }
"coefLeeGA"


#' Levine clock coefficients
#'
#' A dataset containing the coefficients and CpGs information from Levine clock
#'
#' @docType data
#'
#' @usage data(coefLevine)
#'
#' @format A data frame with 514 CpGs and 9 variables
#' @references \url{https://dx.doi.org/10.18632\%2Faging.101414}
#' \describe{
#'   \item{CpGMarker}{CpGs id}
#'   \item{CoefficientTraining}{Training Coefficient}
#'   \item{Chromosome}{Chromosome}
#'   \item{Map.Info}{}
#'   \item{Gene.Symbol}{Gene symbol}
#'   \item{Entrez.ID}{Entrez ID}
#'   \item{Univariate.Age.Correlation}{}
#'   \item{Horvath.Overlap}{}
#'   \item{Hannum.Overlap}{}
#' }
"coefLevine"


#' Mayne Gestational Age clock coefficients
#'
#' A dataset containing the coefficients and CpGs information from Mayne GA clock
#'
#' @docType data
#'
#' @usage data(coefMayneGA)
#' @references \url{https://dx.doi.org/10.2217\%2Fepi-2016-0103}
#' @format A data frame with 1126 CpGs and 5 variables
#' \describe{
#'   \item{CpGMarker}{CpGs id}
#'   \item{CoefficientTraining}{Training Coefficient }
#'   \item{Gene}{Gene}
#'   \item{Chromosome}{Chromosome }
#'   \item{Position}{Position}
#'   \item{Correlation.with.GA}{}
#' }
"coefMayneGA"



#' Skin clock coefficients
#'
#' A dataset containing the coefficients and CpGs information from Skin clock
#'
#' @docType data
#'
#' @usage data(coefSkin)
#' @format A data frame with 392 CpGs and 2 variables
#' \describe{
#'   \item{CpGMarker}{CpGs id}
#'   \item{CoefficientTraining}{Training Coefficient }
#' }
"coefSkin"


#' Telomere Length clock coefficients
#'
#' A dataset containing the coefficients and CpGs information from Telomere Length clock
#'
#' @docType data
#'
#' @usage data(coefTL)
#'
#' @format A data frame with 141 CpGs and 22 variables
#' @references \url{https://dx.doi.org/10.18632\%2Faging.102173}
#' \describe{
#'   \item{CpGMarker}{CpGs id}
#'   \item{CoefficientTraining}{Training Coefficient}
#'   \item{CHR}{Chromosome}
#'   \item{bp19}{}
#'   \item{SourceSeq}{}
#'   \item{Strand}{}
#'   \item{Probe.SNP.based.on.Illumina.annotation}{}
#'   \item{Gene}{}
#'   \item{location}{}
#'   \item{Relation.to..CpGIsland}{}
#'   \item{Probe.SNP.based.on.1000.Genome}{}
#'   \item{SNP}{}
#'   \item{CHR.SNP}{}
#'   \item{bp.SNP}{}
#'   \item{ALLELE}{}
#'   \item{BSGS.P}{}
#'   \item{BSGS.EFFECT}{}
#'   \item{BSGS.H2}{}
#'   \item{LBC.P}{}
#'   \item{LBC.EFFECT}{}
#'   \item{LBC.R2}{}
#'   \item{mQTL}{}
#' }
"coefTL"