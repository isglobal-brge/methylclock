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
#'   \item{Chrom}{Chromosome}
#'   \item{Pos}{Position}
#'   \item{Genes}{Genes}
#'   \item{CpGIsland }{Is CpG Island}
#'   \item{CoefficientTraining}{Coefficient from training }
#' }
#' @examples
#' data(coefHannum)
"coefHannum"


#' @title Horvath clock coefficients
#'
#' @description A dataset containing the coefficients and CpGs information from Horvath clock
#'
#' @name coefHorvath
#' @docType data
#'
#' @usage data(coefHorvath)
#' @references \url{https://doi.org/10.1371/journal.pone.0014821}
#' @format A data frame with 354 CpGs and 23 variables :
#' \describe{
#'   \item{CpGmarker}{CpGs id}
#'   \item{CoefficientTraining}{Coefficient from training }
#'   \item{CoefficientTrainingShrunk}{Coefficient Training Shrunk}
#'   \item{varByCpG}{var by CpG}
#'   \item{minByCpG}{min by CpG}
#'   \item{maxByCpG}{max by CpG}
#'   \item{medianByCpG}{median by CpG}
#'   \item{medianByCpGYoung}{median by CpG young}
#'   \item{medianByCpGOld}{median by CpG old}
#'   \item{Gene_ID}{Gene_ID}
#'   \item{GenomeBuild}{Genome Build}
#'   \item{Chr}{Chromosome}
#'   \item{MapInfo}{Map Info}
#'   \item{SourceVersion}{Source Version}
#'   \item{TSS_Coordinate}{TSS_Coordinate}
#'   \item{Gene_Strand}{Gene_Strand}
#'   \item{Symbol}{Symbol}
#'   \item{Synonym}{Synonym}
#'   \item{Accession}{Accession}
#'   \item{GID}{GID}
#'   \item{Annotation}{Annotation}
#'   \item{Product}{Product}
#'   \item{Marginal.Age.Relationship}{Marginal age relationship}
#' }
#' @examples
#' data(coefHorvath)
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
#'   \item{CpGmarker}{CpGs id}
#'   \item{CoefficientTraining}{Training Coefficient}
#' }
#' @examples
#' data(coefKnightGA)
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
#'   \item{Coefficient_CPC}{Coefficient_CPC}
#'   \item{Coefficient_RPC}{RPC Coefficient }
#'   \item{Coefficient_refined_RPC}{Coefficient refined RPC}
#'   \item{Coefficient_sex_classifier}{Coefficient sex classifier}
#'   \item{CpGmarker}{CpGmarker}
#' }
#'
#' @examples
#' data(coefLeeGA)
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
#'   \item{Map.Info}{Map.Info}
#'   \item{Gene.Symbol}{Gene symbol}
#'   \item{Entrez.ID}{Entrez ID}
#'   \item{Univariate.Age.Correlation}{}
#'   \item{Horvath.Overlap}{Horvath.Overlap}
#'   \item{Hannum.Overlap}{Hannum.Overlap}
#' }
#' @examples
#' data(coefLevine)
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
#'   \item{CpGmarker}{CpGs id}
#'   \item{CoefficientTraining}{Training Coefficient }
#'   \item{Gene}{Gene}
#'   \item{Chromosome}{Chromosome }
#'   \item{Position}{Position}
#'   \item{Correlation.with.GA}{Correlation with GA}
#' }
#' @examples
#' data(coefMayneGA)
"coefMayneGA"


#' PedBE clock coefficients
#'
#' A dataset containing the coefficients and CpGs information from PedBE clock
#'
#' @docType data
#'
#' @usage data(coefPedBE)
#' @references \url{https://doi.org/10.1073/pnas.1820843116}
#' @format A data frame with 1126 CpGs and 5 variables
#' \describe{
#'   \item{CpGmarker}{CpGs id}
#'   \item{CoefficientTraining}{Training Coefficient }
#'   \item{corAgeTraining}{Correlation age training}
#'   \item{corAgeTest}{Correlation age test}
#' }
#'
#' @examples
#' data(coefPedBE)
"coefPedBE"


#' Skin clock coefficients
#'
#' A dataset containing the coefficients and CpGs information from Skin clock
#'
#' @docType data
#'
#' @usage data(coefSkin)
#' @format A data frame with 392 CpGs and 2 variables
#' \describe{
#'   \item{CpGmarker}{CpGs id}
#'   \item{CoefficientTraining}{Training Coefficient }
#' }
#'
#' @examples
#' data(coefSkin)
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
#'   \item{bp19}{bp19}
#'   \item{SourceSeq}{SourceSeq}
#'   \item{Strand}{Strand}
#'   \item{Probe.SNP.based.on.Illumina.annotation}{Probe.SNP.based.on.Illumina.annotation}
#'   \item{Gene}{Gene}
#'   \item{location}{location}
#'   \item{Relation.to..CpGIsland}{Relation.to..CpGIsland}
#'   \item{Probe.SNP.based.on.1000.Genome}{Probe.SNP.based.on.1000.Genome}
#'   \item{SNP}{SNP}
#'   \item{CHR.SNP}{CHR.SNP}
#'   \item{bp.SNP}{bp.SNP}
#'   \item{ALLELE}{ALLELE}
#'   \item{BSGS.P}{BSGS.P}
#'   \item{BSGS.EFFECT}{BSGS.EFFECT}
#'   \item{BSGS.H2}{BSGS.H2}
#'   \item{LBC.P}{LBC.P}
#'   \item{LBC.EFFECT}{LBC.EFFECT}
#'   \item{LBC.R2}{LBC.R2}
#'   \item{mQTL}{mQTL}
#' }
#'
#' @examples
#' data(coefTL)
"coefTL"

#' probeAnnotation21kdatMethUsed
#'
#' A dataset containing probeAnnotation21kdatMethUsed (REMOVE??)
#'
#' @docType data
#' @usage data(probeAnnotation21kdatMethUsed)
#' @format A data frame with 21368 CpGs and 7 variables
#'
#' @examples
#' data(probeAnnotation21kdatMethUsed)
"probeAnnotation21kdatMethUsed"

#' TestDataset
#'
#' A dataset containing TestDataset
#'
#' @docType data
#' @usage data(TestDataset)
#' @format A data frame with 27236 CpGs and 4 variables
#'
#' @examples
#' data(TestDataset)
"TestDataset"

#' progress_data
#'
#' A dataset containing progress_data
#'
#' @docType data
#' @usage data(progress_data)
#' @format A data frame with 148 obs. and 151 variables
#'
#' @examples
#' data(progress_data)
"progress_data"

#' progress_vars
#'
#' A dataset containing progress_vars
#'
#' @docType data
#' @usage data(progress_vars)
#' @format A data frame with 150 obs. and 3 variables
#'
#' @examples
#' data(progress_vars)
"progress_vars"


#' MethylationDataExample55
#'
#' A dataset containing data for vignette examples
#'
#' @docType data
#' @usage data(MethylationDataExample55)
#' @format A data frame with 27578 cpgs and 17 samples
#'
#' @examples
#' data(MethylationDataExample55)
"MethylationDataExample55"

#' references
#'
#' A dataset containing data for references data for blood
#'
#' @docType data
#' @usage data(references)
#' @format A data frame with 27578 cpgs and 17 samples
#'
#' @examples
#' data(references)
"references"
NULL