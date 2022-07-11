# methylclock


To use methylclock under R<4.1 you need to install the package under main branch or release <= 0.99.0 (https://github.com/isglobal-brge/methylclock/releases/tag/v0.7.7)

## Installation : 

To install methylclock you need  R >= 4.1 : 

```r{install, eval=FALSE}

if (!requireNamespace("BiocManager", quietly = TRUE))
    install.packages("BiocManager")

BiocManager::install("methylclock")

```

## Description
 
This package allows to estimate chronological and gestational DNA methylation (DNAm) age as well as biological age using different methylation clocks. The package includes the following estimators:

### Chronological DNAm age (in years)

- **Horvath's clock**: It uses 353 CpGs described in @horvath2013dna. It was trained using 27K and 450K arrays in samples from different tissues. Other three different age-related biomarkers are also computed:
     - **AgeAcDiff** (DNAmAge acceleration difference): Difference between DNAmAge and chronological age.
     <!-- To be removed - **IEAA** (Intrinsic Epigenetic Age Acceleration): Residuals obtained after regressing DNAmAge and chronological age adjusted by cell counts. -->
     <!-- To be removed - **EEAA** (Extrinsic Epigenetic Age Acceleration): Residuals obtained after regressing DNAmAge and chronological age. This measure was also known as DNAmAge acceleration residual in the first Horvath's paper.-->
     - **IEAA** Residuals obtained after regressing DNAmAge and chronological age adjusted by cell counts.
     - **EEAA** Residuals obtained after regressing DNAmAge and chronological age. This measure was also known as DNAmAge acceleration residual in the first Horvath's paper.
- **Hannum's clock**: It uses 71 CpGs described in @hannum2013genome. It was trained using 450K array in blood samples. Another are-related biomarer is also computed:
     - **AMAR** (Apparent Methylomic Aging Rate): Measure proposed in @hannum2013genome computed as the ratio between DNAm age and the chronological age.
- **BNN**: It uses Horvath's CpGs to train a Bayesian Neural Network (BNN) to predict DNAm age as described in @alfonso2018.
- **Horvath's skin+blood clock (skinHorvath)**: Epigenetic clock for skin and blood cells. It uses 391 CpGs described in @horvath2018epigenetic. It was trained using 450K EPIC arrays in skin and blood sampels.
- **PedBE clock**: Epigenetic clock from buccal epithelial swabs. It's intended purpose is buccal samples from individuals aged 0-20 years old. It uses 84 CpGs described in @mcewen2019pedbe. The authors gathered 1,721 genome-wide DNAm profiles from 11 different cohorts with individuals aged 0 to 20 years old. 
- **Wu's clock**: It uses 111 CpGs described in @wu2019dna. It is designed to predict age in children. It was trained using 27K and 450K.
- **BLUP clock**:  It uses 319607 CpGs described in @zhang2019improved. It was trained using 450K and EPIC arrays in blood (13402 samples) and saliva (259 samples). Age predictors based on training sets with various sample sizes using Best Linear Unbiased Prediction (BLUP)  
- **EN clock**:  It uses 514 CpGs described in @zhang2019improved. It was trained using 450K and EPIC arrays in blood (13402 samples) and saliva (259 samples). Age predictors based on training sets with various sample sizes using Elastic Net (EN) 

### Gestational DNAm age (in weeks)

- **Knight's clock**: It uses 148 CpGs described in @knight2016epigenetic. It was trained using 27K and 450K arrays in cord blood samples.
- **Bohlin's clock**: It uses 96 CpGs described in @bohlin2016prediction. It was trained using 450K array in cord blood samples.
- **Mayne's clock**: It uses 62 CpGs described in @mayne2017accelerated. It was trained using 27K and 450K.
- **EPIC clock**: EPIC-based predictor of gestational age. It uses 176 CpGs described in @haftorn2021epic. It was trained using EPIC arrays in cord blood samples.
- **Lee's clocks**: Three different biological clocks described in @lee2019placental are implemented. It was trained for 450K and EPIC arrays in placenta samples.
     - **RPC clock**: Robust placental clock (RPC). It uses 558 CpG sites.
     - **CPC clock**: Control placental clock (CPC). It usses 546 CpG sites.
     - **Refined RPC clock**: Useful for uncomplicated term pregnancies (e.g. gestational age >36 weeks). It uses 396 CpG sites.


The biological DNAm clocks implemented in this package are:

- **Levine's clock** (also know as PhenoAge): It uses 513 CpGs described in @levine2018epigenetic. It was trained using 27K, 450K and EPIC arrays in blood samples.
- **Telomere Length's clock** (TL): It uses 140 CpGs described in @lu2019dna It was trained using 450K and EPIC arrays in blood samples.

## Citation
<a id="1">[1]</a> 
Dolors Pelegri-Siso, Paula de Prado, Justiina Ronkainen, Mariona Bustamante, Juan R Gonzalez, methylclock: a Bioconductor package to estimate DNA methylation age, Bioinformatics, Volume 37, Issue 12, 15 June 2021, Pages 1759â€“1760, doi: [10.1093/bioinformatics/btaa825](https://doi.org/10.1093/bioinformatics/btaa825). PMID: 32960939.
