# Looking up APB fields by meaning

## Introduction

The AnnData Proteomics Bridge (APB) converts the reports of
quantification software such as DIA-NN, Spectronaut or MaxQuant into one
result layout. The columns keep the vendors’ names, so a posterior error
probability is `PEP` in MaxQuant and `EG_PEP` in Spectronaut.
apb-catalog records what each column means;
*[apbCatalogR](https://bioconductor.org/packages/3.23/apbCatalogR)*
implements its API in R, so an R application asks for a meaning and gets
the column or its data.

The package works on a bundle apb-catalog releases: the APB parse rules,
the reviewed catalogue sets and the answers apb-catalog gives on
synthetic results. One bundle ships with the package.

## Installation

``` r

if (!requireNamespace("BiocManager", quietly = TRUE)) {
  install.packages("BiocManager")
}
BiocManager::install("apbCatalogR")
```

``` r

library(apbCatalogR)
catalog_version()
#> $version
#> [1] "0.1.1"
#> 
#> $origin
#> [1] "shipped"
#> 
#> $path
#> [1] "/home/runner/work/_temp/Library/apbCatalogR/extdata/bundle"
#> 
#> $apb2_version
#> [1] "0.1.1"
```

## Catalogue sets

Each catalogue set serves one kind of consumer:

``` r

catalogues()[, c("catalogue", "has_requirements", "entries")]
#>                catalogue has_requirements entries
#> 1              abundance             TRUE      48
#> 2             confidence            FALSE      22
#> 3 proteobench_entrapment            FALSE       7
#> 4                    apb             TRUE      66
#> 5         chromatography            FALSE      54
```

## Which column a software version reports

`Catalog` answers from the rules alone, without data:

``` r

diann <- Catalog$new("DIA-NN", "2.2.0", "abundance")
diann$layer("peptidoform_ion", kind = "intensity", normalization = "none")
#> <Reference peptidoform_ion layers: Precursor_Quantity>
diann$kinds("peptidoform_ion", "layers")
#> [1] "intensity"     "ms1_intensity"
```

## The data of a result

[`read_parsed_levels()`](https://anndata-omics-bridge.github.io/apbCatalogR/reference/read_parsed_levels.md)
reads a Parquet result apb2 wrote, one anndataR `AnnData` per level. The
bundle carries synthetic results; this one has MaxQuant’s
peptidoform-ion level.

``` r

results <- tempfile()
utils::untar(system.file("extdata", "bundle", "results.tar.gz", package = "apbCatalogR"), exdir = results)
result <- read_parsed_levels(file.path(results, "maxquant_wide-peptidoform_ion.parquet"))
result
#> <ParsedLevels>
#>   peptidoform_ion: 2 x 3, layers: Intensity, MS_MS_Count, Retention_Time, Retention_Length, Calibrated_Retention_Time, Calibrated_Retention_Time_Start, Calibrated_Retention_Time_Finish, Score, PEP
```

`DataCatalog` answers on the result and hands back the data:

``` r

confidence <- DataCatalog$new(result, "confidence")
pep <- confidence$layer("peptidoform_ion", kind = "pep")
dim(pep)
#> [1] 2 3
```

MaxQuant reports no q-value on this level, so that lookup gives `NULL`:

``` r

confidence$layer("peptidoform_ion", kind = "q_value")
#> NULL
```

A lookup that several fields answer signals `UnresolvedField` rather
than choosing silently. DIA-NN 1.8 reports more than one unnormalized
and normalized intensity per precursor:

``` r

tryCatch(
  Catalog$new("DIA-NN", "1.8.1", "abundance")$layer("peptidoform_ion", kind = "intensity"),
  UnresolvedField = function(condition) conditionMessage(condition)
)
#> [1] "kind=intensity on peptidoform_ion is ambiguous: 2 entries match: Precursor_Quantity, Precursor_Normalised; candidates: Precursor_Quantity, Precursor_Normalised"
```

## Rule coverage

[`rule_coverage()`](https://anndata-omics-bridge.github.io/apbCatalogR/reference/rule_coverage.md)
checks every rule against one set; a set with requirements judges each
level:

``` r

coverage <- rule_coverage("abundance")
unique(coverage[, c("software", "version", "level", "verdict")])[1:5, ]
#>    software version           level verdict
#> 1  AlphaDIA    1.10 peptidoform_ion    warn
#> 3  AlphaDIA    1.12 peptidoform_ion    warn
#> 5  AlphaDIA     2.x peptidoform_ion    warn
#> 7  AlphaDIA     2.x         protein    <NA>
#> 8 AlphaPept     0.5 peptidoform_ion    pass
```

## A newer catalog

[`update_catalog()`](https://anndata-omics-bridge.github.io/apbCatalogR/reference/update_catalog.md)
downloads the bundle of the latest apb-catalog release into the user
cache; later sessions use the newest bundle, shipped or downloaded.

``` r

update_catalog()
```

## Session information

``` r

sessionInfo()
#> R version 4.6.1 (2026-06-24)
#> Platform: x86_64-pc-linux-gnu
#> Running under: Ubuntu 24.04.5 LTS
#> 
#> Matrix products: default
#> BLAS:   /usr/lib/x86_64-linux-gnu/openblas-pthread/libblas.so.3 
#> LAPACK: /usr/lib/x86_64-linux-gnu/openblas-pthread/libopenblasp-r0.3.26.so;  LAPACK version 3.12.0
#> 
#> locale:
#>  [1] LC_CTYPE=C.UTF-8       LC_NUMERIC=C           LC_TIME=C.UTF-8       
#>  [4] LC_COLLATE=C.UTF-8     LC_MONETARY=C.UTF-8    LC_MESSAGES=C.UTF-8   
#>  [7] LC_PAPER=C.UTF-8       LC_NAME=C              LC_ADDRESS=C          
#> [10] LC_TELEPHONE=C         LC_MEASUREMENT=C.UTF-8 LC_IDENTIFICATION=C   
#> 
#> time zone: UTC
#> tzcode source: system (glibc)
#> 
#> attached base packages:
#> [1] stats     graphics  grDevices utils     datasets  methods   base     
#> 
#> other attached packages:
#> [1] apbCatalogR_0.99.0 BiocStyle_2.40.0  
#> 
#> loaded via a namespace (and not attached):
#>  [1] Matrix_1.7-5        bit_4.6.0           jsonlite_2.0.0     
#>  [4] compiler_4.6.1      BiocManager_1.30.27 Rcpp_1.1.2         
#>  [7] tidyselect_1.2.1    assertthat_0.2.1    anndataR_1.2.3     
#> [10] jquerylib_0.1.4     png_0.1-9           arrow_26.0.0       
#> [13] systemfonts_1.3.2   textshaping_1.0.5   yaml_2.3.12        
#> [16] fastmap_1.2.0       reticulate_1.47.0   lattice_0.22-9     
#> [19] R6_2.6.1            knitr_1.52          htmlwidgets_1.6.4  
#> [22] bookdown_0.48       desc_1.4.3          bslib_0.12.0       
#> [25] rlang_1.3.0         cachem_1.1.0        xfun_0.62          
#> [28] fs_2.1.0            sass_0.4.10         bit64_4.8.6        
#> [31] otel_0.2.0          cli_3.6.6           pkgdown_2.2.1      
#> [34] magrittr_2.0.5      digest_0.6.39       grid_4.6.1         
#> [37] lifecycle_1.0.5     vctrs_0.7.3         evaluate_1.0.5     
#> [40] glue_1.8.1          ragg_1.5.2          rmarkdown_2.32     
#> [43] purrr_1.2.2         tools_4.6.1         htmltools_0.5.9
```
