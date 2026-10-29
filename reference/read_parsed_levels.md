# Read an APB Parquet result

Reads the directory apb2 writes for a `.parquet` result into a
[ParsedLevels](https://anndata-omics-bridge.github.io/apbCatalogR/reference/ParsedLevels.md)
object, one anndataR `AnnData` per level.

## Usage

``` r
read_parsed_levels(path)
```

## Arguments

- path:

  The result directory, such as `converted.parquet`.

## Value

A
[ParsedLevels](https://anndata-omics-bridge.github.io/apbCatalogR/reference/ParsedLevels.md)
object.

## Examples

``` r
results <- tempfile()
archive <- system.file("extdata", "bundle", "results.tar.gz", package = "apbCatalogR")
utils::untar(archive, exdir = results)
read_parsed_levels(file.path(results, "edge-two-levels.parquet"))
#> <ParsedLevels>
#>   peptidoform_ion: 2 x 3, layers: Ms1_Normalised, Precursor_Normalised, Precursor_Quantity, Ms1_Area, Q_Value, PEP, RT, Averagine, Best_Fr_Mz, Best_Fr_Mz_Delta, Channel_Evidence, Channel_Q_Value, Empirical_Quality, Evidence, FWHM, GG_Q_Value, IM, Mass_Evidence, Ms1_Apex_Area, Ms1_Apex_Mz_Delta, Ms1_Profile_Corr, Ms1_Q_Value, Ms1_Total_Signal_After, Ms1_Total_Signal_Before, Normalisation_Factor, Normalisation_Noise, PTM_Site_Confidence, Peptidoform_Q_Value, Predicted_IM, Predicted_RT, Predicted_iIM, Predicted_iRT, Profile_After, Profile_Before, Profile_HM, Protein_Q_Value, Quantity_Quality, RT_Start, RT_Stop, Translated_Q_Value
#>   protein: 2 x 3, layers: PG_MaxLFQ, Genes_MaxLFQ, PG_Q_Value, PG_PEP, Genes_MaxLFQ_Empirical_Quality, Genes_MaxLFQ_Empirical_Unique_Quality, Genes_MaxLFQ_Quality, Genes_MaxLFQ_Unique, Genes_MaxLFQ_Unique_Quality, Genes_TopN, PG_MaxLFQ_Empirical_Quality, PG_MaxLFQ_Quality, PG_TopN
```
