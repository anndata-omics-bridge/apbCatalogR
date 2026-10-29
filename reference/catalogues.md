# The catalogue sets

Every catalogue set of the bundle with its purpose, its users and its
number of entries.

## Usage

``` r
catalogues()
```

## Value

A data frame with columns `catalogue`, `purpose`, `used_by` (a list
column), `has_requirements` and `entries`.

## Examples

``` r
catalogues()
#>                catalogue
#> 1              abundance
#> 2             confidence
#> 3 proteobench_entrapment
#> 4                    apb
#> 5         chromatography
#>                                                                                                                                                     purpose
#> 1                                                What each layer an APB2 rule marks abundance measures, and whether the software normalizes it across runs.
#> 2                                    How likely each identification, or the signal quantified for it, is correct: PEP and q-values per run and per feature.
#> 3                                                                      The precursor q-values ProteoBench's entrapment scoring ranks by, each its own kind.
#> 4 The fields APB writes itself for every vendor, asked for by kind: ProForma identifiers, decoy and contaminant flags, role columns and sample annotations.
#> 5                                                         Retention times, elution-peak bounds and peak widths, with the unit the software reports them in.
#>        used_by has_requirements entries
#> 1 apb-prot....             TRUE      48
#> 2 apb-aggr....            FALSE      22
#> 3 apb-prot....            FALSE       7
#> 4 apb-expo....             TRUE      66
#> 5 apb-expo....            FALSE      54
```
