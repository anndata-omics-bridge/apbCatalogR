# Check every rule against one catalogue set

One row per catalogued or required field of each rule level the set
reviews; a set without vendor catalogues is checked against every rule
level. A set with requirements judges each level: `fail` when a MUST
field is unsupported, `warn` when a SHOULD or MAY field is, `pass`
otherwise.

## Usage

``` r
rule_coverage(catalogue)
```

## Arguments

- catalogue:

  The set's name.

## Value

A data frame with columns `software`, `version`, `rule`, `level`,
`kind`, `qualifiers` (the other qualifiers as `key=value`), `origin`,
`producer` (`NA` for the vendor's own fields), `requirement`,
`location`, `table`, `column` (`NA` when unsupported), `inputs` (a list
column of the vendor columns apb2 computes the column from), `needs`
(what to run before the field exists) and `verdict` (`NA` for a set
without requirements).

## Examples

``` r
head(rule_coverage("abundance"))
#>   software version                      rule           level      kind
#> 1 AlphaDIA    1.10 alphadia/v1_10/rules.json peptidoform_ion intensity
#> 2 AlphaDIA    1.10 alphadia/v1_10/rules.json peptidoform_ion directlfq
#> 3 AlphaDIA    1.12 alphadia/v1_12/rules.json peptidoform_ion intensity
#> 4 AlphaDIA    1.12 alphadia/v1_12/rules.json peptidoform_ion directlfq
#> 5 AlphaDIA     2.x    alphadia/v2/rules.json peptidoform_ion intensity
#> 6 AlphaDIA     2.x    alphadia/v2/rules.json peptidoform_ion directlfq
#>                  qualifiers origin producer requirement location table
#> 1        normalization=none   <NA>       NA      SHOULD   layers  <NA>
#> 2 normalization=run_setting   <NA>       NA        <NA>   layers  <NA>
#> 3        normalization=none   <NA>       NA      SHOULD   layers  <NA>
#> 4 normalization=run_setting   <NA>       NA        <NA>   layers  <NA>
#> 5        normalization=none   <NA>       NA      SHOULD   layers  <NA>
#> 6 normalization=run_setting   <NA>       NA        <NA>   layers  <NA>
#>                column inputs needs verdict
#> 1                <NA>         <NA>    warn
#> 2           Intensity         <NA>    warn
#> 3                <NA>         <NA>    warn
#> 4           Intensity         <NA>    warn
#> 5                <NA>         <NA>    warn
#> 6 Precursor_Intensity         <NA>    warn
```
