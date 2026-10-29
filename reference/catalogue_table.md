# The catalogue sets as one long table

One row per entry and rule it applies to, as `apb-catalog table` prints
it.

## Usage

``` r
catalogue_table(catalogue = NULL)
```

## Arguments

- catalogue:

  One set's name; `NULL` lists every set of the bundle.

## Value

A data frame with columns `set`, `source` (the vendor, `producer:<name>`
or `annotation:<convention>`), `version_pattern`, `rule` (`*` for an
entry of every rule), `level`, `location`, `table`, `name`
(`role:<role>` or `prefix:<prefix>` for a field found by role or
column-name prefix), one `qualifier.<name>` column per qualifier,
`requirement` and `entry_id`.

## Examples

``` r
head(catalogue_table("confidence"))
#>          set    source    version_pattern                      rule
#> 1 confidence  AlphaDIA ^1\\.12(?:\\.|-|$) alphadia/v1_12/rules.json
#> 2 confidence  AlphaDIA              ^2\\.    alphadia/v2/rules.json
#> 3 confidence AlphaPept          ^0\\.5\\.      alphapept/rules.json
#> 4 confidence    DIA-NN  ^1\\.[0-7](\\.|$)     diann/v1_7/rules.json
#> 5 confidence    DIA-NN   ^1\\.[89](\\.|$)     diann/v1_8/rules.json
#> 6 confidence    DIA-NN            ^2\\..*       diann/v2/rules.json
#>             level location table    name qualifier.kind qualifier.stage
#> 1 peptidoform_ion   layers  <NA>  QValue        q_value  identification
#> 2 peptidoform_ion   layers  <NA>  QValue        q_value  identification
#> 3 peptidoform_ion   layers  <NA> Q_Value        q_value  identification
#> 4 peptidoform_ion   layers  <NA> Q_Value        q_value  identification
#> 5 peptidoform_ion   layers  <NA> Q_Value        q_value  identification
#> 6 peptidoform_ion   layers  <NA> Q_Value        q_value  identification
#>   requirement                              entry_id
#> 1        <NA> alphadia.v1_12.peptidoform_ion.QValue
#> 2        <NA>    alphadia.v2.peptidoform_ion.QValue
#> 3        <NA>     alphapept.peptidoform_ion.Q_Value
#> 4        <NA>         diann.peptidoform_ion.Q_Value
#> 5        <NA>         diann.peptidoform_ion.Q_Value
#> 6        <NA>         diann.peptidoform_ion.Q_Value
```
