As5697\_tidying\_and\_exploratory
================
Apoorva Srinivasan
12/10/2018

``` r
library(tidyverse)
```

    ## ── Attaching packages ─────────────────────────────────────────────────────────── tidyverse 1.2.1 ──

    ## ✔ ggplot2 3.0.0     ✔ purrr   0.2.5
    ## ✔ tibble  1.4.2     ✔ dplyr   0.7.8
    ## ✔ tidyr   0.8.1     ✔ stringr 1.3.1
    ## ✔ readr   1.1.1     ✔ forcats 0.3.0

    ## ── Conflicts ────────────────────────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()

``` r
cancer_reg = read_csv("./data/Cancer_Registry.csv") %>%
  janitor::clean_names() %>%
  select(target_death_rate, everything()) %>%
  separate(geography, into = c("county", "state"), sep = ",")
```

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   avgDeathsPerYear = col_integer(),
    ##   medIncome = col_integer(),
    ##   popEst2015 = col_integer(),
    ##   binnedInc = col_character(),
    ##   Geography = col_character()
    ## )

    ## See spec(...) for full column specifications.

There are in total 35 variables and 3047observations in the dataset.

``` r
#missing data

#colSums(is.na(cancer_reg))  #pct_some_col18_24 has 2285 NAs, pct_employed_coverage_alone has 609 NA, pct_employed16_over has 152 NAs


missing_value = sapply(cancer_reg[1:34], function(x) sum(length(which(is.na(x)))))


# Percentage of missing value
percentage_missing = sapply(cancer_reg[1:34], function(x) sum(length(which(is.na(x)))) / nrow(cancer_reg))
percentage_missing %>% data.frame()
```

    ##                                     .
    ## target_death_rate          0.00000000
    ## avg_ann_count              0.00000000
    ## avg_deaths_per_year        0.00000000
    ## incidence_rate             0.00000000
    ## med_income                 0.00000000
    ## pop_est2015                0.00000000
    ## poverty_percent            0.00000000
    ## study_per_cap              0.00000000
    ## binned_inc                 0.00000000
    ## median_age                 0.00000000
    ## median_age_male            0.00000000
    ## median_age_female          0.00000000
    ## county                     0.00000000
    ## state                      0.00000000
    ## avg_household_size         0.00000000
    ## percent_married            0.00000000
    ## pct_no_hs18_24             0.00000000
    ## pct_hs18_24                0.00000000
    ## pct_some_col18_24          0.74991795
    ## pct_bach_deg18_24          0.00000000
    ## pct_hs25_over              0.00000000
    ## pct_bach_deg25_over        0.00000000
    ## pct_employed16_over        0.04988513
    ## pct_unemployed16_over      0.00000000
    ## pct_private_coverage       0.00000000
    ## pct_private_coverage_alone 0.19986872
    ## pct_emp_priv_coverage      0.00000000
    ## pct_public_coverage        0.00000000
    ## pct_public_coverage_alone  0.00000000
    ## pct_white                  0.00000000
    ## pct_black                  0.00000000
    ## pct_asian                  0.00000000
    ## pct_other_race             0.00000000
    ## pct_married_households     0.00000000

``` r
##getting rid of variables with missing values

cancer_reg %>% select(-pct_some_col18_24, -pct_private_coverage_alone)
```

    ## # A tibble: 3,047 x 33
    ##    target_death_ra… avg_ann_count avg_deaths_per_… incidence_rate
    ##               <dbl>         <dbl>            <int>          <dbl>
    ##  1             165.          1397              469           490.
    ##  2             161.           173               70           412.
    ##  3             175.           102               50           350.
    ##  4             195.           427              202           430.
    ##  5             144.            57               26           350.
    ##  6             176            428              152           505.
    ##  7             176.           250               97           462.
    ##  8             184.           146               71           404 
    ##  9             190.            88               36           459.
    ## 10             178.          4025             1380           511.
    ## # ... with 3,037 more rows, and 29 more variables: med_income <int>,
    ## #   pop_est2015 <int>, poverty_percent <dbl>, study_per_cap <dbl>,
    ## #   binned_inc <chr>, median_age <dbl>, median_age_male <dbl>,
    ## #   median_age_female <dbl>, county <chr>, state <chr>,
    ## #   avg_household_size <dbl>, percent_married <dbl>, pct_no_hs18_24 <dbl>,
    ## #   pct_hs18_24 <dbl>, pct_bach_deg18_24 <dbl>, pct_hs25_over <dbl>,
    ## #   pct_bach_deg25_over <dbl>, pct_employed16_over <dbl>,
    ## #   pct_unemployed16_over <dbl>, pct_private_coverage <dbl>,
    ## #   pct_emp_priv_coverage <dbl>, pct_public_coverage <dbl>,
    ## #   pct_public_coverage_alone <dbl>, pct_white <dbl>, pct_black <dbl>,
    ## #   pct_asian <dbl>, pct_other_race <dbl>, pct_married_households <dbl>,
    ## #   birth_rate <dbl>

``` r
##percentage missing for pct_employed16_over is  ~5%, checking to see if its correlated with the outcome 
reg = lm(target_death_rate~pct_employed16_over, data = cancer_reg) %>%
  summary()

##Since the p-value is small, we will retain pct_employed16_over
```
