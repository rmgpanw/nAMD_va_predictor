
<!-- README.md is generated from README.Rmd. Please edit that file -->

# nAMD_va_predictor

<!-- badges: start -->
<!-- badges: end -->

An R Shiny app that describes visual outcomes at 1 year for a given
baseline visual acuity, based on publicly available real-world data
included in the [eyedata](https://github.com/tjebo/eyedata) package.
Visit <https://7hocgq-rmgpanw.shinyapps.io/nAMD_va_predictor/> to try
out app.

Summary of dataset:

``` r
library(eyedata)
suppressPackageStartupMessages(library(tidyverse))
amd %>% 
  mutate(patID = as.integer(str_remove(string = patID,
                                       pattern = "id_"))) %>% 
  group_by(patID) %>% 
  arrange(time) %>% 
  summarise(sex = sex[1],
            age = age[1],
            ethnicity = ethnicity[1],
            follow_up_length = max(time, na.rm = TRUE),
            baseline_va = va[1],
            regimen = str_c(unique(regimen, 
                                   sep = "", 
                                   collapse = "; "))) %>% 
  select(-patID) %>% 
  gtsummary::tbl_summary() %>% 
  gtsummary::as_tibble() %>% 
  mutate(across(everything(),
                ~ ifelse(is.na(.x), "", .x))) %>% 
  knitr::kable()
```

| **Characteristic** | **N = 7,802**    |
|:-------------------|:-----------------|
| sex                |                  |
| f                  | 4,776 (61%)      |
| m                  | 3,026 (39%)      |
| age                |                  |
| 50-59              | 227 (2.9%)       |
| 60-69              | 975 (12%)        |
| 70-79              | 2,633 (34%)      |
| \>80               | 3,967 (51%)      |
| ethnicity          |                  |
| afrocarribean      | 177 (2.3%)       |
| caucasian          | 4,785 (61%)      |
| mixed              | 21 (0.3%)        |
| se_asian           | 652 (8.4%)       |
| unknown/other      | 2,167 (28%)      |
| follow_up_length   | 658 (261, 1,248) |
| baseline_va        | 57 (45, 68)      |
| regimen            |                  |
| aflibercept        | 3,951 (51%)      |
| ranibizumab        | 3,851 (49%)      |
