library(gtsummary)
library(dplyr)

# ── Simulate data ──────────────────────────────────────────────────────────────
set.seed(42)
n <- 300

df <- tibble(
  severe_ad  = rbinom(n, 1, prob = 0.4),
  age        = round(rnorm(n, mean = 35, sd = 12)),
  bmi        = round(rnorm(n, mean = 24, sd = 4), 1),
  il13_level = round(rnorm(n, mean = 5,  sd = 2),   2),
  tslp_level = round(rnorm(n, mean = 3,  sd = 1.5), 2),
  sex        = factor(sample(c("Male", "Female"), n, replace = TRUE)),
  treatment  = factor(sample(c("Dupilumab", "Cyclosporin", "Placebo"),
                             n, replace = TRUE)),
  smoker     = factor(sample(c("No", "Yes"), n, replace = TRUE,
                             prob = c(0.75, 0.25)))
) %>%
  mutate(severe_ad = factor(severe_ad, levels = c(0,1),
                            labels = c("Mild", "Severe")))

# ── Univariable table ──────────────────────────────────────────────────────────
univ_tab <- df %>%
  select(severe_ad, age, bmi, il13_level, tslp_level, sex, treatment, smoker) %>%
  tbl_uvregression(
    method       = glm,
    y            = severe_ad,
    method.args  = list(family = binomial),
    exponentiate = TRUE,
    pvalue_fun   = label_style_pvalue(digits = 3)
  ) %>%
  bold_p(t = 0.05) %>%
  bold_labels() %>%
  add_nevent() %>%
  modify_header(estimate ~ "**Unadjusted OR**")

# ── Multivariable table ────────────────────────────────────────────────────────
mv_tab <- glm(
  severe_ad ~ age + bmi + il13_level + tslp_level + sex + treatment + smoker,
  data = df, family = binomial
) %>%
  tbl_regression(
    exponentiate = TRUE,
    pvalue_fun   = label_style_pvalue(digits = 3)
  ) %>%
  bold_p(t = 0.05) %>%
  bold_labels() %>%
  modify_header(estimate ~ "**Adjusted OR**")

# ── Merge & export ─────────────────────────────────────────────────────────────
tbl_merge(
  tbls        = list(univ_tab, mv_tab),
  tab_spanner = c("**Univariable**", "**Multivariable**")
) %>%
  modify_caption("**Table 1. Predictors of Severe Atopic Dermatitis**")

# library(flextable)
# final_table %>% as_flex_table() %>%
#   flextable::save_as_docx(path = "regression_table.docx")
