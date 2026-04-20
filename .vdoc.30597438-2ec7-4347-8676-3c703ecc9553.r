#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#organizar pacotes e imports aqui

pkgs <- c(
  "tidyverse",
  "janitor",
  "skimr",
  "reticulate",
  "gtsummary",
  "patchwork",
  'ggcorrplot'
)

instalar <- pkgs[!pkgs %in% installed.packages()[, "Package"]]

if (length(instalar) > 0) {
  install.packages(instalar)
}

library(reticulate)
library(tidyverse)
library(janitor)
library(skimr)
library(ggplot2)
library(dplyr)
library(tidyr)
library(janitor)
library(patchwork)
library(ggcorrplot)


py_config()
reticulate::py_install(c("pandas", "tabulate", "ipython"))


```
#
#
#
#
#
#
#
#
#
#
#
#
#
#
loan <- read_csv("data/Loan/loan_approval_dataset.csv") |>
  clean_names()
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#| include: false
n_obs <- nrow(loan)
n_vars <- ncol(loan)
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#| label: fig-distribuicoes
#| fig-cap: "Distribuição das principais variáveis numéricas do conjunto de dados"
#| fig-subcap: 
#|   - "Distribuição da renda anual"
#|   - "Distribuição do valor do empréstimo"
#|   - "Distribuição do prazo"
#|   - "Distribuição do CIBIL score"
#|   - "Distribuição dos ativos residenciais"
#|   - "Distribuição dos ativos comerciais"
#|   - "Distribuição dos bens de luxo"
#|   - "Distribuição dos ativos bancários"
#| layout-ncol: 2
#| out-width: "100%"
#| 
library(patchwork)
loan <- janitor::clean_names(loan)
vars <- c(
  "income_annum",
  "loan_amount",
  "loan_term",
  "cibil_score",
  "residential_assets_value",
  "commercial_assets_value",
  "luxury_assets_value",
  "bank_asset_value"
)

for (v in vars) {
  
  p <- ggplot(loan, aes(x = .data[[v]])) +
    geom_histogram(bins = 30, fill = "steelblue", color = "white") +
    theme_minimal() +
    labs(
      #title = paste("Distribuição de", v),
      x = v,
      y = "Frequência"
    )
  
  print(p)
}

#
#
#
#
#
#
#
#
#
#
#| label: fig-target-plot
#| fig-cap: "Distribuição da aprovação de empréstimos"


loan |>
  count(loan_status) |>
  mutate(pct = n / sum(n)) |>
  ggplot(aes(x = factor(loan_status), y = pct)) +
  geom_col() +
  scale_y_continuous(labels = scales::percent) +
  labs(
    x = "Status do Empréstimo",
    y = "Percentual"
  )
```
#
#| include: false

target_tab <- loan |>
  dplyr::count(loan_status) |>
  dplyr::mutate(
    pct = n / sum(n) * 100
  )

positive_pct <- round(
  target_tab$pct[target_tab$loan_status == "Approved"], 2
)

negative_pct <- round(
  target_tab$pct[target_tab$loan_status == "Rejected"], 2
)
positive_pct
#
#
#
#
#
#
#
#

loan |> 
  count(education, loan_status) |> 
  group_by(education) |> 
  mutate(rate = n / sum(n))
loan |> 
  ggplot(aes(x = income_annum, fill = factor(loan_status))) +
  geom_histogram(bins = 30, alpha = 0.6, position = "identity") +
  labs(
    x = "Renda anual",
    fill = "Status do empréstimo"
  )
#
#
#
#
#| label: fig-distribuicoes-target
#| fig-cap: "Distribuição das variáveis numéricas por status de aprovação do empréstimo"
#| fig-width: 14
#| fig-height: 10

library(tidyverse)
library(janitor)
library(patchwork)

loan <- clean_names(loan)

loan$loan_status <- factor(
  loan$loan_status,
  labels = c("Rejeitado", "Aprovado")
)

vars <- c(
  "income_annum",
  "loan_amount",
  "loan_term",
  "cibil_score",
  "residential_assets_value",
  "commercial_assets_value",
  "luxury_assets_value",
  "bank_asset_value"
)

loan_long <- loan %>%
  select(all_of(vars), loan_status) %>%
  pivot_longer(
    cols = all_of(vars),
    names_to = "variavel",
    values_to = "valor"
  )

plot_vars <- function(vars_subset) {
  loan_long %>%
    filter(variavel %in% vars_subset) %>%
    ggplot(aes(x = valor, fill = loan_status)) +
    geom_density(alpha = 0.35) +
    facet_wrap(~variavel, scales = "free", ncol = 2) +
    scale_fill_manual(values = c("#d95f02", "#1b9e77")) +
    theme_minimal(base_size = 15) +
    theme(
      legend.position = "bottom",
      legend.box = "horizontal",
      strip.text = element_text(size = 13),
      axis.text = element_text(size = 12),
      axis.title = element_text(size = 13),
      legend.text = element_text(size = 12),
      legend.title = element_text(size = 13)
    ) +
    labs(
      x = NULL,
      y = "Densidade",
      fill = "Status"
    )
}

p1 <- plot_vars(vars[1:4])
p2 <- plot_vars(vars[5:8])

(p1 / p2) +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")
#
#
#
#| label: fig-score
#| fig-cap: "Distribuição da aprovação de empréstimos por score de crédito"
loan |> 
  ggplot(aes(x = cibil_score, y = factor(loan_status))) +
  geom_boxplot() +
  labs(
    x = "CIBIL Score",
    y = "Status do Empréstimo"
  )  
#
#
#
#
#
#
#| label: fig-correlacao
#| fig-cap: "Mapa de calor da correlação entre variáveis numéricas"
#| warning: false
#| fig-width: 8
#| fig-height: 8

loan_num <- loan %>%
  clean_names() %>%
  select(
    income_annum,
    loan_amount,
    loan_term,
    cibil_score,
    residential_assets_value,
    commercial_assets_value,
    luxury_assets_value,
    bank_asset_value
  )

cor_matrix <- round(cor(loan_num, use = "complete.obs"), 3)

ggcorrplot(
  cor_matrix,
  type = "lower",
  lab = TRUE,
  lab_size = 4,
  colors = c("#b2182b", "white", "#2166ac"),
  outline.col = "gray70"
) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(
      angle = 90,
      hjust = 1,
      vjust = 0.5
    ),
    axis.text.y = element_text(size = 12)
  )
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
summary(loan)
colSums(is.na(loan))
#
#
#
#
#


negativos <- sum(loan$residential_assets_value < 0)
total <- nrow(loan)

percentual <- negativos / total 
percentual
negativos

# tratamento para os valores negativos
n_before <- nrow(loan)

loan <- loan |>
  dplyr::filter(residential_assets_value >= 0)

n_after <- nrow(loan)

n_removed <- n_before - n_after
#
#
#
#
#
#

#
#
#
#
#
#
#
#
#
#
library(dplyr)
library(janitor)

loan <- loan %>%
  clean_names()

loan$loan_status <- as.factor(loan$loan_status)
#
#
#
#
modelo_logit <- glm(
  loan_status ~ no_of_dependents +
    education +
    self_employed +
    income_annum +
    loan_amount +
    loan_term +
    cibil_score +
    residential_assets_value +
    commercial_assets_value +
    luxury_assets_value +
    bank_asset_value,
  data = loan,
  family = binomial(link = "logit")
)

summary(modelo_logit)

exp(coef(modelo_logit))

exp(confint(modelo_logit))

prob <- predict(modelo_logit, type = "response")

pred <- ifelse(prob > 0.5, 1, 0)

table(Predito = pred, Real = loan$loan_status)
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
