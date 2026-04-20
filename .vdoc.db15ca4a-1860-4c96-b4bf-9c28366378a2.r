#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
install.packages(c("tidyverse", "janitor", "skimr", 'reticulate', 'gtsummary'))
install.packages("patchwork")
library(reticulate)

library(tidyverse)
library(janitor)
library(skimr)


py_config()
reticulate::py_install("pandas")
reticulate::py_install("tabulate")
reticulate::py_install("IPython")
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
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
summary(loan)
dim(loan)
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
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
#| label: fig-target-plot
#| fig-cap: "Distribuição da aprovação de empréstimos"

loan |> 
  count(loan_status) |> 
  ggplot(aes(x = factor(loan_status), y = n)) +
  geom_col() +
  labs(
    x = "Status do Empréstimo",
    y = "Número de Observações"
  )
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
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
plot(cars)
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
