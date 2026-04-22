#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
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
  'ggcorrplot',
  'fastDummies',
  'dplyr'
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
library(dplyr)
library(fastDummies)


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
# Percentuais no_of_dependents
dep_tab <- resumo_cat |>
  dplyr::filter(variavel == "no_of_dependents")

# Educação
edu_grad <- resumo_cat |>
  dplyr::filter(variavel == "education", categoria == "Graduate") |>
  dplyr::pull(`percentual (%)`)

edu_not_grad <- resumo_cat |>
  dplyr::filter(variavel == "education", categoria == "Not Graduate") |>
  dplyr::pull(`percentual (%)`)

# Self employed
self_yes <- resumo_cat |>
  dplyr::filter(variavel == "self_employed", categoria == "Yes") |>
  dplyr::pull(`percentual (%)`)

self_no <- resumo_cat |>
  dplyr::filter(variavel == "self_employed", categoria == "No") |>
  dplyr::pull(`percentual (%)`)
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
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
#
#
#
#
#
#
#| label: fig-correlacao
#| fig-cap: "Mapa de calor da correlação entre variáveis numéricas"
#| warning: false
#| fig-width: 7
#| fig-height: 5
#| fig-pos: "htbp"

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
    axis.text.y = element_text(size = 8)
  )
#
#
#
#
#
#
#
#
#| label: fig-boxplots-target
#| fig-cap: "Boxplots das variáveis numéricas por status de aprovação do empréstimo"
#| fig-width: 14
#| fig-height: 20

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
  mutate(
    loan_status = factor(
      loan_status,
      labels = c("Rejeitado", "Aprovado")
    )
  ) %>%
  pivot_longer(
    cols = all_of(vars),
    names_to = "variavel",
    values_to = "valor"
  )

ggplot(loan_long, aes(x = loan_status, y = valor, fill = loan_status)) +
  geom_boxplot(
    alpha = 0.6,
    outlier.shape = 16,      
    outlier.size = 1.5,
    outlier.color = "red",  
    outlier.alpha = 0.7
  ) +
  facet_wrap(~ variavel, scales = "free_y", ncol = 2) +
  scale_fill_manual(
    values = c("Rejeitado" = "#d95f02", "Aprovado" = "#1b9e77")
  ) +
  scale_y_continuous(labels = scales::comma) +
  theme_minimal(base_size = 16) +
  theme(
    legend.position = "none",
    strip.text = element_text(size = 14),
    axis.title = element_blank(),
    axis.text.x = element_text(size = 13),
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
#| label: fig-categoricas
#| fig-cap: "Distribuição das variáveis categóricas segundo o status do empréstimo"
#| fig-width: 12
#| fig-height: 8

library(tidyverse)
library(janitor)
library(patchwork)

loan <- loan %>% 
  clean_names() %>%
  mutate(
    loan_status = factor(loan_status, labels = c("Rejeitado", "Aprovado")),
    no_of_dependents = factor(
      no_of_dependents,
      levels = sort(unique(no_of_dependents)),  # garante ordem 0,1,2,...
      ordered = TRUE
    )
  )

plot_cat <- function(var, xlab) {
  ggplot(
    loan,
    aes(x = .data[[var]], fill = loan_status)
  ) +
    geom_bar(position = "dodge") +
    scale_fill_manual(values = c("#d95f02", "#1b9e77")) +
    theme_minimal(base_size = 15) +
    theme(
      legend.position = "bottom",
      axis.text = element_text(size = 13),
      axis.title.x = element_text(size = 14),
      axis.title.y = element_text(size = 14),
      legend.text = element_text(size = 13),
      legend.title = element_text(size = 14)
    ) +
    labs(
      x = xlab,
      y = "Número de observações",
      fill = "Status do Empréstimo"
    )
}

p1 <- plot_cat("education", "Nível educacional")
p2 <- plot_cat("self_employed", "Trabalho autônomo")
p3 <- plot_cat("no_of_dependents", "Número de dependentes")

(p1 + p2) / p3
#
#
#
#
#
#
#
#
#
#apenas para consulta
summary(loan)
colSums(is.na(loan))
#
#
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
names(loan)
#
#
#

vars_categoricas <- c(
  "education",
  "self_employed"
)

vars_numericas <- c(
  "no_of_dependents",
  "income_annum",
  "loan_amount",
  "loan_term",
  "cibil_score",
  "residential_assets_value",
  "commercial_assets_value",
  "luxury_assets_value",
  "bank_asset_value"
)

# One-Hot Encoding

loan_encoded <- loan %>%
  mutate(
    education = factor(education),
    self_employed = factor(self_employed)
  ) %>%
  fastDummies::dummy_cols(
    select_columns = vars_categoricas,
    remove_selected_columns = TRUE,
    remove_first_dummy = TRUE
  )

# Padronização (Standardization)
loan_encoded[vars_numericas] <- lapply(
  loan_encoded[vars_numericas],
  function(x) as.numeric(as.character(x))
)

loan_encoded[vars_numericas] <- scale(loan_encoded[vars_numericas])

#
#
#
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
