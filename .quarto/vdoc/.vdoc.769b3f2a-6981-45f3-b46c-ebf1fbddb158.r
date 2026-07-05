#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
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

#pkgs <- c(
  #"tidyverse",
  #"janitor",
  #"skimr",
  #"reticulate",
  #"gtsummary",
  #"patchwork",
  #'ggcorrplot',
  #'fastDummies',
  #'dplyr',
  #'pROC',
  #'caret', 
  #'PRROC',
  #'gt',
  #'broom',
  #'kableExtra'
#)
pkgs <- c(
  "tidymodels",
  'glmnet',
  "rstanarm", 
  "reticulate",
  'yardstick'
)

instalar <- pkgs[!pkgs %in% installed.packages()[, "Package"]]

if (length(instalar) > 0) {
  install.packages(instalar)
}
#library(kableExtra)

#library(tidyverse)
#library(janitor)
#library(skimr)

#library(dplyr)
#library(tidyr)
#library(janitor)

#library(ggcorrplot)
#library(dplyr)
#library(fastDummies)
#library(PRROC)
#library(pROC)
library(caret)
#library(gt)
#library(broom)
#library(dplyr)
#library(yardstick) recipies

library(tidyverse)
library("tidymodels")
library('janitor')
library('ggcorrplot')
library(workflowsets)
library(rstanarm)
library(glmnet)
library(reticulate)
library(ggplot2)
library(patchwork)
library(dplyr)

py_config()
reticulate::py_install(c("pandas", "tabulate", "ipython"))

set.seed(123)
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
#
#
#
#
#
#
#
loan <- read_csv("data/Loan/loan_approval_dataset.csv") |>
  clean_names()

#https://www.kaggle.com/datasets/uciml/default-of-credit-card-clients-dataset
default <- read_csv("data/default/UCI_Credit_Card.csv")

#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
loan <- loan %>%
  rename(
    num_dependentes      = no_of_dependents,
    escolaridade         = education,
    autonomo             = self_employed,
    renda_anual          = income_annum,
    valor_emprestimo     = loan_amount,
    prazo_anos           = loan_term,
    score_credito        = cibil_score,
    ativos_residenciais  = residential_assets_value,
    ativos_comerciais    = commercial_assets_value,
    ativos_luxo          = luxury_assets_value,
    ativos_bancarios     = bank_asset_value,
    status    = loan_status
)


loan$status <- ifelse(
  loan$status %in% c("Approved", 1, "1"),
  1, 0
)
loan$status <- as.factor(loan$status)

#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
default
default <- default %>%
  rename(
    id_cliente = ID,
    limite_credito = LIMIT_BAL,
    sexo = SEX,
    escolaridade = EDUCATION,
    estado_civil = MARRIAGE,
    idade = AGE,
    atraso_pagamento_0 = PAY_0,
    atraso_pagamento_2 = PAY_2,
    atraso_pagamento_3 = PAY_3,
    atraso_pagamento_4 = PAY_4,
    atraso_pagamento_5 = PAY_5,
    atraso_pagamento_6 = PAY_6,
    fatura_valor_1 = BILL_AMT1,
    fatura_valor_2 = BILL_AMT2,
    fatura_valor_3 = BILL_AMT3,
    fatura_valor_4 = BILL_AMT4,
    fatura_valor_5 = BILL_AMT5,
    fatura_valor_6 = BILL_AMT6,
    pagamento_valor_1 = PAY_AMT1,
    pagamento_valor_2 = PAY_AMT2,
    pagamento_valor_3 = PAY_AMT3,
    pagamento_valor_4 = PAY_AMT4,
    pagamento_valor_5 = PAY_AMT5,
    pagamento_valor_6 = PAY_AMT6,
    inadimplencia_proximo_mes = default.payment.next.month
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
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
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

vars_numericas <- c(
  "renda_anual",
  "valor_emprestimo",
  "prazo_anos",
  "score_credito",
  "ativos_residenciais",
  "ativos_comerciais",
  "ativos_luxo",
  "ativos_bancarios"
)

for (v in vars_numericas) {
  
  p <- ggplot(loan, aes(x = .data[[v]])) +
    geom_histogram(bins = 30, fill = "steelblue", color = "white") +
    theme_minimal() +
    labs(
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
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
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

target_tab <- loan |>
  dplyr::count(status) |>
  dplyr::mutate(
    pct = n / sum(n) * 100
  )

positive_pct <- round(
  target_tab$pct[target_tab$status == 1], 2
)

negative_pct <- round(
  target_tab$pct[target_tab$status == 0], 2
)
#
#
#
#
#
#| label: fig-target-plot
#| fig-cap: "Distribuição da aprovação de empréstimos"
#| fig-width: 4
#| fig-height: 3

loan |>
  count(status) |>
  mutate(pct = n / sum(n)) |>
  ggplot(aes(x = factor(status), y = pct)) +
  geom_col() +
  scale_y_continuous(labels = scales::percent) +
  labs(
    x = "Status do Empréstimo",
    y = "Percentual"
  )
#
#
#
#
#
# removi do texto, mantendo o código apenas para minha análise
loan |> 
  count(escolaridade, status) |> 
  group_by(escolaridade) |> 
  mutate(rate = n / sum(n))
loan |> 
  ggplot(aes(x = renda_anual, fill = factor(status))) +
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
#
#
#
#
#| label: fig-distribuicoes-target
#| fig-cap: "Distribuição das variáveis numéricas por status de aprovação do empréstimo"
#| fig-width: 14
#| fig-height: 10

loan_long <- loan %>%
  select(all_of(vars_numericas), status) %>%
  pivot_longer(
    cols = all_of(vars_numericas),
    names_to = "variavel",
    values_to = "valor"
  )

plot_vars <- function(vars_subset) {
  loan_long %>%
    filter(variavel %in% vars_subset) %>%
    ggplot(aes(x = valor, fill = status)) +
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

p1 <- plot_vars(vars_numericas[1:4])
p2 <- plot_vars(vars_numericas[5:8])

(p1 / p2) +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")
#
#
#
#
#
#| label: fig-categoricas
#| fig-cap: "Distribuição das variáveis categóricas segundo o status do empréstimo"
#| fig-width: 12
#| fig-height: 8

loan <- loan %>% 
  clean_names() %>%
  mutate(
    status = factor(status, labels = c("Rejeitado", "Aprovado")),
    num_dependentes = factor(
      num_dependentes,
      levels = sort(unique(num_dependentes)),  
      ordered = TRUE
    )
  )

plot_cat <- function(var, xlab) {
  ggplot(
    loan,
    aes(x = .data[[var]], fill = status)
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

p1 <- plot_cat("escolaridade", "Nível educacional")
p2 <- plot_cat("autonomo", "Trabalho autônomo")
p3 <- plot_cat("num_dependentes", "Número de dependentes")

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
    vars_numericas
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
#
#
#
#
#
#| label: tbl-outliers
#| tbl-cap: "Número e percentual de outliers identificados pelo critério do IQR"
#| echo: false
#| warning: false

library(dplyr)
library(tidyr)
library(knitr)

# Função para contar outliers
iqr_outliers <- function(x) {
  Q1 <- quantile(x, 0.25, na.rm = TRUE)
  Q3 <- quantile(x, 0.75, na.rm = TRUE)
  IQR_val <- Q3 - Q1
  lower <- Q1 - 1.5 * IQR_val
  upper <- Q3 + 1.5 * IQR_val
  sum(x < lower | x > upper, na.rm = TRUE)
}

# Processamento dos dados
outlier_df <- loan %>%
  select(all_of(vars_numericas)) %>%
  pivot_longer(everything(), names_to = "Feature", values_to = "value") %>%
  group_by(Feature) %>%
  summarise(
    Outliers = iqr_outliers(value),
    # Criamos como número puro para formatar no GT depois
    Percentual = (Outliers / nrow(loan)) 
  ) %>%
  ungroup()

# Gerando a tabela
kable(outlier_df)
#
#
#
#
#
#apenas para consulta
summary(loan)
colSums(is.na(loan))
sum(duplicated(loan))
#
#
#
#
#
#
#


negativos <- sum(loan$ativos_residenciais < 0)
total <- nrow(loan)

percentual <- negativos / total 
percentual
negativos

# tratamento para os valores negativos
n_before <- nrow(loan)

loan <- loan |>
  dplyr::filter(ativos_residenciais >= 0)

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
#
names(loan)
vars_categoricas <- c(
  "escolaridade",
  "autonomo"
)

# One-Hot Encoding
loan_encoded <- loan %>%
  mutate(
    escolaridade = factor(escolaridade),
    autonomo = factor(autonomo)
  ) %>%
  fastDummies::dummy_cols(
    select_columns = vars_categoricas,
    remove_selected_columns = TRUE,
    remove_first_dummy = TRUE
  )

table(loan_encoded$status)
str(loan_encoded$status)
#
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

#
#
#
#
#| include: false

#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#

#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
