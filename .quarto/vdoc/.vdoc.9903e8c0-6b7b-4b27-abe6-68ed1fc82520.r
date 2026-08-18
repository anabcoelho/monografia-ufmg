#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
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
library(grid)
library(broom)

py_config()
reticulate::py_install(c("pandas", "tabulate", "ipython"))

set.seed(42)
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
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#| label: fig-function-logitica
#| fig-cap: "Função logística para um modelo com uma única variável preditora ($\\beta_0 = 0$ e $\\beta_1 = 1$)."
#| warning: false
#| fig-width: 18
#| fig-height: 12
#| out-width: "100%"
#| fig-align: "center"
x <- seq(-8, 8, length.out = 500)

dados <- data.frame(
  x = x,
  prob = 1 / (1 + exp(-x))
)

ggplot(dados, aes(x, prob)) +
  geom_line(linewidth = 1.2) +
  geom_hline(
    yintercept = c(0, 0.5, 1),
    linetype = c("dashed", "dotted", "dashed"),
    alpha = 0.5
  ) +
  geom_vline(
    xintercept = 0,
    linetype = "dotted",
    alpha = 0.5
  ) +
  labs(
    x = expression(eta),
    y = expression(P(Y == 1 ~ "|" ~ eta))
  ) +
  coord_cartesian(ylim = c(0, 1)) +
  theme_minimal(base_size = 14) +
  theme(
    axis.title = element_text(size = 22),
    axis.text = element_text(size = 20)
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
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#| label: fig-regularizacao
#| fig-cap: "Representação geométrica das regiões de penalização Ridge (L2) e Lasso (L1). A região circular associada à penalização L2 tende a reduzir simultaneamente a magnitude dos coeficientes, enquanto a região em formato de losango da penalização L1 favorece soluções em que alguns coeficientes assumem valor exatamente igual a zero."
#| warning: false
#| fig-width: 8
#| fig-height: 6
#| out-width: "80%"
#| fig-align: "center"
library(ggplot2)

theta <- seq(0, 2*pi, length.out = 500)

ridge <- data.frame(
  x = cos(theta),
  y = sin(theta)
)

lasso <- data.frame(
  x = c(-1,0,1,0,-1),
  y = c(0,1,0,-1,0)
)

ggplot() +
  geom_path(
    data = ridge,
    aes(x, y),
    linewidth = 1
  ) +
  geom_path(
    data = lasso,
    aes(x, y),
    linewidth = 1
  ) +
  coord_equal() +
  labs(
    x = expression(beta[1]),
    y = expression(beta[2]),
    title = "Regiões de penalização Ridge (L2) e Lasso (L1)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.title = element_text(size = 22),
    axis.text = element_text(size = 20)
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
#| label: fig-prior-normal
#| fig-cap: "Distribuições Normais a priori com diferentes desvios padrão."
#| fig-width: 7
#| fig-height: 4.5
#| fig-align: center
#| out-width: "75%"

library(ggplot2)

x <- seq(-8, 8, length.out = 500)

df <- rbind(
  data.frame(x = x,
             dens = dnorm(x, 0, 0.5),
             sigma = "σ = 0.5"),
  data.frame(x = x,
             dens = dnorm(x, 0, 2),
             sigma = "σ = 2"),
  data.frame(x = x,
             dens = dnorm(x, 0, 5),
             sigma = "σ = 5")
)

ggplot(df, aes(x, dens, linetype = sigma)) +
  geom_line(linewidth = 1.1) +
  labs(
    x = expression(beta[j]),
    y = "Densidade",
    linetype = NULL
  ) +
  theme_minimal(base_size = 15) +
  theme(
    legend.position = "top",
    legend.text = element_text(size = 13),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12)
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
#| label: fig-prior-laplace
#| fig-cap: "Distribuições Laplace a priori com diferentes parâmetros de escala."
#| fig-width: 7
#| fig-height: 4.5
#| fig-align: center
#| out-width: "75%"

x <- seq(-8,8,length.out = 500)

laplace <- function(x, mu = 0, b = 1){
  1/(2*b) * exp(-abs(x-mu)/b)
}

df <- rbind(
  data.frame(x=x,
             dens=laplace(x,b=.5),
             b="b = 0.5"),
  data.frame(x=x,
             dens=laplace(x,b=2),
             b="b = 2"),
  data.frame(x=x,
             dens=laplace(x,b=5),
             b="b = 5")
)

ggplot(df,aes(x,dens,linetype=b))+
  geom_line(linewidth=1.1)+
  labs(
    x=expression(beta[j]),
    y="Densidade",
    linetype=NULL
  )+
  theme_minimal(base_size = 15)+
  theme(
    legend.position="top"
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
#| label: fig-traceplots
#| fig-cap: "Exemplo ilustrativo de traceplots. À esquerda, cadeias convergidas apresentam boa mistura e exploram a mesma distribuição posterior. À direita, cadeias não convergidas permanecem separadas, indicando problemas de convergência."
#| fig-width: 8
#| fig-height: 4
#| out-width: "90%"
#| warning: false
#| message: false

set.seed(123)

iter <- 1:1000

# --------------------------
# Cadeias convergidas
# --------------------------
good <- do.call(rbind, lapply(1:4, function(ch){
  y <- cumsum(rnorm(1000, 0, 0.05))
  y <- scale(y)[,1]*0.3 + rnorm(1000,0,0.05)
  data.frame(
    iter = iter,
    beta = y,
    chain = factor(ch)
  )
}))

p1 <- ggplot(good,
             aes(iter, beta, colour = chain)) +
  geom_line(linewidth = .4) +
  labs(
    title = "Convergência",
    x = "Iteração",
    y = expression(beta)
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust=.5)
  )

# --------------------------
# Cadeias sem convergência
# --------------------------
bad <- do.call(rbind, lapply(1:4, function(ch){

  mu <- c(-3,-1,1,3)[ch]

  data.frame(
    iter = iter,
    beta = mu + cumsum(rnorm(1000,0,.02)),
    chain = factor(ch)
  )
}))

p2 <- ggplot(bad,
             aes(iter, beta, colour = chain)) +
  geom_line(linewidth=.4) +
  labs(
    title = "Sem convergência",
    x = "Iteração",
    y = expression(beta)
  ) +
  theme_minimal() +
  theme(
    legend.position="none",
    plot.title = element_text(hjust=.5)
  )

p1 + p2
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
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
default <- default %>%
  rename(
    id_cliente = ID,
    limite_credito = LIMIT_BAL,
    sexo = SEX,
    escolaridade = EDUCATION,
    estado_civil = MARRIAGE,
    idade = AGE,
    atraso_pagto_0 = PAY_0,
    atraso_pagto_2 = PAY_2,
    atraso_pagto_3 = PAY_3,
    atraso_pagto_4 = PAY_4,
    atraso_pagto_5 = PAY_5,
    atraso_pagto_6 = PAY_6,
    fatura_valor_1 = BILL_AMT1,
    fatura_valor_2 = BILL_AMT2,
    fatura_valor_3 = BILL_AMT3,
    fatura_valor_4 = BILL_AMT4,
    fatura_valor_5 = BILL_AMT5,
    fatura_valor_6 = BILL_AMT6,
    pagto_valor_1 = PAY_AMT1,
    pagto_valor_2 = PAY_AMT2,
    pagto_valor_3 = PAY_AMT3,
    pagto_valor_4 = PAY_AMT4,
    pagto_valor_5 = PAY_AMT5,
    pagto_valor_6 = PAY_AMT6,
    PD30 = default.payment.next.month
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
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#

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
#| label: fig-distribuicoes
#| fig-cap: "Distribuição das principais variáveis numéricas do conjunto de dados"
#| fig-width: 7.5
#| fig-height: 9.5
#| fig-align: center
#| out-width: "100%"
#| 
# 2. Seu vetor com as 8 variáveis numéricas reais
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

# 3. Tema Claro Customizado
tema_claro <- theme(
  panel.background = element_rect(fill = "#F8F9FA", color = NA),
  plot.background = element_rect(fill = "#FFFFFF", color = NA),
  panel.grid.major = element_line(color = "#E9ECEF", linewidth = 0.5),
  panel.grid.minor = element_line(color = "#F1F3F5", linewidth = 0.25),
  axis.text = element_text(color = "#495057", size = 9),
  axis.title = element_blank(), 
  plot.title = element_text(color = "#212529", hjust = 0.5, size = 11, face = "bold"),
  axis.ticks = element_line(color = "#CED4DA")
)

# 4. Função para criar os histogramas (usando o nome da coluna como título automático)
criar_histograma <- function(data, coluna) {
  ggplot(data, aes(x = .data[[coluna]])) +  
    geom_histogram(bins = 30, fill = NA, color = "#495057", linewidth = 0.4) + 
    # scale_x_continuous formata os números gigantes para o padrão "10M", "20M", etc.
    scale_x_continuous(labels = label_number(scale_cut = cut_short_scale())) + 
    labs(title = coluna) + 
    tema_claro
}

# 5. AUTOMAÇÃO: Gera uma lista contendo os 8 gráficos de uma vez só
# O lapply passa por cada nome de coluna dentro de 'vars_numericas' e aplica a função no seu objeto 'loan'
lista_de_graficos <- lapply(vars_numericas, function(col) {
  criar_histograma(loan, col)
})

# 6. Juntar todos os gráficos da lista em uma grade automática
# ncol = 3 organiza em 3 colunas (ficará uma grade de 3x3, deixando o último espaço em branco já que são 8 gráficos)
grade_graficos <- wrap_plots(lista_de_graficos, ncol = 3)

# 7. Criar o texto da lateral esquerda
texto_lateral <- wrap_elements(
  panel = textGrob(
    "Percentual de Ocorrências", 
    rot = 90, 
    gp = gpar(fontface = "plain", col = "#212529", fontsize = 12)
  )
)

# 8. Juntar o texto com a grade final de 8 gráficos
layout_final <- texto_lateral + grade_graficos + plot_layout(widths = c(1, 25))

# Mostrar o resultado na tela
layout_final


#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
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


n_obs <- nrow(default)
n_vars <- ncol(default)
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#| label: fig-distribuicoes-default
#| fig-cap: "Distribuição das principais variáveis numéricas do conjunto de dados"
#| fig-width: 7.5
#| fig-height: 9.5
#| fig-align: center
#| out-width: "100%"
#| 
# 2. Vetor com as suas 14 variáveis numéricas de crédito
vars_numericas <- c(
  "limite_credito",
  "idade",
  "fatura_valor_1",
  "fatura_valor_2",
  "fatura_valor_3",
  "fatura_valor_4",
  "fatura_valor_5",
  "fatura_valor_6",
  "pagto_valor_1",
  "pagto_valor_2",
  "pagto_valor_3",
  "pagto_valor_4",
  "pagto_valor_5",
  "pagto_valor_6"
)



# 5. Criando a lista com os 14 gráficos de uma vez só
lista_de_graficos <- lapply(vars_numericas, function(col) {
  criar_histograma(default, col)
})

# 6. Agrupando os 14 gráficos em uma grade de 3 colunas
# Como são 14 gráficos, ele criará uma grade organizada de 5 linhas por 3 colunas
grade_graficos <- wrap_plots(lista_de_graficos, ncol = 3)

# 7. Criando o texto da lateral esquerda ("Percentual de Ocorrências")
texto_lateral <- wrap_elements(
  panel = textGrob(
    "Percentual de Ocorrências", 
    rot = 90, 
    gp = gpar(fontface = "plain", col = "#212529", fontsize = 12)
  )
)

# 8. Juntando o texto lateral esquerdo com a grande grade de gráficos
layout_final <- texto_lateral + grade_graficos + plot_layout(widths = c(1, 25))

# 9. Plota o layout final unificado na página
layout_final
```
#
# 10. Suas transformações de fatores (mantidas exatamente iguais)
default <- default %>%
  mutate(
    sexo = factor(
      sexo,
      levels = c(1, 2),
      labels = c("Masculino", "Feminino")
    ),
    escolaridade = factor(
      escolaridade,
      levels = c(1, 2, 3, 4, 5, 6),
      labels = c(
        "PosGraduacao",
        "Universidade",
        "EnsinoMedio",
        "Outros",
        "Desconhecido1",
        "Desconhecido2"
      )
    ),
    estado_civil = factor(
      estado_civil,
      levels = c(1, 2, 3),
      labels = c("Casado", "Solteiro", "Outros")
    ),
    PD30 = factor(PD30, levels = c(0, 1))
  )

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
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#| label: fig-target-plot-default
#| fig-cap: "Distribuição da Inadimplência"
#| fig-width: 4
#| fig-height: 3

default |>
  count(PD30) |>
  mutate(pct = n / sum(n)) |>
  ggplot(aes(x = factor(PD30), y = pct)) +
  geom_col() +
  scale_y_continuous(labels = scales::percent) +
  labs(
    x = "Inadimplência (PD30)",
    y = "Percentual"
  )

target_tab <- default |>
  dplyr::count(PD30) |>
  dplyr::mutate(
    pct = n / sum(n) * 100
  )

positive_pct <- round(
  target_tab$pct[target_tab$PD30 == 1], 2
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
#| label: fig-correlacao-default
#| fig-cap: "Mapa de calor da correlação entre variáveis numéricas do conjunto de dados de crédito"
#| warning: false
#| fig-width: 18
#| fig-height: 12
#| out-width: "100%"
#| fig-align: "center"
library(grid)
default_num <- default %>%
  dplyr::select(
    limite_credito,
    idade,
    atraso_pagto_0, atraso_pagto_2, atraso_pagto_3,
    atraso_pagto_4, atraso_pagto_5, atraso_pagto_6,
    fatura_valor_1, fatura_valor_2, fatura_valor_3,
    fatura_valor_4, fatura_valor_5, fatura_valor_6,
    pagto_valor_1, pagto_valor_2, pagto_valor_3,
    pagto_valor_4, pagto_valor_5, pagto_valor_6
  )

cor_matrix <- round(cor(default_num, use = "complete.obs"), 3)

ggcorrplot::ggcorrplot(
  cor_matrix,
  type = "lower",
  lab = TRUE,
  lab_size = 5
) +
  theme_minimal(base_size = 23) +
  theme(
    axis.text.x = element_text(
      angle = 90,
      hjust = 1,
      vjust = 0.5,
      size = 22
    ),
    axis.text.y = element_text(size = 22),

    # 👇 AQUI está o que você quer
    legend.text = element_text(size = 18),   # números da barra de cor
    legend.title = element_text(size = 20, face = "bold"),
    legend.key.size = unit(1.2, "cm"),       # aumenta a “caixinha” da escala

    plot.title = element_text(size = 18, face = "bold")
  ) +
  coord_fixed(ratio = 0.7)
  

#
#
#
#
#
#
#
#
#
#
#| label: tbl-outliers-default
#| tbl-cap: "Número e percentual de outliers identificados pelo critério do IQR no conjunto de dados de crédito"
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

# Variáveis numéricas do default (PD30)
vars_numericas_default <- default %>%
  select(
    limite_credito,
    idade,
    atraso_pagto_0, atraso_pagto_2, atraso_pagto_3,
    atraso_pagto_4, atraso_pagto_5, atraso_pagto_6,
    fatura_valor_1, fatura_valor_2, fatura_valor_3,
    fatura_valor_4, fatura_valor_5, fatura_valor_6,
    pagto_valor_1, pagto_valor_2, pagto_valor_3,
    pagto_valor_4, pagto_valor_5, pagto_valor_6
  )

# Processamento dos dados
outlier_df <- vars_numericas_default %>%
  pivot_longer(everything(), names_to = "Feature", values_to = "value") %>%
  group_by(Feature) %>%
  summarise(
    Outliers = iqr_outliers(value),
    Percentual = Outliers / nrow(default)
  ) %>%
  ungroup()

# Tabela final
knitr::kable(outlier_df)

#
#
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
#
#
#
#
#
split_loan <- initial_split(
  loan,
  prop   = 0.75,
  strata = status
)

train_data_loan <- training(split_loan)
test_data_loan  <- testing(split_loan)

split_default <- initial_split(
  default,
  prop   = 0.75,
  strata = PD30
)

train_data_default <- training(split_default)
test_data_default  <- testing(split_default)
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
log_glm <- logistic_reg() |>
  set_engine("glm") |>
  set_mode("classification")
#lasso
log_l1 <- logistic_reg(
  penalty = tune(),
  mixture = 1
) |>
  set_engine("glmnet") |>
  set_mode("classification")

log_l2 <- logistic_reg(
  penalty = tune(),
  mixture = 0
) |>
  set_engine("glmnet") |>
  set_mode("classification")


folds <- vfold_cv(train_data_loan, v = 5, strata = status)
grid <- grid_regular(
  penalty(range = c(-6, 1)),
  levels = 20
)
metrics <- metric_set(
  accuracy,
  roc_auc,
  f_meas
)
#
#
#
#
rec_all <- recipe(status ~ ., data = train_data_loan) %>%
  step_dummy(all_nominal_predictors())


rec_def <- recipe(status ~ ., data = train_data_loan) %>%
  step_dummy(all_nominal_predictors()) %>%
  step_normalize(all_numeric_predictors())

rec_score_only <- recipe(
  status ~ score_credito,
  data = train_data_loan
)%>%
  step_dummy(all_nominal_predictors())

rec_no_score <- recipe(
  status ~ .,
  data = train_data_loan
) %>%
  step_dummy(all_nominal_predictors())%>%
  step_rm(score_credito)

rec_no_corr <- recipe(
  status ~ .,
  data = train_data_loan
) %>%
  step_dummy(all_nominal_predictors())%>%
  step_corr(all_numeric_predictors(), threshold = 0.75)

#
#
#
wf_logit <- workflow_set(
  preproc = list(
    all_features = rec_all,
    score_only   = rec_score_only,
    no_score     = rec_no_score,
    no_corr      = rec_no_corr
  ),
  models = list(
    logit = log_glm
  )
)

wf_l1 <- workflow() %>%
  add_recipe(rec_def) %>%
  add_model(log_l1)

tuned_l1 <- tune_grid(
  wf_l1,
  resamples = folds,
  grid = grid,
  metrics = metrics
)

wf_l2 <- workflow() %>%
  add_recipe(rec_def) %>%
  add_model(log_l2)

tuned_l2 <- tune_grid(
  wf_l2,
  resamples = folds,
  grid = grid,
  metrics = metrics
)
```
#
best_l1 <- select_best(tuned_l1, metric = "roc_auc")
final_l1 <- finalize_workflow(wf_l1, best_l1)
fit_l1 <- fit(final_l1, train_data_loan)

best_l2 <- select_best(tuned_l2, metric = "roc_auc")
final_l2 <- finalize_workflow(wf_l2, best_l2)
fit_l2 <- fit(final_l2, train_data_loan)
#
#
#
#vamos por aqui? 

coef_table <- extract_fit_engine(fit_l1) |>
  coef(s = best_l1$penalty)
kable(coef_table)

coef_table <- extract_fit_engine(fit_l2) |>
  coef(s = best_l2$penalty)
kable(coef_table)
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
