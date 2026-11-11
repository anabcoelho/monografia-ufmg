#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
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
  "reticulate","gt", 'Rtools',
  'recipes','kableExtra'
)

instalar <- pkgs[!pkgs %in% installed.packages()[, "Package"]]

if (length(instalar) > 0) {
  install.packages(instalar)
}

library(caret)
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
library(purrr)
library(yardstick)
library(tibble)
library(gt)
library(recipes)
library(kableExtra)
library(posterior)
library(knitr)

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
#https://www.kaggle.com/datasets/uciml/default-of-credit-card-clients-dataset
default <- read_csv("data/default/UCI_Credit_Card.csv")
#
#
#

calc_metrics_from_fit <- function(fit, test_data, truth_col = status) {

  # Predições
  prob_tbl <- predict(fit, test_data, type = "prob")
  class_tbl <- predict(fit, test_data, type = "class")

  truth <- test_data[[deparse(substitute(truth_col))]]

  # Garante a ordem das classes
  truth <- factor(truth, levels = c("0", "1"))
  class <- factor(class_tbl$.pred_class, levels = c("0", "1"))

  # Classe positiva
  positive_class <- "1"

  # Probabilidade da classe positiva
  prob <- prob_tbl[[paste0(".pred_", positive_class)]]

  # Validações
  stopifnot(
    !anyNA(truth),
    !anyNA(class),
    length(truth) == length(prob),
    all(is.finite(prob)),
    all(prob >= 0 & prob <= 1)
  )

  # Resposta em 0/1
  truth_num <- as.integer(truth == positive_class)

  # Evita log(0)
  eps <- 1e-15
  prob_clip <- pmin(pmax(prob, eps), 1 - eps)

  # Log Loss
  log_loss <- -mean(
    truth_num * log(prob_clip) +
      (1 - truth_num) * log(1 - prob_clip)
  )

  # Brier Score
  brier <- mean((prob - truth_num)^2)

  tibble(
    accuracy = accuracy_vec(truth, class),

    bal_accuracy = bal_accuracy_vec(truth, class),

    roc_auc = roc_auc_vec(
      truth, prob,
      event_level = "second"
    ),

    log_loss = log_loss,

    # brier = brier,

    sens = sens_vec(
      truth, class,
      event_level = "second"
    ),

    spec = spec_vec(
      truth, class,
      event_level = "second"
    ),

    precision = precision_vec(
      truth, class,
      event_level = "second"
    ),

    f1 = f_meas_vec(
      truth, class,
      event_level = "second"
    ),

    mcc = mcc_vec(truth, class)
  )
}


extract_model_summary <- function(wf_fit) {
  model_fit <- workflows::extract_fit_parsnip(wf_fit)

  if (inherits(model_fit$fit, "glm")) {
    return(summary(model_fit$fit))
  }

  if (inherits(model_fit$fit, "lognet")) {
    return(model_fit$fit)
  }

  NULL
}

get_model <- function(wf_fit) {
  workflows::extract_fit_parsnip(wf_fit)$fit
}
#
#
#
calc_metrics_bay <- function(truth, class, prob) {
stopifnot(
  identical(levels(truth), c("0", "1")),
  identical(levels(class), c("0", "1")),
  length(truth) == length(class),
  length(truth) == length(prob),
  all(is.finite(prob)),
  all(prob >= 0 & prob <= 1)
)

  y <- as.integer(as.character(truth))

  prob <- pmin(
    pmax(prob, 1e-15),
    1 - 1e-15
  )

  tibble(
    accuracy = accuracy_vec(truth, class),

    bal_accuracy = bal_accuracy_vec(truth, class),

    roc_auc = roc_auc_vec(
      truth,
      prob,
      event_level = "second"
    ),

    log_loss = -mean(
      y * log(prob) +
        (1 - y) * log(1 - prob)
    ),

    sens = sens_vec(
      truth, class,
      event_level = "second"
    ),

    spec = spec_vec(
      truth, class,
      event_level = "second"
    ),

    precision = precision_vec(
      truth, class,
      event_level = "second"
    ),

    f1 = f_meas_vec(
      truth, class,
      event_level = "second"
    ),

    mcc = mcc_vec(truth, class)
  )
}

calc_metrics_bayes <- function(fit, test_data, model_name) {

  prob <- posterior_epred(
    fit,
    newdata = test_data
  ) %>%
    colMeans()

  class <- factor(
    ifelse(prob >= 0.5, "1", "0"),
    levels = c("0", "1")
  )

  truth <- factor(
    test_data$PD30,
    levels = c("0", "1")
  )

  calc_metrics_bay(
    truth = truth,
    class = class,
    prob = prob
  ) %>%
    mutate(wflow_id = model_name)
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
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
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
default <- default %>%
  dplyr::select(-id_cliente)
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
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

# 4. Função para criar os histogramas (usando o nome da coluna como título automático)
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

criar_histograma <- function(data, coluna) {
  ggplot(data, aes(x = .data[[coluna]])) +  
    geom_histogram(bins = 30, fill = NA, color = "#495057", linewidth = 0.4) + 
    # scale_x_continuous formata os números gigantes para o padrão "10M", "20M", etc.
    scale_x_continuous(labels = label_number(scale_cut = cut_short_scale())) + 
    labs(title = coluna) + 
    tema_claro
}

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
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
split_default <- initial_split(
  default,
  prop   = 0.75,
  strata = PD30
)

train_data_default <- training(split_default)
test_data_default  <- testing(split_default)
```
#
# Moda calculada exclusivamente no conjunto de treinamento
moda_escolaridade <- names(
  which.max(table(train_data_default$escolaridade))
)

moda_estado_civil <- names(
  which.max(table(train_data_default$estado_civil))
)

# Imputação pela moda no conjunto de treinamento
train_data_default <- train_data_default %>%
  mutate(
    escolaridade = forcats::fct_na_value_to_level(
      escolaridade,
      level = moda_escolaridade
    ),
    estado_civil = forcats::fct_na_value_to_level(
      estado_civil,
      level = moda_estado_civil
    )
  )

# Aplicação das mesmas modas no conjunto de teste
test_data_default <- test_data_default %>%
  mutate(
    escolaridade = forcats::fct_na_value_to_level(
      escolaridade,
      level = moda_escolaridade
    ),
    estado_civil = forcats::fct_na_value_to_level(
      estado_civil,
      level = moda_estado_civil
    )
  )
#
#
#
#
#
#
colSums(is.na(default))

```
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

folds <- vfold_cv(train_data_default, v = 5, strata = PD30)

calc_vif <- function(wf_fit) {
  wf_fit %>%
    workflows::extract_fit_parsnip() %>%
    pluck("fit") %>%      # objeto glm
    car::vif()
}
```
#
#modelo geral
rec_default  <- recipe(PD30 ~ ., data = train_data_default)%>%
  step_impute_mode(
    all_nominal_predictors()
  ) %>%
  step_dummy(all_nominal_predictors()) %>%
  step_normalize(all_numeric_predictors())
rec_all  <- recipe(PD30 ~ ., data = train_data_default)%>%
  step_impute_mode(
    all_nominal_predictors()
  ) %>%
  step_dummy(all_nominal_predictors()) 

# Removendo alta correlação
rec_corr <- recipe(
  PD30 ~ .,
  data = train_data_default
) %>%
  step_impute_mode(
    all_nominal_predictors()
  )%>% step_dummy(all_nominal_predictors()) %>%
  step_corr(all_numeric_predictors(), threshold = 0.7)


wf_logit_default <- workflow_set(
  preproc = list(
    all_features = rec_all,
    no_corr      = rec_corr
  ),
  models = list(
    logit = log_glm
  )
)

# Métricas utilizadas na validação cruzada
metrics <- metric_set(
  accuracy,
  bal_accuracy,
  roc_auc,
  mn_log_loss
)

# LASSO
wf_l1 <- workflow() %>%
  add_recipe(rec_default) %>%
  add_model(log_l1)

tuned_l1 <- tune_grid(
  wf_l1,
  resamples = folds,
  grid = 30,
  metrics = metrics
)

# Ridge
wf_l2 <- workflow() %>%
  add_recipe(rec_default) %>%
  add_model(log_l2)

tuned_l2 <- tune_grid(
  wf_l2,
  resamples = folds,
  grid = 30,
  metrics = metrics
)

show_notes(tuned_l1)

#
#
#
#
best_l1 <- select_best(tuned_l1, metric = "roc_auc")
final_l1 <- finalize_workflow(wf_l1, best_l1)
fit_l1 <- fit(final_l1, train_data_default)

best_l2 <- select_best(tuned_l2, metric = "roc_auc")
final_l2 <- finalize_workflow(wf_l2, best_l2)
fit_l2 <- fit(final_l2, train_data_default)
# depende se mostraremos os coefs de L1 e L2
coef_table <- extract_fit_engine(fit_l1) |>
  coef(s = best_l1$penalty)
coef_table
coef_table <- extract_fit_engine(fit_l2) |>
  coef(s = best_l2$penalty)
coef_table
#
#
#
fits <- wf_logit_default %>%
  mutate(
    fit = purrr::map(
      info,
      ~ fit(.x$workflow[[1]], data = train_data_default)
    )
  )

preds <- fits %>%
  mutate(
    preds = purrr::map(
      fit,
      ~{
        prob_tbl <- predict(.x, test_data_default, type = "prob")

        tibble(
          truth = test_data_default$PD30,
          class = predict(.x, test_data_default, type = "class")$.pred_class,
          prob  = prob_tbl$.pred_1
        )
      }
    )
  ) %>%
  select(wflow_id, preds)

preds <- preds %>%
  tidyr::unnest(preds)

metrics_wf <- fits %>%
  mutate(
    metrics = purrr::map(
      fit,
      ~ calc_metrics_from_fit(
        fit = .x,
        test_data = test_data_default,
        truth_col = PD30
      )
    )
  ) %>%
  select(wflow_id, metrics) %>%
  tidyr::unnest(metrics)

metrics_l1 <- calc_metrics_from_fit(
  fit = fit_l1,
  test_data = test_data_default,
  truth_col = PD30
) %>%
  mutate(wflow_id = "l1_logit")
  
metrics_l2 <- calc_metrics_from_fit(
  fit = fit_l2,
  test_data = test_data_default,
  truth_col = PD30
) %>%
  mutate(wflow_id = "l2_logit")

fits_vif <- fits %>%
  dplyr::filter(
    wflow_id %in% c(
      "all_features_logit",
      "no_corr_logit"
    )
  )

metrics_table_final <- bind_rows(
  metrics_wf,
  metrics_l1,
  metrics_l2
)

vif_results <- fits_vif %>%
  mutate(
    vif = purrr::map(fit, calc_vif)
  )
vif_tidy <- vif_results %>%
  select(wflow_id, vif) %>%
  tidyr::unnest_longer(vif, values_to = "vif") %>%
  rename(variable = vif_id)

vif_summary <- vif_tidy %>%
  group_by(wflow_id) %>%
  summarise(
    max_vif = max(vif),
    mean_vif = mean(vif)
  )
metrics_table_final
```
#
#| label: tbl-metricas-modelos-pd30
metrics_table_final %>%
  rename(
    Modelo  = wflow_id,
    Acc     = accuracy,
    BAcc  = bal_accuracy,
    AUC     = roc_auc,
    LLoss = log_loss,
    Sens    = sens,
    Spec    = spec,
    Prec    = precision,
    F1      = f1,
    MCC     = mcc
  ) %>%
  kable(digits = 2)
```
#
#| label: tbl-vif-modelos-pd30
kable(vif_summary, digits = 2, align = "lccccccccc")
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
coef_tables <- fits %>%
  transmute(
    wflow_id,
    coef = map(
      fit,
      ~ tidy(extract_fit_parsnip(.x)$fit)
    )
  )

coef_all <- coef_tables %>%
  filter(wflow_id == "no_corr_logit") %>%
  pull(coef) %>%
  .[[1]]



coef_all %>%
  mutate(
    estimate = round(estimate,4),
    std.error = round(std.error,4),
    statistic = round(statistic,2),
    p.value = scales::pvalue(p.value)
  ) %>%
  gt() %>%
  cols_label(
    term = "Variável",
    estimate = "Coeficiente",
    std.error = "Erro padrão",
    statistic = "z",
    p.value = "p-valor"
  )

coef_all <- coef_all %>%
  mutate(
    OR = exp(estimate)
  )

fit_glm <- fits %>%
  filter(wflow_id == "no_corr_logit") %>%
  pull(fit) %>%
  .[[1]] %>%
  extract_fit_parsnip() %>%
  pluck("fit")
#
#
#
#| label: fig-default-residuos
#| fig-cap: ""
#| fig-width: 8
#| fig-height: 8
#| fig-align: center
#| out-width: "100%"
#| 
par(mfrow = c(2, 2), mar = c(4, 4, 3, 1))

#----------------------------------------------------
# 1. Resíduos de Deviance
#----------------------------------------------------
plot(
  fitted(fit_glm),
  residuals(fit_glm, type = "deviance"),
  pch = 19,
  col = "steelblue",
  xlab = "Probabilidade ajustada",
  ylab = "Resíduo de Deviance",
  main = "Resíduos de Deviance"
)
abline(h = 0, lty = 2, col = "red")

#----------------------------------------------------
# 2. Resíduos de Pearson
#----------------------------------------------------
plot(
  fitted(fit_glm),
  residuals(fit_glm, type = "pearson"),
  pch = 19,
  col = "steelblue",
  xlab = "Probabilidade ajustada",
  ylab = "Resíduo de Pearson",
  main = "Resíduos de Pearson"
)
abline(h = 0, lty = 2, col = "red")

#----------------------------------------------------
# 3. Distância de Cook
#----------------------------------------------------
plot(
  cooks.distance(fit_glm),
  type = "h",
  col = "steelblue",
  ylab = "Cook's Distance",
  xlab = "Observação",
  main = "Distância de Cook"
)

#----------------------------------------------------
# 4. Leverage
#----------------------------------------------------
plot(
  hatvalues(fit_glm),
  type = "h",
  col = "steelblue",
  ylab = "Leverage",
  xlab = "Observação",
  main = "Leverage"
)

par(mfrow = c(1,1))
#
#
#
#
#
#
#
#
#
#
#
#
#
#| label: tbl-comp-metrics-default
#| tbl-cap: ""
#| echo: false
#| warning: false 
diag_tbl <-
glance(fit_glm) %>%
    transmute(
        `Null Deviance` = null.deviance,
        `Residual Deviance` = deviance,
        AIC,
        BIC,
        `Graus de liberdade` = df.residual
    )

kable(diag_tbl)
#
#
#
#
#
#
#
#
#
#
#
#
#| label: tbl-drop1-pd30
#| tbl-cap: ""
#| echo: false
#| warning: false 

drop1_tbl <-
  drop1(fit_glm, test = "Chisq") |>
  as.data.frame() |>
  tibble::rownames_to_column("Variável") |>
  dplyr::filter(Variável != "<none>") |>
  dplyr::transmute(
    Variável,
    `Graus de liberdade` = Df,
    `Deviance` = LRT,
    `p-valor` = dplyr::case_when(
      is.na(`Pr(>Chi)`) ~ NA_character_,
      `Pr(>Chi)` < 0.001 ~ "< 0.001",
      TRUE ~ sprintf("%.4f", `Pr(>Chi)`)
    )
  )

knitr::kable(
  drop1_tbl,
  digits = 4,
  caption = "Teste da razão de verossimilhanças para a contribuição individual dos termos do modelo."
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
#| label: tbl-complete-model-default
#| tbl-cap: ""
#| echo: false
#| warning: false 

coef_all  %>%
  kable(digits = 4)
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
fit_or_load <- function(path, fit_expr) {

  if (file.exists(path)) {

    message("Modelo carregado de: ", path)
    return(readRDS(path))

  }

  message("Treinando modelo...")

  fit <- eval.parent(substitute(fit_expr))

  saveRDS(fit, path)

  message("Modelo salvo em: ", path)

  fit
}

rec_default <- recipe(PD30 ~ ., data = train_data_default) %>%
  step_dummy(all_nominal_predictors()) %>%
  step_normalize(all_numeric_predictors())
rec_prep <- prep(rec_default)

train_bayes <- bake(
  rec_prep,
  new_data = NULL
)

test_bayes <- bake(
  rec_prep,
  new_data = test_data_default
)

dir.create("models", showWarnings = FALSE)

#
#
#
fit_normal <- fit_or_load(
  "models/fit_prior_normal.rds",
  stan_glm(
  PD30 ~ .,
  data = train_bayes,
  family = binomial(link = "logit"),

  prior = normal(
    location = 0,
    scale = 1,
    autoscale = FALSE
  ),

  prior_intercept = normal(
    location = 0,
    scale = 5,
    autoscale = FALSE
  ),

  chains = 4,
  iter = 2000,
  warmup = 1000,
  seed = 123
)
)
#Todos os coeficientes recebem contração uniforme.
#
#
#
fit_laplace <- fit_or_load(
  "models/fit_prior_laplace.rds",
  rstanarm::stan_glm(
    PD30 ~ .,
    data = train_bayes,
    family = binomial(link = "logit"),

    prior = rstanarm::laplace(
      location = 0,
      scale = 1,
      autoscale = FALSE
    ),

    prior_intercept = rstanarm::normal(
      location = 0,
      scale = 5,
      autoscale = FALSE
    ),

    chains = 4,
    iter = 2000,
    warmup = 1000,
    seed = 123
  )
)
#Todos os coeficientes sofrem contração, porém alguns podem aproximar-se de zero com maior intensidade.
#
#
#
prob_normal <- posterior_epred(
  fit_normal,
  newdata = test_bayes
) %>%
  colMeans()

summary(prob_normal)

range(prob_normal)

mean(test_bayes$PD30 == "1")

eps <- 1e-15

prob_normal <- pmin(
  pmax(prob_normal, eps),
  1 - eps
)

logloss_manual <- -mean(
  ifelse(
    test_bayes$PD30 == "1",
    log(prob_normal),
    log(1 - prob_normal)
  )
)

logloss_manual
#
#
#
pred_lp <- posterior_epred(fit_laplace, newdata = test_bayes) %>% colMeans()
pred_norm <- posterior_epred(fit_normal, newdata = test_bayes) %>% colMeans()

calc_metrics_bayes <- function(fit, test_data, model_name) {
  
  # Probabilidade posterior média
  prob <- posterior_epred(
    fit,
    newdata = test_data
  ) %>%
    colMeans()
  
  # Classificação
  class <- ifelse(
    prob >= 0.5,
    "1",
    "0"
  ) %>%
    factor(
      levels = levels(test_data$PD30)
    )
  
  # Verdade
  truth <- test_data$PD30
  
  calc_metrics_bay(
    truth = truth,
    class = class,
    prob = prob
  ) %>%
    mutate(
      wflow_id = model_name
    )
}

metrics_p2 <- calc_metrics_bayes(
  fit = fit_laplace,
  test_data = test_bayes,
  model_name = "bayes_prior_l1"
)

metrics_p3 <- calc_metrics_bayes(
  fit = fit_normal,
  test_data = test_bayes,
  model_name = "bayes_prior_normal"
)
#
#
#
#
#
#
summary(fit_normal)
rstan::check_hmc_diagnostics(fit_normal$stanfit)
```
#
diag_normal <- posterior::summarise_draws(
  posterior::as_draws_array(fit_normal$stanfit),
  "rhat",
  "ess_bulk",
  "ess_tail",
  "mcse_mean"
)

diag_normal$variable
#
#
#
#| label: tbl-diagnostics-normal
#| tbl-cap: "Diagnósticos de convergência do modelo bayesiano com prior Normal."
#| echo: false
#| warning: false

diag_normal <- posterior::summarise_draws(
  posterior::as_draws_array(fit_normal$stanfit),
  "rhat", "ess_bulk", "ess_tail", "mcse_mean"
)

diag_normal %>%
  filter(variable != "mean_PPD") %>%
  summarise(
    Rhat_min = min(rhat, na.rm = TRUE),
    Rhat_max = max(rhat, na.rm = TRUE),
    n_eff_min = min(ess_bulk[variable != "log-posterior"], na.rm = TRUE),
    n_eff_max = max(ess_bulk[variable != "log-posterior"], na.rm = TRUE),
    n_eff_log_posterior = ess_bulk[variable == "log-posterior"],
    mcse_log_posterior = mcse_mean[variable == "log-posterior"]
  ) %>%
  tidyr::pivot_longer(
    cols = everything(),
    names_to = "Diagnóstico",
    values_to = "Resultado"
  ) %>%
  mutate(
    Diagnóstico = recode(
      Diagnóstico,
      "Rhat_min" = "Rhat mínimo",
      "Rhat_max" = "Rhat máximo",
      "n_eff_min" = "ESS mínimo",
      "n_eff_max" = "ESS máximo",
      "n_eff_log_posterior" = "ESS do log-posterior",
      "mcse_log_posterior" = "MCSE do log-posterior"
    ),
    Resultado = round(Resultado, 3)
  ) %>%
  knitr::kable(
    format = "latex",
    booktabs = TRUE,
    longtable = FALSE,
    col.names = c("Diagnóstico", "Resultado"),
    align = c("l", "r"),
    digits = 3
  ) %>%
  kableExtra::kable_styling(
    full_width = FALSE,
    position = "center",
    font_size = 9
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
#| label: fig-trace-normal
#| fig-cap: ""
#| fig-width: 8
#| fig-height: 8
#| fig-align: center
#| out-width: "100%"
#| 
bayesplot::mcmc_trace(
  as.array(fit_normal$stanfit),
  pars = c(
    "atraso_pagto_0",
    "limite_credito",
    "idade",
    "fatura_valor_1"
  ),
  facet_args = list(
    ncol = 2
  )
) +
  ggplot2::labs(
    title = "Trajetórias das cadeias do modelo bayesiano com prior Normal",
    x = "Iterações",
    y = "Valor do parâmetro"
  ) +
  ggplot2::theme_minimal(base_size = 12)
#
#
#
pars_normal <- dimnames(
  as.array(fit_normal$stanfit)
)[[3]]

pars_normal <- setdiff(
  pars_normal,
  c("mean_PPD", "log-posterior")
)

bayesplot::mcmc_trace(
  as.array(fit_normal$stanfit),
  pars = pars_normal,
  facet_args = list(ncol = 3)
) +
  ggplot2::labs(
    title = "Trajetórias das cadeias — modelo bayesiano com prior Normal",
    x = "Iterações",
    y = "Valor do parâmetro"
  ) +
  ggplot2::theme_minimal(base_size = 10)

# 1. Criação do gráfico com fonte ligeiramente menor
grafico_trajetorias <- bayesplot::mcmc_trace(
  as.array(fit_normal$stanfit),
  pars = pars_normal,
  facet_args = list(ncol = 3)
) +
  labs(
    title = "Trajetórias das cadeias — modelo bayesiano com prior Normal",
    x = "Iterações",
    y = "Valor do parâmetro"
  ) +
  theme_minimal(base_size = 9) # Reduzido de 10 para 9 para poupar espaço

# 2. Salvando com cairo_pdf para evitar problemas de sobreposição
ggsave(
  filename = "diagnostico/trajetorias_cadeias_normal.pdf", 
  plot = grafico_trajetorias, 
  width = 10,       
  height = 14,      
  device = cairo_pdf # Motor gráfico mais fiável para PDFs complexos no R
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
#| label: fig-pp-check-normal
#| fig-cap: ""
#| fig-width: 8
#| fig-height: 8
#| fig-align: center
#| out-width: "100%"
#| 
bayesplot::pp_check(
  fit_normal,
  type = "bars",
  nreps = 100
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
summary(fit_laplace)
rstan::check_hmc_diagnostics(fit_laplace$stanfit)
```
#
diag_laplace <- posterior::summarise_draws(
  posterior::as_draws_array(fit_laplace$stanfit),
  "rhat",
  "ess_bulk",
  "ess_tail",
  "mcse_mean"
)

diag_laplace$variable
#
#
#
#| label: tbl-diagnostics-laplace
#| tbl-cap: "Diagnósticos de convergência do modelo bayesiano com prior Laplace."
#| echo: false
#| warning: false

diag_laplace <- posterior::summarise_draws(
  posterior::as_draws_array(fit_laplace$stanfit),
  "rhat", "ess_bulk", "ess_tail", "mcse_mean"
)

diag_laplace %>%
  filter(variable != "mean_PPD") %>%
  summarise(
    Rhat_min = min(rhat, na.rm = TRUE),
    Rhat_max = max(rhat, na.rm = TRUE),
    n_eff_min = min(ess_bulk[variable != "log-posterior"], na.rm = TRUE),
    n_eff_max = max(ess_bulk[variable != "log-posterior"], na.rm = TRUE),
    n_eff_log_posterior = ess_bulk[variable == "log-posterior"],
    mcse_log_posterior = mcse_mean[variable == "log-posterior"]
  ) %>%
  tidyr::pivot_longer(
    cols = everything(),
    names_to = "Diagnóstico",
    values_to = "Resultado"
  ) %>%
  mutate(
    Diagnóstico = recode(
      Diagnóstico,
      "Rhat_min" = "Rhat mínimo",
      "Rhat_max" = "Rhat máximo",
      "n_eff_min" = "ESS mínimo",
      "n_eff_max" = "ESS máximo",
      "n_eff_log_posterior" = "ESS do log-posterior",
      "mcse_log_posterior" = "MCSE do log-posterior"
    ),
    Resultado = round(Resultado, 3)
  ) %>%
  knitr::kable(
    format = "latex",
    booktabs = TRUE,
    longtable = FALSE,
    col.names = c("Diagnóstico", "Resultado"),
    align = c("l", "r"),
    digits = 3
  ) %>%
  kableExtra::kable_styling(
    full_width = FALSE,
    position = "center",
    font_size = 9
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
#| label: fig-trace-laplace
#| fig-cap: ""
#| fig-width: 8
#| fig-height: 8
#| fig-align: center
#| out-width: "100%"
#| 
bayesplot::mcmc_trace(
  as.array(fit_laplace$stanfit),
  pars = c(
    "atraso_pagto_0",
    "limite_credito",
    "idade",
    "fatura_valor_1"
  ),
  facet_args = list(
    ncol = 2
  )
) +
  ggplot2::labs(
    title = "Trajetórias das cadeias do modelo bayesiano com prior Laplace",
    x = "Iterações",
    y = "Valor do parâmetro"
  ) +
  ggplot2::theme_minimal(base_size = 12)
#
#
#
pars_laplace <- dimnames(
  as.array(fit_laplace$stanfit)
)[[3]]

pars_laplace <- setdiff(
  pars_laplace,
  c("mean_PPD", "log-posterior")
)

bayesplot::mcmc_trace(
  as.array(fit_laplace$stanfit),
  pars = pars_laplace,
  facet_args = list(ncol = 3)
) +
  ggplot2::labs(
    title = "Trajetórias das cadeias — modelo bayesiano com prior Laplace",
    x = "Iterações",
    y = "Valor do parâmetro"
  ) +
  ggplot2::theme_minimal(base_size = 10)

# 1. Criação do gráfico com fonte ligeiramente menor
grafico_trajetorias <- bayesplot::mcmc_trace(
  as.array(fit_laplace$stanfit),
  pars = pars_laplace,
  facet_args = list(ncol = 3)
) +
  labs(
    title = "Trajetórias das cadeias — modelo bayesiano com prior Laplace",
    x = "Iterações",
    y = "Valor do parâmetro"
  ) +
  theme_minimal(base_size = 9) # Reduzido de 10 para 9 para poupar espaço

# 2. Salvando com cairo_pdf para evitar problemas de sobreposição
ggsave(
  filename = "diagnostico/trajetorias_cadeias_laplace.pdf", 
  plot = grafico_trajetorias, 
  width = 10,       
  height = 14,      
  device = cairo_pdf # Motor gráfico mais fiável para PDFs complexos no R
)
#
#
#
#| label: fig-pp-check-laplace
#| fig-cap: ""
#| fig-width: 8
#| fig-height: 8
#| fig-align: center
#| out-width: "100%"
#| 
bayesplot::pp_check(
  fit_laplace,
  type = "bars",
  nreps = 100
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
#| label: tbl-results-all
#| tbl-cap: "Métricas de desempenho dos modelos de regressão logística bayesiano e frequentista para previsão de PD30 no conjunto de teste."
#| echo: false
#| warning: false

metrics_table_all <- bind_rows(
  metrics_table_final,
  metrics_p2,
  metrics_p3
)

metrics_table_all %>%
  mutate(
    Modelo = recode(
      wflow_id,
      "all_features_logit" = "Completo",
      "no_corr_logit" = "Sem multicol.",
      "l1_logit" = "L1 (LASSO)",
      "l2_logit" = "L2 (Ridge)",
      "bayes_prior_l1" = "Bayesiano (Laplace)",
      "bayes_prior_normal" = "Bayesiano (Normal)"
    )
  ) %>%
  select(
    Modelo,
    accuracy,
    bal_accuracy,
    roc_auc,
    log_loss,
    sens,
    spec,
    precision,
    f1,
    mcc
  ) %>%
  tibble::column_to_rownames("Modelo") %>%
  t() %>%
  as.data.frame() %>%
  tibble::rownames_to_column("Métrica") %>%
  mutate(
    Métrica = recode(
      Métrica,
      "accuracy" = "Acurácia",
      "bal_accuracy" = "Acurácia balanceada",
      "roc_auc" = "ROC AUC",
      "log_loss" = "Log-loss",
      "sens" = "Sensibilidade",
      "spec" = "Especificidade",
      "precision" = "Precisão",
      "f1" = "F1-score",
      "mcc" = "MCC"
    )
  ) %>%
  knitr::kable(
    format = "latex",
    booktabs = TRUE,
    longtable = FALSE,
    digits = 4,
    align = c("l", rep("r", 6))
  ) %>%
  kableExtra::kable_styling(
    full_width = FALSE,
    position = "center",
    font_size = 8
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
#| label: fig-confusion-matrices
#| fig-cap: "Matrizes de confusão dos modelos frequentista e bayesiano no conjunto de teste."
#| fig-width: 10
#| fig-height: 5
#| echo: false
#| warning: false

# --------------------------------------------------
# 1. Extrair o modelo frequentista sem multicolinearidade
# --------------------------------------------------

fit_no_corr <- fits %>%
  filter(wflow_id == "no_corr_logit") %>%
  pull(fit) %>%
  .[[1]]

# --------------------------------------------------
# 2. Predições frequentistas
# --------------------------------------------------

pred_freq <- predict(
  fit_no_corr,
  new_data = test_data_default,
  type = "class"
)$.pred_class

truth_freq <- factor(
  test_data_default$PD30,
  levels = c("0", "1")
)

# --------------------------------------------------
# 3. Predições bayesianas (prior Normal)
# --------------------------------------------------

prob_bayes <- posterior_epred(
  fit_normal,
  newdata = test_bayes
) %>%
  colMeans()

pred_bayes <- factor(
  ifelse(prob_bayes >= 0.5, "1", "0"),
  levels = c("0", "1")
)

truth_bayes <- factor(
  test_bayes$PD30,
  levels = c("0", "1")
)

# --------------------------------------------------
# 4. Verificar consistência dos conjuntos de teste
# --------------------------------------------------

stopifnot(
  length(truth_freq) == length(truth_bayes),
  identical(
    as.character(truth_freq),
    as.character(truth_bayes)
  )
)

# --------------------------------------------------
# 5. Construir as matrizes de confusão
# --------------------------------------------------

conf_freq <- as.data.frame(
  table(
    Real = truth_freq,
    Predito = pred_freq
  )
) %>%
  mutate(
    Modelo = "Frequentista (sem multicol.)"
  )

conf_bayes <- as.data.frame(
  table(
    Real = truth_bayes,
    Predito = pred_bayes
  )
) %>%
  mutate(
    Modelo = "Bayesiano (Normal)"
  )

conf_all <- bind_rows(
  conf_freq,
  conf_bayes
) %>%
  mutate(
    Real = factor(Real, levels = c("0", "1")),
    Predito = factor(Predito, levels = c("0", "1")),
    Modelo = factor(
      Modelo,
      levels = c(
        "Frequentista (sem multicol.)",
        "Bayesiano (Normal)"
      )
    )
  )

# --------------------------------------------------
# 6. Gráfico comparativo
# --------------------------------------------------

ggplot(
  conf_all,
  aes(x = Predito, y = Real, fill = Freq)
) +
  geom_tile(
    color = "white",
    linewidth = 1.2
  ) +
  geom_text(
    aes(label = Freq),
    size = 5,
    fontface = "bold"
  ) +
  facet_wrap(~Modelo) +
  scale_fill_gradient(
    low = "#EAF0F6",
    high = "#245A81",
    name = "Observações"
  ) +
  scale_x_discrete(
    labels = c(
      "0" = "Não inadimplente",
      "1" = "Inadimplente"
    )
  ) +
  scale_y_discrete(
    labels = c(
      "0" = "Não inadimplente",
      "1" = "Inadimplente"
    )
  ) +
  labs(
    title = "Matrizes de confusão dos modelos",
    subtitle = "Avaliação no conjunto de teste",
    x = "Classe predita",
    y = "Classe real"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(
      face = "bold",
      size = 14
    ),
    strip.text = element_text(
      face = "bold",
      size = 11
    ),
    panel.grid = element_blank(),
    legend.position = "right"
  )

#
#
#
# --------------------------------------------------
# 1. Extrair modelo frequentista
# --------------------------------------------------

fit_no_corr <- fits %>%
  filter(wflow_id == "no_corr_logit") %>%
  pull(fit) %>%
  .[[1]]

# --------------------------------------------------
# 2. Obter probabilidades no conjunto de teste
# --------------------------------------------------

prob_freq <- predict(
  fit_no_corr,
  new_data = test_data_default,
  type = "prob"
)$.pred_1

prob_bayes <- posterior_epred(
  fit_normal,
  newdata = test_bayes
) %>%
  colMeans()

# --------------------------------------------------
# 3. Definir classes reais
# --------------------------------------------------

truth_freq <- factor(
  test_data_default$PD30,
  levels = c("0", "1")
)

truth_bayes <- factor(
  test_bayes$PD30,
  levels = c("0", "1")
)

# --------------------------------------------------
# 4. Calcular curvas ROC
# --------------------------------------------------

# Frequentista
dados_roc_freq <- tibble(
  truth = truth_freq,
  prob = prob_freq
)

roc_freq <- yardstick::roc_curve(
  data = dados_roc_freq,
  truth = truth,
  prob,
  event_level = "second"
) %>%
  mutate(
    Modelo = "Frequentista (Sem Multicol.)"
  )

# Bayesiano
dados_roc_bayes <- tibble(
  truth = truth_bayes,
  prob = prob_bayes
)

roc_bayes <- yardstick::roc_curve(
  data = dados_roc_bayes,
  truth = truth,
  prob,
  event_level = "second"
) %>%
  mutate(
    Modelo = "Bayesiano (Normal)"
  )

# Combinar curvas
roc_comparacao <- bind_rows(
  roc_freq,
  roc_bayes
) %>%
  mutate(
    youden = sensitivity + specificity - 1
  )

```
#
#| label: tbl-youden
#| tbl-cap: ""
#| echo: false
#| warning: false 
melhores_limiares <- roc_comparacao %>%
  group_by(Modelo) %>%
  slice_max(
    order_by = youden,
    n = 1,
    with_ties = FALSE
  ) %>%
  ungroup() %>%
  select(
    Modelo,
    .threshold,
    sensitivity,
    specificity,
    youden
  )

kable(melhores_limiares)
#
#
#
#| label: roc-comparacao
#| fig-cap: ""
#| fig-width: 8
#| fig-height: 8
#| fig-align: center
#| out-width: "100%"

ggplot(
  roc_comparacao,
  aes(
    x = 1 - specificity,
    y = sensitivity,
    color = Modelo
  )
) +
  geom_path(linewidth = 1.1) +
  geom_abline(
    intercept = 0,
    slope = 1,
    linetype = "dashed",
    color = "grey60"
  ) +
  coord_equal() +
  labs(
    title = "Curvas ROC dos modelos",
    subtitle = "Avaliação no conjunto de teste",
    x = "Taxa de falsos positivos (1 - especificidade)",
    y = "Sensibilidade",
    color = "Modelo"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "bottom"
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
