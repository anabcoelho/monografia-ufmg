# Revisão da Monografia — Observações e Sugestões

> Arquivo gerado em 2026-11-07 para apoiar a finalização do texto.  
> Foco: revisão geral do texto existente + proposta de Discussão e Conclusão.

---

## 1. Observações Gerais sobre o Texto Existente

### Resumo e Abstract - Depois
- **Resumo:** O próprio documento marca `\red{reavaliar os resumos}`. O resumo atual está coerente com o trabalho, mas **precisa ser atualizado** após a conclusão do TBD para refletir os resultados reais (p. ex., que as abordagens produziram desempenho preditivo semelhante, que a priori influenciou marginalmente as estimativas, etc.).
- **Abstract:** Incompleto — a frase "applied to the prediction of" está cortada. Precisa ser finalizado com resultados e conclusões concretos.

### Introdução - Revisado
- Texto coeso e bem estruturado. Nenhuma alteração substancial necessária.
- Sugestão: na subseção "Delimitação e Objetivos", verificar se todos os objetivos específicos listados foram de fato contemplados nos resultados (em particular "Avaliar os efeitos da multicolinearidade sobre a estimação dos parâmetros" — foi coberto na seção de resultados).

### Referencial Teórico -> ok
- Bem escrito e detalhado. A seção de MCMC e NUTS é extensa e técnica — considerar se o nível de detalhe é adequado para o escopo do trabalho (especialização vs. doutorado).
- A subseção "Sensibilidade às distribuições a priori" (p. 29 aprox.) enuncia o problema mas não tem exemplo concreto — poderia remeter diretamente para a análise posterior dos dados.

### Metodologia -> ok
- Bem estruturada. Separação treino/teste e justificativas claras.
- **Ponto de atenção:** A seção "Análise de categorias L1 vs Remoção de Variáveis Correlacionadas" não tem label de seção visível no documento final. Verificar se aparece corretamente numerada.

### Resultados -> ok
- O texto dos resultados está muito bem elaborado, especialmente a análise de coeficientes frequentista e a discussão sobre multicolinearidade.
- **Tabela `tbl-waic-comparacao`:** o texto menciona "diferença de −0,74, com erro-padrão de 0,32" — essa diferença menor que 2 erros-padrão reforça a equivalência dos modelos e deve ser mencionada explicitamente na discussão. -> ajustado
- **`tbl-resumo-posterior`:** referenciada no texto mas está marcada como `include = FALSE`. Verificar se está aparecendo no PDF gerado. Se não, tornar `echo=FALSE` em vez de `include=FALSE`.
- **`roc-comparacao`:** a figura não tem `fig-cap` preenchida e o label da tabela `tbl-youden` também está vazio. Preencher antes da versão final. - OK
- A análise dos erros de classificação (falsos negativos vs. falsos positivos) é muito bem conduzida — boa ancoragem para a Discussão.

---

## 2. Proposta de Seção: Discussão

> Localização: logo após os Resultados, antes da Conclusão.  
> Extensão estimada: 2–3 páginas.  
> O esqueleto já existe no documento (`# [TBD] Discussão`). O texto abaixo é uma proposta de conteúdo com sugestões de onde inserir cada parágrafo.

---

### 2.1 Equivalência preditiva entre as abordagens

**Onde inserir:** No início da Discussão, como parágrafo de abertura. Retoma a questão de pesquisa central.

**Sugestão de texto:**

> Os resultados obtidos neste estudo evidenciam que, para o conjunto de dados e as especificações avaliadas, as abordagens frequentista e bayesiana produziram desempenho preditivo essencialmente equivalente. Os valores de ROC AUC variaram entre 0,714 e 0,717, enquanto o log-loss permaneceu próximo de 0,470 em todos os modelos. Essa proximidade sugere que, em cenários com amostras de grande dimensão e alta informatividade dos dados, a escolha entre as duas abordagens é pouco determinante para o desempenho classificatório — resultado consistente com a teoria assintótica, que prevê convergência das inferências bayesianas e frequentistas à medida que o tamanho da amostra cresce.

**Observação:** Mencionar que essa equivalência era esperada dado n=30.000 — com amostras pequenas, as prioris têm papel maior.

---

### 2.2 Sensibilidade às distribuições a priori e o papel do tamanho amostral

**Onde inserir:** Segundo bloco da Discussão. Responde diretamente ao objetivo específico "Comparar as distribuições a priori Normal e Laplace quanto à influência sobre as estimativas e a incerteza posterior".

**Sugestão de texto:**

> A comparação entre as especificações bayesianas com prioris Normal e Laplace revelou estimativas posteriores bastante próximas. As medianas dos coeficientes apresentaram diferenças em geral inferiores a 0,02 unidades, e a comparação pelo WAIC produziu uma diferença de ELPD de −0,52 com erro-padrão de 0,28 — magnitude inferior a dois erros-padrão, indicando que as especificações são essencialmente equivalentes do ponto de vista da capacidade preditiva.
>
> Embora as duas distribuições apresentem formatos distintos — a Normal com caudas mais leves e a Laplace com maior concentração de probabilidade em torno de zero e caudas mais pesadas —, a equivalência das variâncias adotadas (σ² = 1 em ambas) restringiu o contraste à forma das distribuições, sem explorar diferenças no grau de informativeness. Mais do que isso, o volume de dados disponível exerceu papel determinante sobre esse resultado. Com n = 30.000 observações, a função de verossimilhança apresenta dispersão extremamente restrita, e sua concentração em torno dos valores estimados pelo modelo supera em magnitude a influência de ambas as prioris. A figura de atualização bayesiana do coeficiente `atraso_pagto_0` documenta esse comportamento diretamente: independentemente da especificação adotada, a posteriori foi praticamente moldada pela verossimilhança, convergindo para valores semelhantes em ambos os modelos. Esse fenômeno é fundamentado pelo teorema de Bernstein-von Mises, segundo o qual, à medida que n → ∞, a distribuição posterior converge para uma distribuição Normal centrada no estimador de máxima verossimilhança, tornando-a assintoticamente independente da priori.
>
> Essa equivalência observada entre as prioris, e entre as abordagens frequentista e bayesiana em geral, não implica que a escolha da distribuição a priori seja irrelevante em outros contextos. Em situações com amostras de menor dimensão — comuns no início da operação de um produto de crédito, em segmentos de nicho ou em carteiras com baixíssima taxa de inadimplência —, a verossimilhança fornece menos informação, e a distribuição a priori passa a exercer influência mais pronunciada sobre as estimativas. Nesse cenário, as diferenças de forma entre as prioris Normal e Laplace poderiam se manifestar de forma mais expressiva: a priori Laplace, por seu comportamento análogo à penalização L1, tenderia a promover contração mais intensa sobre coeficientes de menor magnitude, podendo conduzir estimativas próximas de zero — efeito que a priori Normal, mais suave em sua regularização, não produz com a mesma intensidade. Assim, a abordagem bayesiana com prioris bem especificadas pode oferecer vantagens práticas precisamente nos cenários em que os dados são escassos, seja por incorporar conhecimento prévio sobre os parâmetros de forma explícita e controlada, seja por produzir estimativas mais estáveis do que as obtidas por máxima verossimilhança em amostras pequenas. Essa é, portanto, uma limitação importante deste estudo: os resultados obtidos são específicos ao contexto de grande dimensão amostral e não devem ser generalizados como evidência de equivalência universal entre as abordagens.

**Observação de redação:** a menção ao teorema de Bernstein-von Mises pode ser mantida ou simplificada dependendo do perfil do leitor esperado. Se o orientador preferir uma linguagem mais aplicada, substituir por: *"resultado consistente com a teoria assintótica, que prevê convergência das inferências bayesiana e frequentista quando o tamanho da amostra cresce"*.

---

### 2.3 Multicolinearidade: tratamentos e implicações

**Onde inserir:** Terceiro bloco. O texto já existe parcialmente na seção "Multicolinearidade e regularização bayesiana" dos Resultados — pode ser movido/expandido aqui.

**Sugestão de texto:**

> A multicolinearidade constituiu um aspecto central neste estudo, dada a forte correlação temporal entre as variáveis de fatura (`fatura_valor_1` a `fatura_valor_6`) e, em menor grau, entre as variáveis de atraso de pagamento. No modelo frequentista completo, o VIF máximo de 41,12 indica que determinados coeficientes apresentam variâncias substancialmente infladas, dificultando a interpretação individual de seus efeitos. Contudo, a remoção de variáveis altamente correlacionadas reduziu o VIF máximo para 1,84 sem comprometer o desempenho preditivo — resultado que reforça a recomendação de diagnóstico e tratamento da multicolinearidade em modelos destinados à interpretação inferencial.
>
> Na abordagem bayesiana, as distribuições a priori centradas em zero introduzem uma forma de regularização que pode conferir maior estabilidade às estimativas em presença de multicolinearidade. Entretanto, como os VIFs calculados para os modelos bayesianos — que compartilham a mesma matriz de preditores — também revelaram valores elevados para as variáveis de fatura, fica evidente que a regularização bayesiana não elimina a estrutura de dependência entre os preditores. O que a priori faz é restringir a magnitude dos coeficientes, reduzindo potencialmente a variância das estimativas individuais, mas não resolvendo o problema de identificação subjacente à multicolinearidade.

---

### 2.4 Diferenças inferenciais entre as abordagens

**Onde inserir:** Quarto bloco. Este é o ponto mais relevante do ponto de vista metodológico.

**Sugestão de texto:**

> Além do desempenho preditivo, as abordagens frequentista e bayesiana diferem de forma substantiva no que se refere à interpretação e à quantificação da incerteza dos parâmetros. Na abordagem frequentista, a incerteza é expressa por intervalos de confiança, cuja interpretação está ligada ao procedimento de amostragem repetida: um intervalo de 95% indica que, em 95% das amostras hipotéticas extraídas do mesmo processo gerador, o intervalo construído dessa maneira conteria o verdadeiro valor do parâmetro. Na abordagem bayesiana, os intervalos de credibilidade de 95% permitem uma interpretação diretamente probabilística: dado o modelo e os dados observados, existe uma probabilidade de 95% de que o parâmetro pertença ao intervalo obtido. Essa distinção é relevante em aplicações práticas, nas quais afirmações probabilísticas sobre os parâmetros são frequentemente mais naturais e informativas do que declarações sobre procedimentos amostrais.
>
> Do ponto de vista da interpretação dos coeficientes, ambas as abordagens produziram conclusões consistentes sobre a direção e a relevância dos preditores. A variável `atraso_pagto_0` destacou-se em todos os modelos como o preditor de maior impacto sobre o risco de inadimplência, corroborando estudos prévios na área de credit scoring. As variáveis `fatura_valor_1`, `pagto_valor_1`, `pagto_valor_2` e `limite_credito` apresentaram associações negativas com a inadimplência, enquanto variáveis demográficas como `sexo_Feminino` e `estado_civil_Solteiro` também mostraram associações estatisticamente significativas em ambas as abordagens.

---

### 2.5 Desbalanceamento de classes e limitações práticas

**Onde inserir:** Quinto bloco.

**Sugestão de texto:**

> Um aspecto comum às duas abordagens foi a baixa sensibilidade na identificação de clientes inadimplentes com o ponto de corte convencional de 0,5. A especificidade elevada (~97%) aliada à sensibilidade reduzida (~25%) indica que os modelos tendem a classificar a maioria dos inadimplentes como não inadimplentes, o que pode comprometer sua utilidade prática para fins de prevenção de perdas. Esse comportamento é consequência direta do desbalanceamento da variável resposta — aproximadamente 22% de inadimplentes — e não de uma deficiência específica de qualquer das abordagens avaliadas.
>
> A análise do índice de Youden demonstrou que a adoção de pontos de corte mais baixos (~0,26) poderia elevar a sensibilidade para cerca de 55%, às custas de uma redução na especificidade para ~82-84%. A proximidade dos resultados entre os modelos frequentista e bayesiano sob essa análise reforça a semelhança comportamental das duas abordagens diante de ajustes no critério de classificação.

---

## 3. Proposta de Seção: Conclusão

> Localização: após a Discussão.  
> Extensão estimada: 1–1,5 página.

---

**Sugestão de texto:**

> Este trabalho investigou a aplicação de modelos de regressão logística à previsão de inadimplência em cartões de crédito, comparando as abordagens de inferência frequentista e bayesiana em termos de desempenho preditivo, interpretação dos parâmetros e quantificação da incerteza.
>
> Do ponto de vista preditivo, as seis especificações avaliadas — quatro frequentistas e duas bayesianas — produziram resultados notavelmente próximos. Os valores de ROC AUC variaram entre 0,714 e 0,717, e o log-loss entre 0,469 e 0,470, indicando que a incorporação de distribuições a priori, nas configurações avaliadas, não proporcionou ganhos expressivos em relação às alternativas frequentistas. Esse resultado é coerente com a teoria, que prevê convergência das inferências à medida que o tamanho da amostra cresce — condição atendida neste estudo com 30.000 observações.
>
> No que tange à inferência, as abordagens se distinguem de forma relevante. A abordagem bayesiana permitiu quantificar a incerteza dos parâmetros por meio de distribuições a posteriori e intervalos de credibilidade, cuja interpretação é mais intuitiva em contextos aplicados do que a dos intervalos de confiança frequentistas. A comparação entre as prioris Normal e Laplace revelou estimativas posteriores praticamente equivalentes, sugerindo que, diante de uma amostra altamente informativa, a forma da distribuição a priori tem impacto limitado sobre as conclusões — embora suas diferenças possam ser mais pronunciadas em contextos com amostras menores ou maior incerteza paramétrica.
>
> Quanto ao tratamento da multicolinearidade, a remoção de preditores altamente correlacionados se mostrou uma estratégia eficaz para reduzir a instabilidade das estimativas sem comprometer o desempenho preditivo. As abordagens de regularização — Lasso (L1), Ridge (L2) e as prioris bayesianas — apresentaram comportamentos análogos do ponto de vista da penalização, sem diferenças expressivas nos resultados obtidos.
>
> **Limitações e trabalhos futuros.** Este estudo apresenta algumas limitações que devem ser consideradas na interpretação dos resultados. A análise foi restrita a um único conjunto de dados, referente a clientes de cartão de crédito em Taiwan no período de 2005, o que limita a generalização das conclusões para outros mercados, populações e períodos. Além disso, as prioris bayesianas foram especificadas de forma relativamente não informativa, sem incorporação de conhecimento especializado do domínio de crédito. A utilização de prioris informativas baseadas em estudos anteriores ou em julgamentos de especialistas poderia modificar as estimativas e revelar diferenças mais expressivas entre as abordagens. Por fim, estratégias para lidar com o desbalanceamento de classes — como reamostragem, ajuste de pesos ou limiares de classificação baseados em custos assimétricos — poderiam ser exploradas em trabalhos futuros para melhorar a identificação da classe de inadimplentes.
>
> Em síntese, os resultados obtidos sugerem que, para problemas de classificação binária com amostras de grande dimensão, as abordagens frequentista e bayesiana tendem a produzir resultados preditivos equivalentes. A principal vantagem da abordagem bayesiana reside na riqueza inferencial: a capacidade de expressar diretamente a incerteza sobre os parâmetros e de incorporar informação prévia de forma explícita e controlada. Assim, a escolha entre as duas abordagens deve considerar não apenas o desempenho preditivo esperado, mas também os objetivos da análise — se voltados primariamente à discriminação e previsão, ou à compreensão dos mecanismos associados ao risco de inadimplência e à quantificação da incerteza das estimativas.

---

## 4. Outros Ajustes Pendentes

| Item | Status | Ação necessária |
|------|--------|-----------------|
| Resumo e Abstract | Incompletos | Reescrever após finalização do texto |
| `tbl-resumo-posterior` | `include=FALSE` | Mudar para `echo=FALSE` se deve aparecer no PDF |
| `fig-cap` de `roc-comparacao` | Vazia | Adicionar legenda |
| `tbl-cap` de `tbl-youden` | Vazia | Adicionar legenda |
| Seção `[TO DO]` Referencial Teórico | Marcada como TO DO | Verificar se está completa para submissão |
| Legenda de status do documento | Visível no PDF | Remover antes da versão final (bloco de controle interno) |
| Comandos `\red{}` e `\blue{}` | Visíveis no PDF | Mudar para `\newcommand{\red}[1]{}` na versão final |
| Análise de sensibilidade à multicolinearidade | `\red{Encaixar aqui...}` | Completar ou remover marcação |

---

## 6. Redundâncias de Texto e Fórmulas Duplicadas

> **Como usar:** para cada item, o trecho marcado como **manter** é o mais completo ou no lugar certo; o trecho marcado como **encurtar/remover** pode ser reduzido a uma frase de remissão ("como discutido em X" / "conforme a @eq-Y").

---

### 6.1 Redundâncias de Texto

#### R1 — Explicação de FP/FN repetida (Resultados → Discussão)

| Onde | O que diz |
|------|-----------|
| **Resultados §4.3.3.6** "Análise dos erros de classificação" (p. 100–102) | Define FP e FN, apresenta os números (1.231 FN, 181 FP) e discute implicações práticas para concessão de crédito |
| **Discussão §5.4** "Desbalanceamento de classes" (p. 108–109) | Redefine FP e FN com texto quase idêntico, cita novamente "97% de especificidade" e "22,6% a 25,9%", repete a lógica de custos assimétricos |

**Ação:** Encurtar o início de §5.4. Os dois primeiros parágrafos que definem FP/FN e citam os números podem ser comprimidos em uma frase de remissão. Manter apenas o argumento sobre **custos assimétricos e ponto de corte** — esse é o ganho da Discussão em relação aos Resultados.

> **Sugestão de abertura para §5.4:**
> "Conforme evidenciado na análise dos erros de classificação (§4.3.3.6), ambos os modelos produziram predominantemente falsos negativos — cerca de 74% dos inadimplentes não foram identificados com o limiar de 0,5. No contexto da concessão de crédito, esse padrão de erros tem implicações econômicas assimétricas: [seguir com custos de FP vs. FN e discussão do ponto de corte]."

---

#### R2 — Diagnósticos MCMC: Normal e Laplace com texto quase idêntico

| Onde | O que diz |
|------|-----------|
| **§4.3.3.4 Priori Normal** (pp. ~85-88) | Descreve Rhat ≈ 1,000, ESS 2.448–6.648, ausência de divergências, E-BFMI ok, traceplots com boa mistura, PPC satisfatório |
| **§4.3.3.5 Priori Laplace** (pp. ~89-92) | Descreve Rhat 1,000–1,003, ESS 2.363–5.056, ausência de divergências, E-BFMI ok, traceplots com boa mistura, PPC satisfatório |

A estrutura é idêntica — mesmas métricas, mesmas conclusões, apenas com números ligeiramente diferentes.

**Ação:** Manter os diagnósticos completos da Normal (é o modelo de referência da comparação). Para a Laplace, condensar em um parágrafo único que destaca **apenas as diferenças** (ESS levemente menor, Rhat máximo de 1,003 vs 1,000, ESS do log-posterior menor: 855 vs 1.599) e encerra com "os demais diagnósticos foram semelhantes ao modelo com priori Normal".

---

#### R3 — Descrição do PPC repetida nos dois modelos

| Onde | O que diz |
|------|-----------|
| **§4.3.3.4** (fig-pp-check-normal) | Explica o que é PPC, o que a sobreposição indica, cita o desbalanceamento ~78%/22% |
| **§4.3.3.5** (fig-pp-check-laplace) | Repete a explicação de PPC e o desbalanceamento |

**Ação:** Na seção do modelo Laplace, remover a re-explicação do que é PPC. Substituir por: "A verificação preditiva posterior do modelo Laplace (Figura X) apresentou resultados análogos aos observados para o modelo Normal, com boa sobreposição entre as distribuições observada e replicada."

---

#### R4 — VIF 41,12 → 1,84 citado três vezes

Aparece em:
1. **§4.3.1** na tabela `tbl-vif-modelos-pd30` e texto que a acompanha *(localização correta: Resultados/Frequentista)*
2. **§4.3.3** "Diagnóstico de multicolinearidade" do modelo Bayesiano, que cita os VIFs da Normal/Laplace
3. **§5.2** "Multicolinearidade" na Discussão

**Ação:** Manter o detalhamento em §4.3.1 e §4.3.3 (são os resultados). Em §5.2, apenas referenciar: "conforme apresentado na Tabela X (§4.3.1)" — não repetir os valores literalmente. Um parágrafo de interpretação/implicação é o que a Discussão agrega de novo.

---

#### R5 — Analogia priori Normal ↔ Ridge e Laplace ↔ Lasso apresentada duas vezes

| Onde |
|------|
| **§2.3.2** Referencial Teórico — "Priori Normal" e "Priori Laplace" (pp. 29–31): a equivalência é demonstrada matematicamente com as equações de log-prior |
| **§5.1** Discussão — a analogia é re-mencionada para justificar o comportamento observado |

**Ação:** Em §5.1 é correto mencionar a analogia — é o momento de conectar teoria e resultado. Mas basta uma frase, não re-derivar. Sugestão: *"A priori Laplace, cuja relação com a penalização L1 foi apresentada no §2.3.2, tenderia a..."*

---

### 6.2 Fórmulas Duplicadas

#### F1 — `{#eq-ridge}` com label duplicado ⚠️ OK

O label `#eq-ridge` aparece **duas vezes** no `.qmd`, o que pode causar referências quebradas ou avisos na compilação:

| Localização no .qmd | Fórmula |
|---------------------|---------|
| §2.2.6 "Regularização" (~linha 884) | Estimador Ridge: `\hat{\beta}_{ridge} = \arg\max [\ell(\beta) - \lambda \sum \beta_j^2]` |
| §2.3.2 "Priori Normal" (~linha 1051) | A mesma fórmula Ridge, re-apresentada para mostrar a analogia com a priori Normal |

**Ação:** Renomear o segundo como `{#eq-ridge-bayes}` ou simplesmente remover a equação duplicada e substituir por uma remissão: *"...penalização apresentada na @eq-ridge"*.

---

#### F2 — Penalização Lasso apresentada como equação separada em dois contextos OK

| Localização no .qmd | Conteúdo |
|---------------------|---------|
| §2.2.6 (~linha 909) | `\hat{\beta}_{lasso} = \arg\max [\ell(\beta) - \lambda \sum |\beta_j|]` com label `{#eq-lasso}` |
| §2.3.2 (~linha 1148) | Apenas o termo de penalização `\lambda \sum |\beta_j|` como display math inline, sem label |

Não é um label duplicado, mas é repetitivo. **Ação:** Substituir o bloco em §2.3.2 por: *"...um termo proporcional a $\sum_{j}|\beta_j|$ — a penalização L1 da @eq-lasso..."*

---

#### F3 — Distribuição Bernoulli definida em dois lugares

| Localização | Equação |
|------------|---------|
| §2.1.1 "Variáveis Aleatórias" | `Y \sim \mathrm{Bernoulli}(\theta)`, com `P(Y=1)=\theta` |
| §2.2.1 "Formulação do modelo" | `Y_i \mid \theta_i \sim \mathrm{Bernoulli}(\theta_i)` com as probabilidades condicionais |

A segunda instância expande a primeira para o contexto de regressão — é uma **expansão legítima**, não uma duplicata problemática. Manter como está; nenhuma ação necessária.

---

#### F4 — Log-verossimilhança expandida (`{#eq-log-verossimilhaneca-expandida}`) OK

Definida em §2.2.3 e usada como base para o AIC/BIC em §2.2.4. Nenhum label duplicado, mas o nome da equação tem um typo: `verossimilhaneca` → `verossimilhanca`. Baixo impacto (só afeta referências internas), mas pode ser corrigido para consistência.

---

### 6.3 Prioridade de Correção

| # | Item | Impacto no PDF | Esforço |
|---|------|---------------|---------|
| R1 | FP/FN repetido em §5.4 | Legibilidade / fluxo | Baixo |
| R2 | Diagnósticos MCMC duplicados | Comprimento | Médio |
| R3 | PPC explicado duas vezes | Comprimento | Baixo |
| **F1** | **`{#eq-ridge}` duplicado** | **Referência quebrada potencial** | **Baixo** |
| F2 | Penalização Lasso repetida | Estético | Baixo |
| R4 | VIF repetido 3×  | Fluxo | Baixo |
| R5 | Analogia Ridge/Lasso ↔ priori repetida | Fluxo | Baixo |
| F4 | Typo no label `verossimilhaneca` | Nenhum visível | Mínimo |

---

## 5. Resumo dos Principais Achados (para apoiar a escrita)

- **Dataset:** 30.000 clientes, Taiwan, 2005. PD30 ≈ 22% (desbalanceado).
- **Melhor preditor:** `atraso_pagto_0` — OR ~1,82 (frequentista) / ~1,93 (bayesiano padronizado).
- **Desempenho preditivo:** ROC AUC ~0,715 para todos os modelos; mínima vantagem dos bayesianos.
- **Prioris Normal vs. Laplace:** estimativas posteriores praticamente idênticas; a verossimilhança domina com n=30.000.
- **Multicolinearidade:** VIF máximo caiu de 41,12 → 1,84 com remoção de variáveis; sem impacto no AUC.
- **Ponto de corte 0,5:** sensibilidade ~25%, especificidade ~97%. Ponto de corte ótimo (Youden) ~0,26: sensibilidade ~55%, especificidade ~82%.
- **Diferença chave entre abordagens:** inferencial, não preditiva — intervalos de credibilidade bayesianos permitem afirmações probabilísticas diretas; frequentistas exigem interpretação assintótica.
