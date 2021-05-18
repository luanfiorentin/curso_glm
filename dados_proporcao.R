
# #! Luan Fiorentin
# #! data: 18-05-2021
# #! Modelos Lineares Generalizados


##### ** Pacotes
# Manipulação de dados:
library(tidyverse)


##### ** Simulando os dados
n <- 100
x <- rep(factor(c("Baixa", "Alta"), levels = c("Baixa", "Alta")), each = n / 2)
set.seed(18052021)
y <- c(
    rbernoulli(n = n / 2, p = 0.25),
    rbernoulli(n = n / 2, p = 0.70)
)
da <- data.frame("y" = y, "x" = x)
da


##### ** Verificando os dados graficamente:
ggplot(data = da, aes(x = y)) +
    geom_bar()

ggplot(data = da, aes(x = y)) +
    geom_bar(aes(fill = x), position = position_dodge())


##### ** Modelagem dos dados de contagem
# Verificando a documentação:
?glm
?family
# Modelo ajustado:
bm <- glm(
    formula = y ~ x,
    family = binomial(link = "logit"),
    data = da
)
bm
summary(bm)
# Avaliando a qualidade do ajuste:
# install.packages("hnp")
hnp::hnp(bm)
## Modelo parece que está bem ajustado.


##### ** Predições da resposta
# Organizando a covariável
da <- da %>% mutate(xx = if_else(x == "Baixa", 0, 1))
# Obtendo os valores preditos pelo modelo:
lp <- coef(bm)[1] + coef(bm)[2] * da$xx
pred <- exp(lp) / (1 + exp(lp))
pred
fitted(bm)


##### ** Aplicação em dados reais
# Instalando e carregando pacotes:
# install.packages("forestmangr")
# https://cran.r-project.org/web/packages/forestmangr/forestmangr.pdf
library(forestmangr)
# Visualizando os dados:
data(exfm20)
da <- exfm20
str(da)
head(da)
# Análise descritiva dos dados
da %>%
    group_by(as.factor(dead)) %>%
    summarise(
        mage = mean(dbh)
    )
# Separando dados para ajuste e validação
ggplot(data = da, aes(x = dbh, y = dead, colour = dead)) +
    geom_point() +
    geom_jitter(height = 0.5, alpha = 0.25)
# Dados de ajuste e validação
set.seed(1)
indices <- sample(dim(da)[1], size = 10000)
treino <- da[indices, ]
teste <- da[-indices, ]
prop.table(table(treino$dead))
prop.table(table(teste$dead))
# Modelo ajustado:
bm <- glm(
    formula = dead ~ dbh,
    family = binomial(link = "logit"),
    data = treino
)
summary(bm)
hnp::hnp(bm)
## Modelo não está bem ajustado.


##### ** Predições da resposta
# Obtendo os valores preditos pelo modelo:
lp <- coef(bm)[1] + coef(bm)[2] * treino$dbh
pred <- exp(lp) / (1 + exp(lp))
pred
fitted(bm)
# Gráfico
da$pred <- pred
str(da)
ggplot(data = treino, aes(x = dbh, y = pred)) +
    geom_line(
        size = 2,
        color = "red"
    ) +
    ylim(0, 1)
# Proporção de árvores vivas e mortas
prop.table(table(treino$dead))

# Gráfico completo
novox <- seq(0, 500, lenght = 1000)
novolp <- coef(bm)[1] + coef(bm)[2] * novox
novopred <- exp(novolp) / (1 + exp(novolp))
novoda <- data.frame(novox = novox, lp = novopred)
ggplot(data = novoda, aes(x = novox, y = novopred)) +
    geom_line(
        size = 2,
        color = "red"
    ) +
    ylim(0, 1)


##### ** Matriz de confusão
newdata <- data.frame("dead" = teste$dead)
newdata <- newdata %>%
    mutate(dead = ifelse(dead == TRUE, "1", "0"))
newdata <- newdata %>% mutate(dead = as.factor(dead))
# Predito nos dados de validação
newdata$pred <- as.factor(
    ifelse(
        predict(bm,
            newdata = teste,
            type = "response"
        )
        > 0.50, "1", "0"
    )
)
head(newdata)
# Matriz
caret::confusionMatrix(
    data = newdata$pred,
    reference = newdata$dead,
    positive = "1"
)
# Construindo:
library(ROCR)
pvalida <- predict(bm, type = "response", newdata = teste %>% select(dbh))
predicao <- prediction(predictions = pvalida, labels = teste$dead)
# Plotando a curva ROC
?performance
perf <- performance(predicao, measure = "tpr", x.measure = "fpr")
# tpr: True Positive Rate; fpr: False Positive Rate.
plot(perf,
    colorize = TRUE,
    # print.cutoffs.at = seq(0.05,0.95,0.05),
    lwd = 2,
    xlab = "1-Especificidade", ylab = "Sensibilidade", main = "Mstep"
)
abline(0, 1, lty = 2)
# Extraindo a área sob a curva:
area <- performance(predicao, "auc")
area


##### ** Curva ROC - dados de ajuste
# install.packages("pROC")
library(pROC)
curvaroc <- plot.roc(treino$dead, fitted(bm))
# Gráfico
plot(curvaroc,
    print.auc = TRUE,
    auc.polygon = TRUE,
    grud = c(0.1, 0.2),
    grid.col = c("green", "red"),
    max.auc.polygon = TRUE,
    auc.polygon.col = "lightgreen",
    print.thres = TRUE
)
