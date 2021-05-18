
# #! Luan Fiorentin
# #! data: 18-05-2021
# #! Modelos Lineares Generalizados


##### ** Pacotes
# Manipulação de dados:
library(tidyverse)


##### ** Simulando os dados
# Tamanho da amostra:
n <- 100
# Parâmetros:
b0 <- 1
b1 <- 0.33
# Gerando valores para covariável:
set.seed(18052021)
x <- runif(n = n, min = 1, max = 10)
# Gerando os valores de média
mu <- exp(b0 + b1 * x)
# Gerando os valores de respostas:
y <- rpois(n = n, lambda = mu)
# Dados simulados:
da <- data.frame("y" = y, "x" = x)
str(da)
head(da)
tail(da)


##### ** Verificando os dados graficamente:
barplot(y)
plot(da$y ~ da$x)
# Notar que há:
## Padrão não linear da resposta em relação a covariável.
## Variância não constante da respota.

##### ** Modelagem dos dados de contagem
# Verificando a documentação:
?glm
?family
# Modelo ajustado:
pm <- glm(
    formula = y ~ x,
    family = poisson(link = "log"),
    data = da
)
pm
summary(pm)
# Avaliando a qualidade do ajuste:
# install.packages("hnp")
hnp::hnp(pm)
## Modelo parece que está bem ajustado.


##### ** Predições da resposta
# Obtendo os valores preditos pelo modelo:
pred <- exp(coef(pm)[1] + coef(pm)[2] * x)
pred
fitted(pm)
# Obtendo a curva dos valores preditos:
plot(pred ~ x)
# Tudo em um único gráfico:
da$pred <- pred
# Gráfico
ggplot(data = da, aes(x = x, y = y)) +
    geom_point(alpha = 0.5, size = 2) +
    geom_line(
        aes(y = pred),
        size = 2,
        color = "red"
    )


##### ** Aplicação em dados reais
# Instalando e carregando pacotes:
# install.packages("forestmangr")
# https://cran.r-project.org/web/packages/forestmangr/forestmangr.pdf
library(forestmangr)
# Visualizando os dados:
data(exfm16)
da <- exfm16
str(da)
head(da)

# Modelo ajustado:
pm <- glm(
    formula = N ~ DH + age + as.factor(strata),
    family = poisson(link = "log"),
    data = da
)
summary(pm)
hnp::hnp(pm)
## Modelo não está bem ajustado.
## Há problemas de superdispersão
mean(da$N)
var(da$N)

glm(
    formula = N ~ DH + age + as.factor(strata),
    family = quasipoisson(link = "log"),
    data = da
) %>% hnp::hnp(.)
## Modelo ficou melhor ajustado.
## Ainda podemos melhorar se acomodar corretamente as covariáveis no modelo.
## Por que ocorre superdispersão?