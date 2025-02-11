# ler dados
library(MASS)
data(Boston)
dados <- Boston
library(ggplot2)

#predizer o valor m�dio das casas ocupadas pelo propriet�rio (medv) 
#compara��o entre os modelos por meio de um processo de valida��o cruzada utilizando como medidas 
#o erro quadr�tico m�dio e correla��o entre os valores preditos e observados. 


# primeiros registros
head(Boston)

# dados
str(Boston)

# sumario estatistico
summary(Boston)

y <- Boston$medv
#todos os dados exceto o medv
x <- Boston[ , -which(names(Boston) == "medv")]
library(caret)
set.seed(123)

# Definir controle para validacao cruzada com 10 folds
train_control <- trainControl(method = "cv", number = 10)

pairs(dados, col = 2, pch = 19)

#Analise de correlacao
cor(Boston)
plot(Boston$rm, Boston$medv)
0
# 1. Modelo de Regress�o Linear Simples (lstat)
modeloLinearSimples <- train(medv ~ lstat, data = dados, method = "lm", trControl = train_control)
metricas_simples <- postResample(predict(modeloLinearSimples, dados), dados$medv)

# 2. Modelo de Regress�o Linear M�ltipla (lstat e rm)
modeloLinearMultiplo <- train(medv ~ lstat + rm, data = dados, method = "lm", trControl = train_control)
metricas_multipla <- postResample(predict(modeloLinearMultiplo, dados), dados$medv)

# 3. Modelo de �rvore de Regress�o (usando lstat e rm)
modeloArvore <- train(medv ~ lstat + rm, data = dados, method = "rpart", trControl = train_control)
metricas_arvore <- postResample(predict(modeloArvore, dados), dados$medv)

comparacao <- data.frame(
  Model = c("Linear (lstat)", "Multiple (lstat + rm)", "Tree"),
  RMSE = c(metricas_simples["RMSE"], metricas_multipla["RMSE"], metricas_arvore["RMSE"]),
  R2 = c(metricas_simples["Rsquared"], metricas_multipla["Rsquared"], metricas_arvore["Rsquared"])
)

print(comparacao)


# Gr�fico para regress�o linear simples (lstat vs medv)
plot_lstat_simple <- ggplot(Boston, aes(x = lstat, y = medv)) +
  geom_point(color = "blue", alpha = 0.6) +
  geom_smooth(method = "lm", formula = y ~ x, color = "red", se = FALSE, linewidth = 1) +
  labs(title = "Regress�o Linear Simples: medv vs lstat",
       x = "LSTAT",
       y = "MEDV")

# Gerar predi��es usando o modelo m�ltiplo
Boston$predicted_medv_multiplo <- predict(modeloLinearMultiplo, newdata = dados)

# Gr�fico para regress�o linear m�ltipla: lstat vs medv
plot_lstat_multiple <- ggplot(Boston, aes(x = lstat, y = medv)) +
  geom_point(color = "blue", alpha = 0.6) +
  geom_line(aes(y = predicted_medv_multiplo), color = "red", linewidth = 1) +
  labs(title = "Regress�o Linear M�ltipla: medv vs lstat (Curva Ajustada)",
       x = "LSTAT",
       y = "MEDV")

# Gr�fico para regress�o linear m�ltipla: rm vs medv
plot_rm_multiple <- ggplot(Boston, aes(x = rm, y = medv)) +
  geom_point(color = "blue", alpha = 0.6) +
  geom_line(aes(y = predicted_medv_multiplo), color = "red", linewidth = 1) +
  labs(title = "Regress�o Linear M�ltipla: medv vs rm (Curva Ajustada)",
       x = "RM",
       y = "MEDV")

plot_lstat_simple

plot_lstat_multiple

plot_rm_multiple

# Visualiza��o da �rvore de regress�o
plot(modeloArvore$finalModel)
text(modeloArvore$finalModel, pretty = 0)
#grid.arrange(plot_lstat_simple, plot_lstat_multiple, plot_rm_multiple, nrow = 3)

