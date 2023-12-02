# Projetos com Feedback
# Formação Cientista de Dados
# Curso Business Analytics
# Sistema de Recomendação Para Rede de Varejo Usando Market Basket Analysis
# Objetivo do Projeto: prever quais produtos adquiridos anteriormente estarão
#no próximo pedido de um usuário

# ------------------------------------------------------------------------------
# Carregando Pacotes Necessários
# ------------------------------------------------------------------------------
library(data.table)
library(dplyr)
library(arules)
library(ggplot2)
library(gridExtra)
library(wordcloud)
library(xgboost)
library(caret)



df <- fread("meu_df/df_final.csv")
View(df)
str(df)
colnames(df)
df<-df %>%
  select(-V1, -eval_set)

products<- fread("archive/products.csv")
  
View(head(df))

#===============================================================================
# Dividivndo os Dados
# ==============================================================================

divisao <- createDataPartition(df$reordered, p = .8, list = FALSE)
df_treino <- df[divisao,]
df_teste <- df[-divisao,]

# Criação de matriz xgb
mat_treino <- xgb.DMatrix(as.matrix(df_treino %>% select(-reordered))
                          ,label = df_treino$reordered)

modelo <- xgboost(dat = mat_treino, nrounds = 50)

# Salvando modelo
xgb.save(modelo, fname = "modelo")

# Aplicação do modelo no grupo de teste
mat_teste <- xgb.DMatrix(as.matrix(df_teste %>% select(-reordered))
                         ,label = df_teste$reordered)

df_teste$previsoes <- predict(modelo, mat_teste)

View(df_teste)

# Como o algoritimo xgboost nos entrega uma probabilidade, ou seja, se a 
# previsão foi de 80%,
# isto significa que existe 80% de chances daquele produto ser pedido novamente,
#vamos aplicar um limiar (threshold) para selecionar apenas previsões >= 70%

df_teste$previsoes <- ifelse(df_teste$previsoes >= .7, 1, 0)

# Avaliação do modelo
# Para saber se o modelo foi bem sucedido, vou verificar se as previsoes 
# acertaram se comparadas às verdadeiras classificações do dataset

df_teste$sucesso <- ifelse(df_teste$reordered == df_teste$previsoes, "sim", "não")

df_teste %>%
  group_by(sucesso) %>%
  summarize(contagem = n()) %>%
  mutate(`% de acertos` = paste0(round(((contagem / sum(contagem)) * 100), 2), " %"))

colnames(df_teste)

# Nosso modelo acertou pouco mais 77% dos casos, sendo satisfatório.
# Vamos tentar alcançar um pouco mais 77% com um novo treinamento com mais rounds

novo_modelo <- xgboost(dat = mat_treino, nrounds = 70)

# Salvando novo modelo
xgb.save(novo_modelo, fname = "novo_modelo")

df_teste$previsoes2 <- predict(novo_modelo, mat_teste)

df_teste$previsoes2 <- ifelse(df_teste$previsoes2 >= .7, 1, 0)

# Agora, para avaliar o modelo, criaremos uma matriz de confusão
# A matriz de confusão é uma tabela de contingência de tamanho 2 x 2
# que nos permite identificar quantas vezes o modelo acertou e quantas ele errou

confusionMatrix(as.factor(df_teste$previsoes2), as.factor(df_teste$reordered))

# Não obtivemos resultado muito diferente, a acurácia segue mais ou menos por 
# volta de 77%. Na matriz de confusão observamos uma quantidade menor de falsos 
# positivos(quantidade de vezes que o produto não foi pedido novamente mas o 
# modelo previu que sim) e uma quantidade alta decasos de falsos negativos(
# quantidade de vezes que o produto foi pedido novamente mas o modelo previu que não)


# ---------------------------------------------------------------------------------------
# Quais produtos previstos para os usuários em seu próximo pedido?
# ---------------------------------------------------------------------------------------


previsoes <- df_teste %>%
  filter(previsoes2 == 1) %>%
  inner_join(products, by ="product_id") %>%
  group_by(user_id) %>%
  summarize(produtos_previstos = paste0(product_name, collapse = ", "))

View(previsoes)

produtos_reais <- df_teste %>%
  filter(reordered == 1) %>%
  inner_join(products, by ="product_id") %>%
  group_by(user_id) %>%
  summarize(produtos_reais = paste0(product_name, collapse = ", "))

comparando_produtos <- previsoes %>%
  inner_join(produtos_reais, by = "user_id")

View(comparando_produtos)

# Salvando df comparativo de produtos previstos x produtos reais
write.csv(comparando_produtos, file = "df_novo/comparando_produtos.csv", row.names = F, quote = F)

sub <- df_teste %>%
  filter(previsoes2 == 1) %>%
  group_by(user_id) %>%
  summarize(produtos = paste0(product_id, collapse = " "))

write.csv(sub, file = "df_novo/resultado_final.csv", row.names = F, quote = F)


# Formação Cientista de Dados - Data Science Academy