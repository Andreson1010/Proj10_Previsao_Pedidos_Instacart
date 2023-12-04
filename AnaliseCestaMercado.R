# Projeto com feedBack - Projeto 10 - Sistema de Recomendação para Rede de
# Varejo Usando Market Basket Analysis

# Formação Cientista de Dados

# Busines Analytics

# Objetivo:
# Neste projeto de ciência de dados, o objetivo será prever quais produtos adquiridos
# anteriormente estarão no próximo pedido de um usuário.


library(arules)
library(arulesViz)
library(data.table)
library(dplyr)
library(tidyr)
library(tibble)
#==============================================================================
# Carregando os dados
#==============================================================================

# Tabela de pedidos
orders <- fread("archive/orders.csv")
View(orders)
dim(orders)

# Preenchendo os valores NA com 0(zero. Issso indica que só
# houve uma compra.
orders$days_since_prior_order[is.na(orders$days_since_prior_order)]<-0
View(orders)

# Tabela de produtos
products<- fread("archive/products.csv")
View(products)
dim(products)
# # Tabela de Departamentos
# 
# departments <- fread("archive/departments.csv")
# View(departments)
# dim(departments)
# 
# # Tabela de Corredores (Aisles)
# 
# aisles <- fread("archive/aisles.csv")
# View(aisles)

# Tabela de pedidos de produtos
orders_products_train <- fread("archive/order_products__train.csv")
View(orders_products_train)
dim(orders_products_train)


# Juntando os dados
# df_inner <- orders %>%
#   inner_join(orders_products_train, by = c("order_id"))
# 
# 
# df <- df_inner %>%
#   left_join(products, by = c("product_id"))

# View(df)

# Vamos focar apenas nos produtos que são comprados com frequência. Isso pode ajudar a
# identificar padrões de compra ou combinações de produtos que ocorrem
# regularmente.

# seleciona os 100 'product_id' mais frequentes, 

df<- orders_products_train %>%
  left_join(products, by = c("product_id"))

View(head(df))
dim(df)

product_counts <- orders_products_train %>%
  group_by(product_id) %>%
  summarise(frequency = n())%>%
  arrange (desc(frequency))%>%
  head(100)%>%
  left_join(products, by = c("product_id"))


View(head(product_counts))

# Filtrar o df com lista dos 100 produtos mais frequentes

freq_products <- product_counts$product_id

length(freq_products)

order_products_mais_frequentes <- orders_products_train[orders_products_train$product_id %in% freq_products, ]

dim(order_products_mais_frequentes)

View(head(order_products_mais_frequentes))


# juntando o dataset
order_products <- order_products_mais_frequentes %>%
  left_join(products, by=c('product_id'))


View(head(order_products))


# Primeiro, criamos uma tabela de contingência

basket <- dcast(order_products, order_id ~ product_name, function(x) length(x) >= 1, value.var = "product_id")

View(head(basket))
dim(basket)

# Convertendo para um objeto de transações
transacoes <- as(basket[, -1], "transactions")

View(transacoes)


# Gerando regras de associação


rules <- apriori(data = transacoes, 
                 parameter = list(supp = 0.001, conf = 0.2, minlen = 2, target = "rules"))

# filtrando para lift >1

rules_lift <- subset(rules, lift > 1)

#Ordenando po lift

rules_sorted <- sort(rules_lift, by = "lift", decreasing = TRUE)


#Visualizar as Regras Ordenadas 
inspect(head(rules_sorted, 5))


# Grafico de Regras de Associação


plot(rules, method = "scatterplot", measure = c("support", "lift"), shading = "confidence")



plot(rules, method = "graph", control = list(type = "items"))


plot(rules, method = "matrix", measure = "lift", control = list(reorder = "none"))


plot(rules, method = "grouped")





















