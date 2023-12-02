# Projeto com feedBack - Projeto 10 - Sistema de Recomendação para Rede de
# Varejo Usando Market Basket Analysis

# Formação Cientista de Dados

# Busines Analytics

# Objetivo:
# Neste projeto de ciência de dados, o objetivo será prever quais produtos adquiridos
# anteriormente estarão no próximo pedido de um usuário.


# Dicionário de Dados:

# orders (3.4m rows, 206k users):
#
# order_id: order identifier 
# user_id: customer identifier eval_set: which
# evaluation set this order belongs in (see SET described below) 
# order_number: the order sequence number for this user (1 = first, n = nth) 
# order_dow: the day of the week the order was placed on 
# order_hour_of_day: the hour of the day the order was placed on 
# days_since_prior: days since the last order, capped at
# 30 (with NAs for order_number = 1) 
#
# products (50k rows):
#
# product_id: product identifier 
# product_name: name of the product 
# aisle_id: foreign key 
# department_id: foreign key 
#
# aisles (134 rows):
#
# aisle_id: aisle identifier 
# aisle: the name of the aisle 
#
# deptartments (21 rows):
#
# department_id: department identifier 
# department: the name of the department
#
# order_products(30m+ rows):
#
# order_id: foreign key 
# product_id: foreign key 
# add_to_cart_order: order in which each product was added to cart 
# reordered: 1 if this product has been ordered by this user in the past, 0 
# otherwise where SET is one of the four following evaluation sets (eval_set in orders):
#
# "prior": orders prior to that users most recent order (~3.2m orders) 
# "train": training data supplied to participants (~131k orders) 


# ==============================================================================
# Carregando os Pacotes 
# ==============================================================================


library(data.table)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(caret)
library(xgboost)
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
# Tabela de Departamentos

departments <- fread("archive/departments.csv")
View(departments)
dim(departments)

# Tabela de Corredores (Aisles)

aisles <- fread("archive/aisles.csv")
View(aisles)

# Tabela de pedidos de produtos
orders_products_train <- fread("archive/order_products__train.csv")
View(orders_products_train)
dim(orders_products_train)



# tabela de prior 

#orders_products2 <- fread("archive/order_products__prior.csv")

# Tabela prior tem mais de 30 milhões de linhas. 
# Por restrição da memória do computador vamos trabalhar com 
# 'order_products__train.csv'.


#-------------------------------------------------------------------------------
# Preparando os dados
#-------------------------------------------------------------------------------

# Juntando os datasets

df_inner <- orders %>%
  inner_join(orders_products_train, by = c("order_id"))
  

df <- df_inner %>%
  left_join(products, by = c("product_id"))

# Verificando valores ausentes

na_count<- colSums((is.na(df)))
print(na_count)

View(head(df))

str(df)

#===============================================================================
#Engenharia de Atributos
#===============================================================================

# Produtos por usuário

products_per_user <- df %>%
  group_by(user_id) %>%
  summarise(unique_product_count = n_distinct(product_id))

df <- left_join(df, products_per_user, by = c("user_id"))
View(df)


# Quantidade de vezes que um usuário comprou um produto

df<- df %>% group_by(user_id, product_id)%>%
              mutate(user_buy_product_times = row_number()) %>%
  ungroup()

View(df)

colSums((is.na(df)))
# Agregações por Produto

df_atributos_products<- df %>%
  group_by(product_id) %>%
  summarise(
    mean_add_to_cart_order = mean(add_to_cart_order, na.rm = TRUE),
    total_orders = n(),
    total_reorders = sum(reordered, na.rm = TRUE),
    reorder_percentage = mean(reordered, na.rm = TRUE),
    unique_users = n_distinct(user_id),
    order_first_time_total_cnt = sum(user_buy_product_times == 1, na.rm = TRUE),
    order_second_time_total_cnt = sum(user_buy_product_times == 2, na.rm = TRUE)) %>%
  ungroup()

View(df_atributos_products)


# Agregações por Corredor(Aisle)

df_atributos_aisle <- df %>%
  group_by(aisle_id) %>%
  summarise(
    aisle_mean_add_to_cart_order = mean(add_to_cart_order, na.rm = TRUE),
            aisle_total_orders = n(),
            aisle_total_reorders = sum(reordered, na.rm = TRUE),
            aisle_reorder_percentage = mean(reordered, na.rm = TRUE),
            aisle_unique_users = n_distinct(user_id))%>%
    ungroup()

View(df_atributos_aisle)

# Agragações por departamento:

df_atributos_department <- df %>%
  group_by(department_id)%>%
  summarise(department_mean_add_to_cart_order = mean(add_to_cart_order, na.rm = TRUE),
            department_total_orders = n(),
            department_total_reorders = sum(reordered, na.rm = TRUE),
            department_reorder_percentage = mean(reordered, na.rm = TRUE),
            department_unique_users = n_distinct(user_id))%>%
  ungroup()
View(df_atributos_department)


# Juntando os datasets:

df_atributos_products <- df_atributos_products %>%
  left_join(products, by = c("product_id"))

df_atributos_products <- df_atributos_products %>%
  left_join(df_atributos_aisle, by = c("aisle_id"))

df_atributos_products <- df_atributos_products %>%
  left_join(aisles, by = c("aisle_id"))

df_atributos_products <- df_atributos_products %>%
  left_join(df_atributos_department, by = c("department_id"))

df_atributos_products <- df_atributos_products %>%
  left_join(departments, by = c("department_id"))

df_atributos_products_ <- df_atributos_products %>%
  left_join(orders_products_train, by = c("product_id"))

View(df_atributos_products)


# ==============================================================================

df_products <- df_atributos_products %>%
  left_join(df, by = c("product_id"))

View(df_products)

colSums(is.na(df_products))



View(df_products_disticnt)

# Retirando as colunas product_names, aisle_id, department_id por entender
# que não serão necessárias.


df_atributos_products <- df_atributos_products %>%
  select(-product_name, -aisle_id, -department_id)
View(df_atributos_products)


dim(df_atributos_products)

str(df_atributos_products)
sapply(df_atributos_products, class)

# Aplicando biinary encoder nas colunas aisles e department.
# Este método de codificação é útil quando você tem muitas categorias 
# únicas e deseja reduzir a dimensionalidade, pois a codificação one-hot 
# pode resultar em um grande número de colunas se houver muitas categorias
# únicas.

binary_encoder <- function(df, cols) {
  for (col in cols) {
    # Converter a coluna para um fator e depois para um número inteiro
    int_vals <- as.integer(as.factor(df[[col]]))
    
    # Encontrar o número máximo de dígitos binários necessários
    max_bin_digits <- ceiling(log2(length(unique(int_vals))))
    
    # Converter os valores inteiros para binário e dividir em colunas
    bin_cols <- t(sapply(int_vals, function(x) {
      bin_vec <- rev(as.numeric(intToBits(x))[1:max_bin_digits])
      if (length(bin_vec) < max_bin_digits) {
        bin_vec <- c(rep(0, max_bin_digits - length(bin_vec)), bin_vec)
      }
      return(bin_vec)
    }))
    
    # Adicionar as novas colunas ao dataframe
    for (i in 1:ncol(bin_cols)) {
      col_name <- paste0(col, "_bin_", i)
      df[[col_name]] <- bin_cols[, i]
    }
    
    # Remover a coluna original
    df[[col]] <- NULL
  }
  return(df)
}

# Aplicar a função ao dataframe
df_atributos_products <- binary_encoder(df_atributos_products, c('aisle', 'department'))

View(df_atributos_products)

dim(df_atributos_products)

colnames(df_atributos_products)

# Verificando valores NA
colSums(is.na(df_atributos_products))


# Juntando os dataframes

df_products <- df_atributos_products %>%
  left_join(df, by = c("product_id"))

View(df_products)

#  Agragações por Usuário

df_atributos_user <- df %>%
  group_by(user_id)%>%
  summarise(
    avg_dow = mean(order_dow, na.rm = TRUE),
    std_dow = sd(order_dow, na.rm = TRUE),
    avg_doh = mean(order_hour_of_day, na.rm = TRUE),
    std_doh = sd(order_hour_of_day, na.rm = TRUE),
    avg_since_order = mean(days_since_prior_order, na.rm = TRUE),
    std_since_order = sd(days_since_prior_order, na.rm = TRUE),
    total_orders_by_user = n_distinct(order_number),
    total_products_by_user = n(),
    total_unique_product_by_user = n_distinct(product_id),
    total_reorders_by_user = sum(reordered, na.rm = TRUE),
    reorder_propotion_by_user = mean(reordered, na.rm = TRUE)
  )

View(df_atributos_user)
dim(df_atributos_user)

colSums(is.na(df_atributos_user))


# Juntando dataframes

df_final <- df_products %>%
  left_join(df_atributos_user, by = c("user_id"))

# Retirando linhas duplicadas se existir

df_final <- df_final%>%
  distinct()
dim(df_final)

# Verificando valores NA
colSums(is.na(df_final))
# Existem valores NA nas colunas std_doh, std_dow, std_since_order 

df_final$std_dow[is.na(df_final$std_dow)] <- 0
df_final$std_doh[is.na(df_final$std_doh)] <- 0
df_final$std_since_order[is.na(df_final$std_since_order)] <- 0

colSums(is.na(df_final))

# Retirando as colunas 

colnames(df_final)

# Retirando as colunas order_id, -product_name,-aisle_id

df_final <- df_final %>%
  select(-order_id, -product_name,-aisle_id)

View(df_final)
dim(df_final)

#Reposicioando 

df_final <-df_final[c(setdiff(names(df_final), "reordered"), "reordered")]
View(head(df_final))
colnames(df_final)


#Gravando arquivo final
write.csv(df_final, "meu_df/df_final.csv")
















































































































































