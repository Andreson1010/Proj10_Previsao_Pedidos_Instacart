# Projetos com Feedback
# Formação Cientista de Dados
# Curso Business Analytics
# Sistema de Recomendação Para Rede de Varejo Usando Market Basket Analysis
# Objetivo do Projeto: prever quais produtos adquiridos anteriormente estarão no próximo pedido de um usuário


# Análise Exploratória

library(data.table)
library(dplyr)
library(ggplot2)
library(gridExtra)
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

#===============================================================================
# Juntando os datasets

df <- orders_products_train %>%
  left_join(products, by = c("product_id"))

df <- df %>%
  left_join(aisles, by = c("aisle_id"))

df<-df %>% 
  left_join(departments, by = c("department_id"))

df<- df %>%
  left_join(orders, by = c("order_id"))

str(df)
View(df)
dim(df)
#===============================================================================
# Análises de Produto
#===============================================================================


df_prod_mais_pedido <- df %>%
  group_by(product_name)%>%
  summarise(total = n(),
            reorders = sum(reordered, na.rm =TRUE))%>%
  arrange(desc(total)) %>%
  ungroup()


df_2 <- df %>%
  group_by(product_name)%>%
  summarise(total = n())

View(df_2)



View(df_prod_mais_pedido)
str(df_prod_mais_pedido)

# Gráfico de Produto mais pedido

temp_df_long <- df_prod_mais_pedido[1:20, ] %>%
  mutate(category = "Total") %>%
  bind_rows(
    df_prod_mais_pedido[1:20, ] %>%
      mutate(total = reorders, category = "Reorders")
  )

ggplot(temp_df_long, aes(x = reorder(product_name, total), y = total, fill = category)) +
  geom_bar(stat = "identity", alpha = 0.7) +
  coord_flip() +
  scale_fill_manual(values = c("Total" = "blue", "Reorders" = "red"), name = "Category") +
  labs(y = "Total Orders", x = "Product", title = "Most Popular Products") +
  theme_minimal()

# Banana é o produto mais popular entre os usuários

#==========================================================================

# Acrescentando a coluna taxa de repedido:

df_prod_mais_pedido<- df_prod_mais_pedido %>%
  mutate(reorder_ratio = reorders/total)

View(df_prod_mais_pedido)


View(total_users)

# Total de usuários por produto:

product_unique_users<- df %>%
  group_by(product_name)%>%
  summarise(total_user = n_distinct(user_id))%>%
  ungroup()


View(product_unique_users)

# juntando os dadtasets

product_unique_users<- product_unique_users %>%
  left_join(df_prod_mais_pedido, by = c("product_name"))
  
View(product_unique_users)

# Soma cumulativa de usuários únicos

product_unique_users <- product_unique_users %>%
  arrange(desc(total_user)) %>%
  mutate(cum_users = cumsum(total_user)) %>%
  rownames_to_column(var = "index") %>%
  select(-index)

View(product_unique_users)


ggplot(product_unique_users, aes(x = 1:nrow(product_unique_users), y = cum_users)) +
  geom_line() +
  labs(x = "Products", y = "Cumulative Sum of Unique Users",
       title = "Cumulative Sum of Unique Users Per Product") +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 9),
        axis.text.y = element_text(size = 9),
        title = element_text(size = 12))

# O gráfico mostra um aumento acentuado em certos pontos, isso indica que alguns
# produtos são extremamente populares e atraem um grande número de usuários
# únicos. Estes produtos podem ser considerados como "best-sellers" ou itens
# essenciais que são consistentemente preferidos pelos consumidores.
#
# após 15000 produtos, os produtos adicionais atraem cada vez menos
# novos usuários. Isso pode ser um sinal de que o mercado para esses produtos
# adicionais é limitado ou que eles são nichos específicos.


# Grafico Total de Produtos X Reorder

ggplot(product_unique_users, aes(x = total_user, y = reorders)) +
  geom_point() +
  labs(x = "Product Buyers", y = "Number of Product Reordered",
       title = "Total Product Orders VS Total Unique Product Reordered") +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 9),
        axis.text.y = element_text(size = 9),
        title = element_text(size = 12))



# Total de Pedidos versus Porcentagem de Repedido

ggplot(product_unique_users, aes(x = total_user, y = reorder_ratio)) +
  geom_point() +
  labs(x = "Product Buyers", y = "Reordered Percentage",
       title = "Total Product Orders VS Reordered Percentage") +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 9),
        axis.text.y = element_text(size = 9),
        title = element_text(size = 12))


# Ordem de adição ao pedido versus taxa de Repedido

temp_df <- df %>%
  group_by(add_to_cart_order)%>%
  summarise(reorder_ratio = mean(reordered))

View(temp_df)

library(ggplot2)

# Criando o gráfico de linha

ggplot(temp_df, aes(x = add_to_cart_order, y = reorder_ratio)) +
  geom_line(size = 1) +  # Linha com largura 1
  geom_point() +  # Adiciona marcadores de ponto
  labs(x = "Add to Cart Order", 
       y = "Reorder Ratio", 
       title = "Add to Cart Order VS Reorder Ratio") +
  theme_minimal()  # Tema minimalista


# Produtos mais Populares por dia da semana

temp_df2 <- df %>%
  group_by(order_dow, product_name)%>%
  arrange(desc(order_dow, count = n()))


temp_df2 <- df %>%
  group_by(order_dow, product_name) %>%
  summarise(counts = n(), .groups = 'drop') %>%
  arrange(order_dow, desc(counts)) %>%
  group_by(order_dow) %>%
  slice_max(n = 5, order_by = counts) %>%
  ungroup()


# Criando o gráfico de barras
p <- ggplot(temp_df2, aes(x = order_dow, y = counts, fill = product_name)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(x = "Day of Week", 
       y = "Total Orders of Most Frequent Products", 
       title = "Most Popular Products on different Days of Week",
       fill = "Product") +
  theme_minimal() +
  theme(legend.position = "bottom") # Posicionando a legenda na parte inferior

# Salvando o gráfico
ggsave("Most Popular Products on Different Days of Week.png", plot = p, width = 10, height = 6)

# Exibindo o gráfico
print(p)

write.csv(df, "meu_df/df")
#==============================================================================
# Análise de Corredor(Aisles)
#===============================================================================

temp_df3 <- df %>%
  group_by(aisle) %>%
  summarise(total = n(),  
            reorders = sum(reordered, na.rm = TRUE)) %>% 
  ungroup() %>%
  arrange(desc(total))

View(temp_df3)

temp_df_top20 <- temp_df3 %>%
  slice(1:20) %>%
  mutate(aisle = factor(aisle, levels = aisle))  

# Criando o gráfico de barras

ggplot(temp_df_top20) +
  geom_bar(aes(y = aisle, x = total), stat = "identity", fill = "blue", alpha = 0.7) +
  geom_bar(aes(y = aisle, x = reorders), stat = "identity", fill = "red", alpha = 0.7) +
  coord_flip() +
  labs(y = "Aisle", x = "Orders Count", title = "Total Orders and Reorders From Most Popular Aisles") +
  theme_minimal() +
  theme(legend.position = "right",
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = c("blue", "red"), labels = c("Total", "Reordered"), name = "Order Type")

# Os corredores mais populares são fresh vegetables e fresh fruits

# Corredores versus taxa de repedidos

temp_df_top20 <- temp_df3 %>%
  slice(1:20) %>%
  mutate(aisle = factor(aisle, levels = aisle))  # Convertendo aisle para fator para manter a ordem


# Criando o gráfico de barras para a taxa de repedido por corredor
ggplot(temp_df_top20, aes(y = aisle, x = reorder_ratio, fill = reorder_ratio)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(y = "Aisles", x = "Reorder Ratio", title = "Aisles with Highest Reorder Ratio") +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 12, angle = 45, hjust =1),
        axis.text.y = element_text(size = 12),
        legend.position = "none")  # Removendo a legenda


# Departments


temp_df4 <- df%>%
  group_by(department) %>%
  summarise(total = n(),
            reorder = sum(reordered, na.rm = TRUE))%>%
  ungroup()%>%
  arrange(desc(total))


ggplot(temp_df4) +
  geom_bar(aes(y = department, x = total), stat = "identity", fill = "blue", alpha = 0.7) +
  geom_bar(aes(y = department, x = reorder), stat = "identity", fill = "red", alpha = 0.7) +
  coord_flip() +
  labs(y = "Department", x = "Frequency", title = "Total Orders and Reorders From Departments") +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 12, angle = 45, hjust =1),
        legend.position = "right", ) +
  scale_fill_manual(values = c("blue", "red"), labels = c("Total", "Reordered"), name = "Order Type")

























