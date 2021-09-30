########################################
#
#   CHAMANDO BIBLIOTECAS IMPORTANTES
#
########################################

library(tidyverse) #pacote para manipulacao de dados
library(cluster) #algoritmo de cluster
library(dendextend) #compara dendogramas
library(factoextra) #algoritmo de cluster e visualizacao
library(fpc) #algoritmo de cluster e visualizacao
library(gridExtra) #para a funcao grid arrange
library(readxl)


########################################
#
#   Brincando e comparando todos os métodos com dbscan
#
########################################

#Carregar base de dados: 
notas_categ <- as.data.frame(read_excel("notas_categ.xlsx"))

#pegando os dados que usaremos
notas_alunos <- notas_categ %>% 
  select(Estudante, Atuaria, Mkt)

#para visualizar no plano
notas_alunos %>% ggplot() +
  geom_point(aes(x = Atuaria,
                 y = Mkt),
             size = 3)

#Transformar o nome 
rownames(notas_alunos) <- notas_alunos[,1]
notas_alunos <- notas_alunos[,-1]

#Padronizar variaveis
notas_alunos_pad <- scale(notas_alunos) # aqui não precisava fazer isso

#calcular as distancias da matriz utilizando a distancia euclidiana
distancia <- dist(notas_alunos_pad, method = "euclidean")



### método hiearquico

#Calcular o Cluster
cluster.hierarquico <- hclust(distancia, method = "single" )

# Dendrograma
plot(cluster.hierarquico, cex = 0.6, hang = -1)

#criando grupos
grupo_alunos_hierarquico <- cutree(cluster.hierarquico, k = 3)
table(grupo_alunos_hierarquico)

#transformando em data frame a saida do cluster
grupo_alunos_hierarquico <- data.frame(grupo_alunos_hierarquico)

#juntando com a base original
notas_alunos_fim <- cbind(notas_alunos, grupo_alunos_hierarquico)

#visualizando em cores os clusters
notas_alunos_fim %>% ggplot() +
  geom_point(aes(x = Atuaria,
                 y = Mkt,
                 color = as.factor(grupo_alunos_hierarquico)),
             size = 3)



### método k-means

#Calcular o Cluster
cluster.k3 <- kmeans(notas_alunos_pad, centers = 3)

#criando grupos
grupo_alunos_kmeans3 <- data.frame(cluster.k3$cluster)

#juntando com a base original
notas_alunos_fim <- cbind(notas_alunos_fim, grupo_alunos_kmeans3)

#visualizando em cores os clusters
notas_alunos_fim %>% ggplot() +
  geom_point(aes(x = Atuaria,
                 y = Mkt,
                 color = as.factor(cluster.k3.cluster)),
             size = 3)


### método dbscan

#Calcular o Cluster
dbscan <- fpc::dbscan(notas_alunos_pad,eps = 0.56, MinPts = 3)

notas_alunos_fim$dbscan <- dbscan$cluster

#visualizando em cores os clusters
notas_alunos_fim %>% ggplot() +
  geom_point(aes(x = Atuaria,
                 y = Mkt,
                 color = as.factor(dbscan)),
             size = 3)

