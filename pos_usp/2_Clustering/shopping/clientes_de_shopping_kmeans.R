#############################################################
#
#    CLUSTER NAO HIERARQUICO - Clientes de um Shopping
#
#############################################################


# carregando as bibliotecas

library(tidyverse)  # pacote para manipulação de dados
library(cluster)    # algoritmo de cluster
library(factoextra) # algoritmo de cluster e visualização
library(fpc)        # algoritmo de cluster e visualização
library(gridExtra)  # para a funcao grid arrange
library(readxl)     # leitura dos dados


# carregando a base de dados

clientes <- read.table('Mall_Customers.csv',sep = ",", header = T, dec = ",")
rownames(clientes) <- clientes[,1]
clientes <- clientes[,-c(1,2,3)]

clientes <- clientes %>% rename(salario.anual=Annual.Income..k..,
                    score=Spending.Score..1.100.
                    )

# padronizando os dados

clientes.padronizado <- scale(clientes)


# verificando o número de clusters com o elbow

fviz_nbclust(clientes.padronizado, FUN = hcut, method = "wss")


# testando de 3 a 6 centros para visualizar a melhor divisão

clientes.k3 <- kmeans(clientes.padronizado,centers=3)
clientes.k4 <- kmeans(clientes.padronizado,centers=4)
clientes.k5 <- kmeans(clientes.padronizado,centers=5)
clientes.k6 <- kmeans(clientes.padronizado,centers=6)


# gráficos

G1 <- fviz_cluster(clientes.k3, geom="point", data=clientes.padronizado) + ggtitle("k=3")
G2 <- fviz_cluster(clientes.k4, geom="point", data=clientes.padronizado) + ggtitle("k=4")
G3 <- fviz_cluster(clientes.k5, geom="point", data=clientes.padronizado) + ggtitle("k=5")
G4 <- fviz_cluster(clientes.k6, geom="point", data=clientes.padronizado) + ggtitle("k=6")


# matriz dos gráficos

grid.arrange(G1,G2,G3,G4, nrow=2)


# juntando os dados

clientes2 <- read.table('Mall_Customers.csv',sep = ",", header = T, dec = ",")
clientesfit <- data.frame(clientes.k5$cluster)

clientes.final <- cbind(clientes2,clientesfit)

# análise descritiva dos grupos

## média das variaveis

mediagrupo <- clientes.final %>% 
  group_by(clientes.k5.cluster) %>% 
  summarise(n=n(),
            salario.anual=mean(Annual.Income..k..),
            score=mean(Spending.Score..1.100.)
            )
mediagrupo


# salvando o dataframe com a coluna de cluster

write.csv(clientes.final,file = "clientesfinal.csv")







