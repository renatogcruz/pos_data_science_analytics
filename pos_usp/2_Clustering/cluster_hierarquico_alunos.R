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
#         CLUSTER HIERARQUICO - juntos
#
########################################

#LEITURA DOS DADOS

alunos_pap <- read.table("alunos_pap.csv", sep = ";", header = T, dec = ",")

View(alunos_pap)

rownames(alunos_pap) <- alunos_pap[,1] #nome das linhas recebem primeira variavel

alunos_pap <- alunos_pap[,-1]          #deletando primeira coluna (categórica)

#CALCULANDO MATRIZ DE DISTANCIAS

d <- dist(alunos_pap, method = "euclidean") #distancia euclidiana
d

#DEFININDO O CLUSTER A PARTIR DO METODO ESCOLHIDO
#metodos disponiveis "average", "single", "complete" e "ward.D"

hc1 <- hclust(d, method = "single" )    # Single Linkage - Vizinho mais próximo
hc2 <- hclust(d, method = "complete" )  # Complete Linkage - Vizinho mais longe
hc3 <- hclust(d, method = "average" )   # Avarage Linkage - Média
hc4 <- hclust(d, method = "ward.D" )    # Ward´s Method

#DESENHANDO O DENDOGRAMA

plot(hc1, cex = 0.6, hang = -1)
plot(hc2, cex = 0.6, hang = -1)
plot(hc3, cex = 0.6, hang = -1)
plot(hc4, cex = 0.6, hang = -1)

#BRINCANDO COM O DENDOGRAMA PARA 2 GRUPOS

rect.hclust(hc4, k = 2)

#COMPARANDO DENDOGRAMAS
#comparando o metodo average com ward

dend3 <- as.dendrogram(hc3)
dend4 <- as.dendrogram(hc4)
dend_list <- dendlist(dend3, dend4) 

#EMARANHADO, quanto menor, mais iguais os dendogramas sao

tanglegram(dend3, dend4, main = paste("Emaranhado =", round(entanglement(dend_list),2)))
#agora comparando o metodo single com complete

dend1 <- as.dendrogram(hc1)
dend2 <- as.dendrogram(hc2)
dend_list2 <- dendlist(dend1, dend2) 

#EMARANHADO, quanto menor, mais iguais os dendogramas sao
tanglegram(dend1, dend2, main = paste("Emaranhado =", round(entanglement(dend_list2),2)))

#criando 2 grupos de alunos
grupo_alunos2 <- cutree(hc4, k = 2)
table(grupo_alunos2)

#transformando em data frame a saida do cluster
alunos_grupos <- data.frame(grupo_alunos2)

#juntando com a base original
Base_alunos_fim <- cbind(alunos_pap, alunos_grupos)

# entendendo os clusters
#FAZENDO ANALISE DESCRITIVA

#MEDIAS das variaveis por grupo
mediagrupo_alunos <- Base_alunos_fim %>% 
  group_by(grupo_alunos2) %>% 
  summarise(n = n(),
            Portugues = mean(Portugues), 
            Matematica = mean(Matematica))
mediagrupo_alunos