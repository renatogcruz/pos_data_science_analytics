if(!require(tidyverse)) install.packages("tidyverse")
library(tidyverse)

# este pacote mudam com frequência. Precisamos verificar se há atualizações
# sempre. Utilize 'tidyverse_update()' para fazer isso

tidyverse_update() # atualizando pacote

if(!require(nycflights13)) install.packages("nycflights13")
library(nycflights13)

if(!require(gapminder)) install.packages("gapminder")
library(gapminder)

if(!require(Lahman)) install.packages("Lahman")
library(Lahman)

1 + 2

# Funções são seguidas de parênteses, como sum() ou mean();
# Objetos estão sem parênteses, como flights ou x;
# Se quisermos deixar claro de qual pacote um objeto vem, usaremos o nome
# do pacote seguido por um par de dois-pontos, como dplyr::mutate()


