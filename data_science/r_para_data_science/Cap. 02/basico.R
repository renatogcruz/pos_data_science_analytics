# capítulo 2 - fluxo de trabalho: o básico

1 / 200 * 30
(59 + 73 + 2) / 3
sin(pi / 2)

pi


x <- 3 * 4
x

object_name <- value

x
este_eh_um_nome_bem_longo <- 2.5
este_eh_um_nome_bem_longo
este_eh_um_nome_bem_longo <- 3.5
este_eh_um_nome_bem_longo

r_rocks <- 2 ^ 3
r_rocks


# chamadas de funções
function_name(arg1 = val1, arg2 = val2, ...)

seq(1,10)

# outra forma
1:10

x <- "hello world"
x

y <- seq(1, 10, length.out = 5)
y

(y <- seq(1, 10, length.out = 5))


# Exercícios
library(tidyverse)

ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy))

filter(mpg, cyl > 8)
filter(diamonds, carat > 3)




  