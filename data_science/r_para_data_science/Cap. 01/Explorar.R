# Exercícios

library(tidyverse)

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy))

ggplot(data = mpg) # Uma mensagem de erro

?mpg # pedindo ajuda/informação

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = cyl, y = hwy))

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = class, y = drv))

# MAPEAMENTO ESTÉTICOS - página 7

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, color = class))

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, size = class))

# Top - alpha controla a transparência dos pontos
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, alpha = class))

# Botton 
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, shape = class))

# para manipular manualmente, a variável deve estar fora de aes 
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy), color = "blue")

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy), shape = 8) # de 0 até 20


# FACETAS - página 14

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) + 
  facet_wrap(~class, nrow = 2)

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) + 
  facet_grid(drv ~ cyl)

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) + 
  facet_grid(. ~ cyl)

# OBJETOS GEOMÉTRICOS - página 16

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) # geom de pontos

ggplot(data = mpg) + 
  geom_smooth(mapping = aes(x = displ, y = hwy)) # geom de smooth
  
ggplot(data = mpg) + 
  geom_smooth(mapping = aes(x = displ, y = hwy, linetype = drv))


ggplot(data = mpg) + 
  geom_smooth(mapping = aes(x = displ, y = hwy, group = drv))

ggplot(data = mpg) + 
  geom_smooth(
    mapping = aes(x = displ, y = hwy, linetype = drv),
    show.legend = FALSE
    )

ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy)) + 
  geom_smooth(mapping = aes(x = displ, y = hwy))

ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) +
  geom_point() + 
  geom_smooth()

ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) +
  geom_point(mapping = aes(color = class)) + 
  geom_smooth()

ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) +
  geom_point(mapping = aes(color = class)) + 
  geom_smooth(
    data = filter(mpg, class == "subcompact"),
    se = FALSE
  )

ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) +
  geom_point(mapping = aes(color = class)) + 
  geom_smooth(
    data = filter(mpg, class == "compact"),
    se = FALSE
  )

# TRANSFORMAÇÕES ESTATÍSTICAS - página 22

ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut))

?geom_bar

ggplot(data = diamonds) +
  stat_count(mapping = aes(x = cut))

###############

demo <- tribble(
  ~a, ~b,
  "bar_1", 20,
  "bar_2", 30,
  "bar_3", 40,
)

ggplot(data = demo) +
  geom_bar(
    mapping = aes(x = a, y = b), stat = "identity"
  )

## 
ggplot(data = diamonds) + 
  geom_bar(
    mapping = aes(x = cut, y = ..prop.., group = 1)
  )

##
ggplot(data = diamonds) + 
  stat_summary(
    mapping = aes(x = cut, y = depth),
    fun.min = min,
    fun.max = max,
    fun = median
  )

# AJUSTE DE POSIÇÃO

# podemos colorir um gráfico de barras

ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, color = cut))
ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, fill = cut))


ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, fill = clarity))

# Posição identity
ggplot(
  data = diamonds,
  mapping = aes(x = cut, fill = clarity)
) +
  geom_bar(alpha = 1/5, position = "identity")
ggplot(
  data = diamonds,
  mapping = aes(x = cut, color = clarity)
) + 
  geom_bar(fill = NA, position = "identity")

# position fill
ggplot(data = diamonds) +
  geom_bar(
    mapping = aes(x = cut, color = clarity),
    position = "fill"
) 

# position = dodge
ggplot(data = diamonds) +
  geom_bar(
    mapping = aes(x = cut, color = clarity),
    position = "dodge"
  )

# position jitter
ggplot(data = mpg) +
  geom_point(
    mapping = aes(x = displ, y = hwy),
    position = "jitter"
  )

# SISTEMAS DE COORDENADAS (página 31)

ggplot(data = mpg, mapping = aes(x = class, y = hwy)) +
  geom_boxplot()

ggplot(data = mpg, mapping = aes(x = class, y = hwy)) + 
  geom_boxplot() +
  coord_flip()

nz <- map_data("nz")
ggplot(nz, aes(long, lat, group = group)) +
  geom_polygon(fill = "white", color = "black")


bar <- ggplot(data = diamonds) +
  geom_bar(
    mapping = aes(x = cut, fill = cut),
    show.legend = FALSE,
    width = 1
  ) +
  theme(aspect.ratio = 1) +
  labs(x = NULL, y = NULL)

bar + coord_flip()
bar + coord_polar()