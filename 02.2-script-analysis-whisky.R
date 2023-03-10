
library(tidyverse)
library(plotly)
library(viridis)

glimpse(DF3)

# Generando Gráficas ----

## Marca que más compra

ggplotly(
  
  DF3 %>%
    # Contar los casos
    count(`¿Cuál es la marca que más compra?`) %>% 
    # Ordenar la tabla
    arrange(desc(n)) %>% 
    # Calcular porcentajes
    mutate(Porcentaje = round(n/sum(n),
                              digits = 2)) %>% 
    
    # Crear un gráfico de columnas
    ggplot(mapping = aes(x = `¿Cuál es la marca que más compra?`,
                         y = Porcentaje,
                         fill = `¿Cuál es la marca que más compra?`,
                         label = n)) +
    geom_col() +
    scale_y_continuous(labels = scales::percent) +
    theme(legend.position = "none",
          text = element_text(size = 9))
  
)



## Prueba

DF3 %>% 
  pivot_longer(cols = starts_with("Prueba"),
               names_to = "Variable",
               values_to = "Prueba") %>%
  select(Prueba) %>% 
  na.omit() %>% 
  count(Prueba) %>% 
  
  # Al tratarse de respuesta múltiple, se divide entre nrow()
  mutate(Porcentaje = n/nrow(DF3),
         # Modifica el formato a porcentaje
         Porcentaje2 = scales::percent(Porcentaje)) %>% 
  
  ggplot(mapping = aes(x = Prueba,
                       y = Porcentaje,
                       fill = Prueba,
                       label = Porcentaje2)) +
  geom_col() +
  
  # Para girar el gráfico
  coord_flip() +
  
  # Para agregar texto al gráfico
  geom_label() +
  
  scale_y_continuous(labels = scales::percent) +
  
  theme(legend.position = "none")



## Conocimiento

DF3 %>% 
  select(`Marca temporal`,
         starts_with("Atributo")) %>% 
  pivot_longer(cols = starts_with("Atributo"),
               names_to = "var",
               values_to = "atributo") %>% 
  drop_na(atributo) %>%
  count(atributo) %>% 
  mutate(Porcentaje = n/nrow(DF3),
         Porcentaje2 = scales::percent(Porcentaje)) %>% 
  
  ggplot(mapping = aes(x = atributo,
                       y = Porcentaje,
                       fill = atributo,
                       label = Porcentaje2)) +
  geom_col() +
  geom_label() +
  scale_y_continuous(labels = scales::percent) +
  coord_flip() +
  scale_fill_viridis_d() +
  theme(legend.position = "none")



# Tablas cruzadas ----

DF4 <- DF3 %>% 
  select(`Precio razonable`:`Variedad de maduración (años)`) %>% 
  pivot_longer(cols = everything(),
               names_to = "atributo",
               values_to = "marca")


## Tabla de casos ----
table(DF4)


## Tabla de porcentajes ----
prop.table(table(DF4),
           margin = 1)

prop.table(table(DF4),
           margin = 2)



## Análisis de correspondencias (ANACOR) ----

library(FactoMineR)
FactoMineR::CA(table(DF4))
