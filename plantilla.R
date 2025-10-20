## #00 ****{.tabset}

### **Foto**

![foto de ](fotos\.png){width=500}

### **Descripcion de la pokedex**



### **Origen**

- 
- 
  
### **Caracteristicas**

```{r, echo = FALSE}
caracteristicas_ <- Kanto %>%
  select(`Tipo 1`,
         `Tipo 2`,
         `Proporcion de genero`,
         `Color`,
         `Forma`,
         `Altura (M)`,
         `Peso (kg)`,
         `Amistad base`) %>%
  slice() %>%
  knitr::kable(caption = "Caracteristicas de ") %>%
  kable_styling() %>%
  row_spec(0, background = "#4682B4", color = "black") %>%
  row_spec(1, background = "white")


caracteristicas_
```

### **Resistencias y debilidades**



### **Etapa evolutiva**



### **Habilidades**



### **Metodo de captura**



### **Estadisticas**

```{r, echo = FALSE}
estadisticas_ <- Kanto %>% 
  slice() %>% 
  select(`Hp base`,
         `Ataque base`,
         `Defensa base`,
         `Ataque especial base`,
         `Defensa especial base`,
         `Velocidad base`,
         `Total base stats`)

estadisticas_largas <- estadisticas_ %>%
  pivot_longer(cols = everything(), 
               names_to = "Estadistica", 
               values_to = "Valor")

_estadisticas <- ggplot(estadisticas_largas, 
                                 aes(x = Valor,
                                     y = reorder(Estadistica, Valor),
                                     fill = Estadistica)) +
  geom_bar(stat = "identity", width = 0.5) +  
  labs(title = "EstadC-sticas Base de ",
       x = "Puntos",
       y = NULL) +
  theme_minimal() +
  theme(
    axis.text.y = element_text(angle = 45, hjust = 1),
    legend.position = "") +
  scale_x_continuous(expand = c(0, 0))

_estadisticas

```