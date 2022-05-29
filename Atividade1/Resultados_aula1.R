library(tidyverse)
library(knitr)
library(dplyr)
data.frame(
  participante = seq(1,1),
  fichas = c(read.csv("C:/Users/letic/Documents/Pasta de atividades/Mestrado/Ciência colaborativa/atividade1_LETICIA-EVANGELISTA.csv", header = T) %>% 
               distinct(amostra) %>% 
               pull()%>% 
               sample()),
  n = 'amostra'
) %>% 
  pivot_wider(
    names_from = "n", 
    values_from = "fichas"
  ) %>% 
  knitr::kable()

