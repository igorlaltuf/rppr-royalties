rm(list=ls()) # limpar as variáveis carregadas
# REGIC Mapas estáticos
source('rscripts/00_funcoes_globais.R')
source('rscripts/00_bibliotecas.R')

options(scipen=999)
sf_use_s2(F) # permite que o sf fucione como na versão anterior

# Baixar mapa estados
shape.estados <- read_state(code_state = 'all')

# Importar shapefiles da REGIC
ligacoes.regic <- st_read("Input/REGIC2018_ligacoes_cidades/REGIC2018_Ligacoes_entre_Cidades.shp", options = "ENCODING=UTF-8")
cidades.ponto <- st_read("Input/REGIC2018_cidades_ponto/REGIC2018_Cidades_ponto.shp", options = "ENCODING=UTF-8")

amzl <- read_amazon()
uf.amzl <- c('RO','AM','AC','RO','RR','PA','MA','AP','TO','MT')

########## ligações de saúde de alta complexidade
ligacoes.s <- ligacoes.regic %>% 
  dplyr::filter(cod_dest %in% '1100205',
                quest_4 > 0)

pontos <- cidades.ponto %>% 
  dplyr::filter(uf %in% uf.amzl,
                cod_cidade %in% c(ligacoes.s$cod_ori)) %>% 
  mutate(lat = unlist(map(geometry, 1)),
         lng = unlist(map(geometry, 2)))

shape.selec <- st_intersection(amzl, shape.estados)

shape.estados <- shape.estados %>% 
  dplyr::filter(abbrev_state %in% uf.amzl)


ggplot() +
  geom_sf(data = shape.estados) +
  geom_sf(data = ligacoes.s, size = .2, aes(colour = 'Ligações')) +
  geom_sf(data = pontos, size = .1) +
  coord_sf(xlim = c(-67.5, -58), ylim = c(-14, -6), expand = FALSE) +
  geom_text(
    data = pontos,
    label = pontos$nome_cidad, 
    aes(x = lat, y = lng), # separar coordenadas em duas colunas com lat e long
    check_overlap = T,
    size = 2.5, 
    fontface = "bold") +
  scale_color_manual(values = c('Ligações' = '#2c7fb8'), name = NULL,
                     guide = guide_legend(override.aes = list(linetype=c("solid")))) +
  theme(legend.position = "bottom") +
  labs(y = NULL, x = NULL)
  
ggsave('output/regic_pv_saude.png', width = 9, height = 6, dpi = 600)

