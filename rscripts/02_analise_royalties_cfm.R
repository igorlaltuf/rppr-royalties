# Análise dos royalties CFM 2008 - 2018 na AMZL

# 1 - Carregar pacotes, importar dados e declarar variáveis iniciais -------------------------
rm(list=ls()) 
source('Rscripts/00_bibliotecas.R')
options(scipen = 999) # remove notação científica
royalties_cfm <- read_csv2('temp/royalties_mineracao_2008_2018_amzl.csv')

# 2 - Analisar Royalties da Mineração --------------------------------------------------------

# Quantidade de municípios na amzl: 427
length(unique(royalties_cfm$codigo_ibge))

# Qtd de anos que receberam royalties
anos_cfm <- royalties_cfm %>% 
  group_by(codigo_ibge) %>% 
  count()

# Análise global sobre o tempo que os municípios receberam royalties
anos_cfm_muni <- anos_cfm %>% 
  group_by(n) %>% 
  count()
# dos 427 municípios, 201 recebem royalties CFM há pelo menos 8 anos.


# Municípios que mais receberam royalties CFEM
cfm_muni <- royalties_cfm %>% 
  group_by(codigo_ibge, municipio) %>% 
  summarise(valor_acumulado_defl_2020 = sum(royalt_cfm_real)) %>% 
  arrange(desc(valor_acumulado_defl_2020)) %>% 
  left_join(anos_cfm) %>% 
  mutate(media_anual_defl_2020 = round(valor_acumulado_defl_2020/n, 2)) %>% 
  rename('anos_recebendo_royalties' = 'n')

# 75% dos municípios que receberam royalties CFEM na AMZL receberam em média ménos de 53.004 reais por ano no período estudado.
summary(cfm_muni$media_anual_defl_2020)
# apenas 20 municípios receberam em média acima de 1 milhão de reais por ano em valores de 2020.


# Municípios que mais receberam royalties CFEM
cfm_muni_20 <- cfm_muni %>% 
  dplyr::filter(valor_acumulado_defl_2020 >= 9479993.86) %>% # selecionar as 20 primeiras tendo agrupado
  select(1,2,4,3,5)

# 3 - Exportar dados em csv ------------------------------------------------------------------------

# Exportar em csv e fazer tabela no Excel.
write.csv2(x = cfm_muni_20, fileEncoding = 'UTF-8', row.names = F, file = 'output/cfm_20_muni.csv')


# 4 - Mapa com a localização desses municípios ---------------------------------------------------

br <- geobr::read_state() %>% 
  dplyr::filter(abbrev_state %in% c("PA","AP","AM","MT","RO","MA","TO","RR","AC"))
munis <- geobr::read_municipal_seat()
dados <- left_join(cfm_muni, munis, by = c('codigo_ibge' = 'code_muni'))
dados_20 <- left_join(cfm_muni_20, munis, by = c('codigo_ibge' = 'code_muni'))
  
ggplot() +
  geom_sf(data = br, aes(geometry = geom), fill = NA) +
  geom_sf(data = dados, aes(geometry = geom, col = 'Municípios que receberam royalties'), stat = "sf_coordinates", 
          size = 1, show.legend = 'point') +
  geom_sf(data = dados_20, aes(geometry = geom, col = '20 municípios com maiores valores'), stat = "sf_coordinates", 
          size = 1, show.legend = 'point') +
  scale_color_manual(values = c("Municípios que receberam royalties" = alpha('black', 1),
                                "20 municípios com maiores valores" = alpha('#ffbf00', 1)),
                     name = NULL,
                     guide = guide_legend(override.aes = list(linetype= "solid", #blank e na
                                                              shape= 16))) +
  labs(x = NULL, y = NULL) + #Muda o nome da legenda com o fill.
  coord_sf(crs = 4674) +
  annotation_scale(location = 'br')+
  annotation_north_arrow(location = 'tl', 
                         style = north_arrow_fancy_orienteering()) +
  theme_classic() + # retira o grid e coloca o fundo branco
  theme(legend.position = 'bottom',
        plot.title = element_text(hjust = 0.5)) 


ggsave('output/mapa_cfm.png', dpi = 600, width = 9, height = 6, scale = .8)
# Colorir os 20 maiores no mapa acima


