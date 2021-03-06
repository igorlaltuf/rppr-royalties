# An�lise dos royalties CFH 2008 - 2018 na AMZL

# 1 - Carregar pacotes, importar dados e declarar vari�veis iniciais -------------------------
rm(list=ls()) 
source('Rscripts/00_bibliotecas.R')
options(scipen = 999) # remove nota��o cient�fica
royalties_cfh <- read_csv2('temp/royalties_energia_2008_2018_amzl.csv')

# 2 - Analisar Royalties de Energia ----------------------------------------------------------

# Quantidade de munic�pios na amzl: 71
length(unique(royalties_cfh$codigo_ibge))

# Qtd de anos que receberam royalties
anos_cfh <- royalties_cfh %>% 
  group_by(codigo_ibge) %>% 
  count()

# An�lise global sobre o tempo que os munic�pios receberam royalties
anos_cfh_muni <- anos_cfh %>% 
  group_by(n) %>% 
  count()
# dos 71 munic�pios, 53 recebem royalties cfh h� pelo menos 8 anos.


# Munic�pios que mais receberam royalties CFEM
cfh_muni <- royalties_cfh %>% 
  group_by(codigo_ibge, municipio) %>% 
  summarise(valor_acumulado_defl_2020 = sum(royalt_cfh_real)) %>% 
  arrange(desc(valor_acumulado_defl_2020)) %>% 
  left_join(anos_cfh) %>% 
  mutate(media_anual_defl_2020 = round(valor_acumulado_defl_2020/n, 2)) %>% 
  rename('anos_recebendo_royalties' = 'n')

# 75% dos munic�pios que receberam royalties CFEM na AMZL receberam em m�dia menos de 2.830.978 reais por ano no per�odo estudado.
summary(cfh_muni$media_anual_defl_2020)
# apenas 20 munic�pios receberam em m�dia acima de 1 milh�o de reais por ano em valores de 2020.


# Munic�pios que mais receberam royalties CFEM
cfh_muni_20 <- cfh_muni %>% 
  dplyr::filter(valor_acumulado_defl_2020 >= 20291978.50) %>% # selecionar as 20 primeiras tendo agrupado
  select(1,2,4,3,5)

# 3 - Exportar dados em csv ------------------------------------------------------------------------

# Exportar em csv e fazer tabela no Excel.
write.csv2(x = cfh_muni_20, fileEncoding = 'UTF-8', row.names = F, file = 'output/cfh_20_muni.csv')


# 4 - Mapa com a localiza��o desses munic�pios ---------------------------------------------------

br <- geobr::read_state() %>% 
  dplyr::filter(abbrev_state %in% c("PA","AP","AM","MT","RO","MA","TO","RR","AC"))
munis <- geobr::read_municipal_seat()
dados <- left_join(cfh_muni, munis, by = c('codigo_ibge' = 'code_muni'))
dados_20 <- left_join(cfh_muni_20, munis, by = c('codigo_ibge' = 'code_muni'))

ggplot() +
  geom_sf(data = br, aes(geometry = geom), fill = NA) +
  geom_sf(data = dados, aes(geometry = geom, col = 'Munic�pios que receberam royalties'), stat = "sf_coordinates", 
          size = 1, show.legend = 'point') +
  geom_sf(data = dados_20, aes(geometry = geom, col = '20 munic�pios com maiores valores'), stat = "sf_coordinates", 
          size = 1, show.legend = 'point') +
  scale_color_manual(values = c("Munic�pios que receberam royalties" = alpha('black', 1),
                                "20 munic�pios com maiores valores" = alpha('#ffbf00', 1)),
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


ggsave('output/mapa_cfh.png', dpi = 600, width = 9, height = 6, scale = .8)
# Colorir os 20 maiores no mapa acima

