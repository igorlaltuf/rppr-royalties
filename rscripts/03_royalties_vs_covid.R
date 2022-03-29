# Tabela COVID e Royalties

# 1 - Carregar pacotes, importar dados e declarar variáveis iniciais -------------------------
rm(list=ls()) 
source('Rscripts/00_bibliotecas.R')
source('Rscripts/00_funcoes_globais.R')
options(scipen = 999) # remove notação científica

# dados covid 27-03-2022
covid <- read.csv('input/caso_full.csv', fileEncoding = 'UTF-8')

# dados royalties 2008-2018
dados_cfh <- read_csv2(file = 'output/cfh_20_muni.csv')
dados_cfm <- read_csv2(file = 'output/cfm_20_muni.csv')

# 2 - Limpar e organizar dados da COVID-19 ---------------------------------------------------
covid <- covid %>% 
  dplyr::select(city_ibge_code,city,place_type,state,date,last_available_deaths,estimated_population) %>% 
  dplyr::filter(place_type == 'city' &
                  date == '2021-08-12') %>% 
  mutate(obitos_100_mil_ha = (last_available_deaths/estimated_population)*100000) %>% 
  na.omit()

covid <- classificar.variavel(covid,'obitos_100_mil_ha','class_obit_100_mil_ha')

# 3 - Unir as bases --------------------------------------------------------------------------

# 3.1 - CFM
cfm_covid <- dados_cfm %>% 
  left_join(covid, by = c('codigo_ibge' = 'city_ibge_code')) %>% 
  select('municipio','state','anos_recebendo_royalties','valor_acumulado_defl_2020',
         'media_anual_defl_2020','obitos_100_mil_ha','class_obit_100_mil_ha')

# por categoria covid: 12 municípios acima da média. 9 municípios dos 20 foram classificados como alto ou muito alto.
cat_cfm <- cfm_covid %>% 
  group_by(class_obit_100_mil_ha) %>% 
  count()

write.csv2(x = cfm_covid, row.names = F, file = 'output/cfm_20_covid.csv')

# 3.2 - CFH
cfh_covid <- dados_cfh %>% 
  left_join(covid, by = c('codigo_ibge' = 'city_ibge_code')) %>% 
  select('municipio','state','anos_recebendo_royalties','valor_acumulado_defl_2020',
         'media_anual_defl_2020','obitos_100_mil_ha','class_obit_100_mil_ha')

# por categoria covid: 9 municípios acima da média. 5 municípios dos 20 foram classificados como alto ou muito alto.
cat_cfh <- cfh_covid %>% 
  group_by(class_obit_100_mil_ha) %>% 
  count()

write.csv2(x = cfh_covid, row.names = F, file = 'output/cfh_20_covid.csv')
