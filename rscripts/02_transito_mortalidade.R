
# 1 Óbitos a cada 100 mil habitantes em acidentes de trânsito ----------------------------------------

rm(list=ls()) 

source('Rscripts/00_bibliotecas.R')

options(scipen = 999) # remove notação científica



# Altamira 
# estimativa da população em 2019
pop <- readr::read_csv2('input/pop_muni_2000-2020.csv') %>%
  mutate(cod_muni = substr(cod_muni, 1, 6),
         ano = as.character(ano)) %>% 
  dplyr::filter(cod_muni %in% 150060)

dados <- microdatasus::fetch_datasus(year_start = 2006, year_end = 2020, uf = "PA", information_system = "SIM-DO")

dados <- dados %>%
  janitor::clean_names() %>%
  dplyr::filter(codmunocor %in% 150060) %>%
  filter(str_detect(causabas, "^V"))

obito_ano <- dados %>%
  mutate(ano_obito = substr(dtobito, 5, 8)) %>%
  group_by(ano_obito) %>%
  count() %>%
  left_join(pop, by = c('ano_obito' = 'ano')) %>%
  mutate(obitos_100_mil_hab = (n / populacao) * 100000) %>%
  mutate(obitos_100_mil_hab = round(obitos_100_mil_hab, 0))

# evolução da taxa de mortalidade em teresópolis (mortalidade no transito a cada 100 mil habitantes)
mort_transito <- ggplot(obito_ano) +
  aes(x = ano_obito, weight = obitos_100_mil_hab) +
  geom_bar(fill = "grey") +
  labs(x = "Ano",
       y = "Taxa de Mortalidade por Acidentes de Transporte\na cada 100 mil habitantes") +
  theme_classic()

mort_transito

ggsave(plot = mort_transito, dpi = 600, 'output/transito/altamira_tx_mortalidade.png')




# Porto Velho
# estimativa da população em 2019
pop <- readr::read_csv2('input/pop_muni_2000-2020.csv') %>%
  mutate(cod_muni = substr(cod_muni, 1, 6),
         ano = as.character(ano)) %>% 
  dplyr::filter(cod_muni %in% 110020)

dados <- microdatasus::fetch_datasus(year_start = 2006, year_end = 2020, uf = "RO", information_system = "SIM-DO")

dados <- dados %>%
  janitor::clean_names() %>%
  dplyr::filter(codmunocor %in% 110020) %>%
  filter(str_detect(causabas, "^V"))

obito_ano <- dados %>%
  mutate(ano_obito = substr(dtobito, 5, 8)) %>%
  group_by(ano_obito) %>%
  count() %>%
  left_join(pop, by = c('ano_obito' = 'ano')) %>%
  mutate(obitos_100_mil_hab = (n / populacao) * 100000) %>%
  mutate(obitos_100_mil_hab = round(obitos_100_mil_hab, 0))

# evolução da taxa de mortalidade em teresópolis (mortalidade no transito a cada 100 mil habitantes)
mort_transito <- ggplot(obito_ano) +
  aes(x = ano_obito, weight = obitos_100_mil_hab) +
  geom_bar(fill = "grey") +
  labs(x = "Ano",
       y = "Taxa de Mortalidade por Acidentes de Transporte\na cada 100 mil habitantes") +
  theme_classic()

mort_transito

ggsave(plot = mort_transito, dpi = 600, 'output/transito/porto_velho_tx_mortalidade.png')



# 2 Taxa de motorização (automóveis a cada 100 mil habitantes) ------------------------------------

motorizacao <- readr::read_csv2('input/motorizacao_muni_2003-2020.csv') 
# Altamira
taxa_altamira <- motorizacao %>% 
  filter(cod_muni %in% 1500602)

motori_altamira <- ggplot(taxa_altamira) +
  aes(x = ano, weight = taxa_motorizacao) +
  geom_bar(fill = "grey") +
  labs(x = "Ano",
       y = "Taxa de Motorização") +
  theme_classic() +
  scale_x_continuous(breaks = c(2003:2020))

motori_altamira

ggsave(plot = motori_altamira, dpi = 600, 'output/transito/altamira_motorizacao.png')

# Porto Velho
taxa_porto_velho <- motorizacao %>% 
  filter(cod_muni %in% 1100205) 
  
motori_porto_velho <- ggplot(taxa_porto_velho) +
  aes(x = ano, weight = taxa_motorizacao) +
  geom_bar(fill = "grey") +
  labs(x = "Ano",
       y = "Taxa de Motorização") +
  theme_classic() +
  scale_x_continuous(breaks = c(2003:2020))

motori_porto_velho

ggsave(plot = motori_porto_velho, dpi = 600, 'output/transito/porto_velho_motorizacao.png')
