# Dados de Educação - Censo Escolar - INEP

rm(list=ls()) 
# carregar scripts
source('Rscripts/00_bibliotecas.R')
source('Rscripts/00_funcoes_globais.R')
muni <- read_xlsx('input/municipios_brasil.xlsx')

cfm <- read_csv2('output/cfm_20_muni.csv')
cfm <- cfm$codigo_ibge

cfh <- read_csv2('output/cfh_20_muni.csv')
cfh <- cfh$codigo_ibge

dados_escolas <- read.csv('input/01_escolas_publicas_AMZL_2009_2020.csv')

# 1 - Dados de escolas públicas e matrículas em escolas públicas a cada 100 mil habitantes

# CFM 

cfm_2009 <- dados_escolas %>% 
  dplyr::filter(ano %in% 2009,
                cod_muni %in% cfm) %>% 
  rename("qtd_matriculas_2009" = qtd_matriculas,
         "qtd_escolas_2009" = qtd_escolas,
         "populacao_2009" = populacao,
         "matr_100_mil_2009" = matr_100_mil,
         "escol_100_mil_2009" = escol_100_mil) %>% 
  select(-ano)

cfm_2018 <- dados_escolas %>% 
  dplyr::filter(ano %in% 2018,
                cod_muni %in% cfm) %>% 
  rename("qtd_matriculas_2018" = qtd_matriculas,
         "qtd_escolas_2018" = qtd_escolas,
         "populacao_2018" = populacao,
         "matr_100_mil_2018" = matr_100_mil,
         "escol_100_mil_2018" = escol_100_mil) %>% 
  select(-ano)

cfm_educacao <- left_join(cfm_2009,cfm_2018) %>% 
  select(-'sigla_uf',-'cod_muni',-'populacao_2009',-'populacao_2018') %>% 
  select(1,2,6,4,8,3,6,7,5,9) %>% 
  mutate_if(is.numeric, ~round(., 0)) %>% 
  mutate(saldo_matr_100_mil_hab = matr_100_mil_2018 - matr_100_mil_2009,
         saldo_escola_100_mil_hab = escol_100_mil_2018 - escol_100_mil_2009)
  


# CFH

cfh_2009 <- dados_escolas %>% 
  dplyr::filter(ano %in% 2009,
                cod_muni %in% cfh) %>% 
  rename("qtd_matriculas_2009" = qtd_matriculas,
         "qtd_escolas_2009" = qtd_escolas,
         "populacao_2009" = populacao,
         "matr_100_mil_2009" = matr_100_mil,
         "escol_100_mil_2009" = escol_100_mil) %>% 
  select(-ano)

cfh_2018 <- dados_escolas %>% 
  dplyr::filter(ano %in% 2018,
                cod_muni %in% cfh) %>% 
  rename("qtd_matriculas_2018" = qtd_matriculas,
         "qtd_escolas_2018" = qtd_escolas,
         "populacao_2018" = populacao,
         "matr_100_mil_2018" = matr_100_mil,
         "escol_100_mil_2018" = escol_100_mil) %>% 
  select(-ano)


# tabelas síntese
cfh_educacao <- left_join(cfh_2009,cfh_2018) %>% 
  select(-'sigla_uf') %>% 
  mutate_if(is.numeric, ~round(., 0)) %>% 
  mutate(saldo_matr_100_mil_hab = matr_100_mil_2018 - matr_100_mil_2009,
         saldo_escola_100_mil_hab = escol_100_mil_2018 - escol_100_mil_2009)

cfm_educacao <- left_join(cfm_2009, cfm_2018) %>% 
  select(-'sigla_uf') %>% 
  mutate_if(is.numeric, ~round(., 0)) %>% 
  mutate(saldo_matr_100_mil_hab = matr_100_mil_2018 - matr_100_mil_2009,
         saldo_escola_100_mil_hab = escol_100_mil_2018 - escol_100_mil_2009)

# 2 - Proporção de docentes por alunos 

# Query feita pelo base dos dados no script do projeto FAO.
docentes_cfh <- read.csv('input/00_base_docentes_cfh_2009_2020.csv') %>% 
  group_by(id_municipio, ano) %>% 
  count(id_municipio) %>% 
  dplyr::filter(ano %in% c(2009, 2018)) %>% 
  pivot_wider(names_from = 'ano', values_from = 'n') %>% 
  rename('docentes_2009' = '2009',
         'docentes_2018' = '2018') 


docentes_cfm <- read.csv('input/00_base_docentes_cfm_2009_2020.csv') %>% 
  group_by(id_municipio, ano) %>% 
  count(id_municipio) %>% 
  dplyr::filter(ano %in% c(2009, 2018)) %>% 
  pivot_wider(names_from = 'ano', values_from = 'n') %>% 
  rename('docentes_2009' = '2009',
         'docentes_2018' = '2018')


# CONTINUAR DAQUI:
sintese_cfh <- left_join(cfh_educacao, docentes_cfh, by = c('cod_muni' = 'id_municipio')) %>% 
  mutate(docentes_100_mil_hab_2009 = (docentes_2009 / populacao_2009) * 100000,
         docentes_100_mil_hab_2018 = (docentes_2018 / populacao_2018) * 100000,
         saldo_docentes_100_mil_hab = docentes_100_mil_hab_2018 - docentes_100_mil_hab_2009)

sintese_cfm <- left_join(cfm_educacao, docentes_cfm, by = c('cod_muni' = 'id_municipio')) %>% 
  mutate(docentes_100_mil_hab_2009 = (docentes_2009 / populacao_2009) * 100000,
         docentes_100_mil_hab_2018 = (docentes_2018 / populacao_2018) * 100000,
         saldo_docentes_100_mil_hab = docentes_100_mil_hab_2018 - docentes_100_mil_hab_2009)

# Exportar
write.csv2(sintese_cfh, 'output/síntese_educacao_cfh.csv', row.names = F)
write.csv2(sintese_cfm, 'output/síntese_educacao_cfm.csv', row.names = F)

















