# Dados de Saúde Pública - DataSUS de 2008 até 2018
# 1 - Carregar scripts, variáveis e bibliotecas ---------------------------------------------------------------------
rm(list=ls()) 
source('Rscripts/00_bibliotecas.R')
source('Rscripts/00_funcoes_globais.R')

# Estados da AMZL
uf.amz.legal <- c('AC','RR','AM','RO','PA','AP','MA','TO','MT')

cidades.brasil.nome <- read_excel(path = 'input/municipios_brasil.xlsx') %>% 
  mutate(cod_muni_6_dig = substr(cod_muni, 1, 6))

# Dados populacionais
pop <- read_csv2('input/pop_muni_2000-2020.csv') %>% # População - 2001 - 2020
  mutate(cod_muni_6_dig = substr(cod_muni, 1, 6))

# 2 - Baixar e analisar dados dos estabelecimentos ------------------------------------------------------------------
# Baixar e limpar os dados
var.cnes.st <- c('CNES','CODUFMUN','COD_CEP','VINC_SUS','TPGESTAO','TP_UNID','TURNO_AT','GESPRG1E', 
                 'GESPRG1M','GESPRG2E','GESPRG2M','GESPRG4E','GESPRG4M','GESPRG5E','GESPRG5M', 
                 'GESPRG6E','GESPRG6M','NIVATE_A','NIVATE_H','URGEMERG')
ano <- 2008 # ano inicial
mes <- 12 # mês em que os dados serão coletados
while(ano <= 2018){
  dados <- fetch_datasus(year_start = ano ,
                         year_end = ano, 
                         month_start = mes, 
                         month_end = mes, 
                         uf = uf.amz.legal, 
                         information_system = "CNES-ST", 
                         vars = var.cnes.st) %>% 
    mutate(ano = ano)
  assign(paste("estabelec", ano, sep = "."), dados)
  ano = ano + 1
}

dados.st <- do.call(rbind, lapply(paste0("estabelec.", 2008:2018), get)) %>% 
  janitor::clean_names()


# 3 - Estabelecimentos: análise dos 20 maiores municípios CFM e CFH ---------------------------------------------------------------
cfh <- read_csv2('output/cfh_20_muni.csv')
cfh$codigo_ibge <- substr(cfh$codigo_ibge, 1, 6)
cfh_muni_20 <- cfh$codigo_ibge

cfm <- read_csv2('output/cfm_20_muni.csv')
cfm$codigo_ibge <- substr(cfm$codigo_ibge, 1, 6)
cfm_muni_20 <- cfm$codigo_ibge


dados_cfh <- dados.st %>% 
  dplyr::filter(nivate_h == 1, # sobre o atendimento: Tem atendimento hospitalar municipal ou estadual? exclui farmácias etc
                tpgestao %in% c('M','E','D'), # M, E ou D (dupla) administração municipal, estadual ou dupla
                vinc_sus == 1) %>%  # vinculados ao SUS
  group_by(ano, codufmun) %>% 
  count() %>% 
  mutate(codufmun = as.character(codufmun)) %>% 
  dplyr::filter(codufmun %in% cfh_muni_20, # filtra CFH
                ano %in% c(2008, 2018)) %>% 
  pivot_wider(names_from = ano, values_from = n) %>% 
  replace(is.na(.), 0) %>% 
  rename(ano_2018 = '2018',
         ano_2008 = '2008') %>% 
  mutate(saldo = ano_2018 - ano_2008) %>% 
  left_join(cidades.brasil.nome, by = c('codufmun' = 'cod_muni_6_dig')) %>% 
  select(muni, codufmun, ano_2008, ano_2018, saldo)



dados_cfm <- dados.st %>% 
  dplyr::filter(nivate_h == 1, # sobre o atendimento: Tem atendimento hospitalar municipal ou estadual? exclui farmácias etc
                tpgestao %in% c('M','E','D'), # M, E ou D (dupla) administração municipal, estadual ou dupla
                vinc_sus == 1) %>%  # vinculados ao SUS
  group_by(ano, codufmun) %>% 
  count() %>% 
  mutate(codufmun = as.character(codufmun)) %>% 
  dplyr::filter(codufmun %in% cfm_muni_20, # filtra CFM
                ano %in% c(2008, 2018)) %>% 
  pivot_wider(names_from = ano, values_from = n) %>% 
  replace(is.na(.), 0) %>% 
  rename(ano_2018 = '2018',
         ano_2008 = '2008') %>% 
  mutate(saldo = ano_2018 - ano_2008) %>% 
  left_join(cidades.brasil.nome, by = c('codufmun' = 'cod_muni_6_dig')) %>% 
  select(muni, codufmun, ano_2008, ano_2018, saldo)


# 4 - Leitos: análise dos 20 maiores municípios CFM e CFH ---------------------------------------------------------------
# valores para todos os leitos, e não apenas os leitos de internação

var.cnes.lt <- c('CNES','CODUFMUN','TP_UNID','TP_LEITO','CODLEITO','QT_EXIST','QT_CONTR','QT_SUS','QT_NSUS')
ano <- 2008 # ano inicial
mes <- 12 # mês em que os dados serão coletados
while(ano <= 2018){
  dados <- fetch_datasus(year_start = ano ,
                         year_end = ano,
                         month_start = mes,
                         month_end = mes,
                         uf = uf.amz.legal,
                         information_system = "CNES-LT",
                         vars = var.cnes.lt) %>%
    mutate(ano = ano)
  assign(paste("leito", ano, sep="."), dados)
  ano = ano + 1
}

dados.lt <- do.call(rbind, lapply(paste0("leito.", 2008:2018),get)) %>%
  janitor::clean_names() %>%
  mutate(codufmun = as.numeric(as.character(codufmun)))

dados.lt <- dados.lt %>% 
  mutate(codufmun = as.character(codufmun)) %>% 
  left_join(pop, by = c('codufmun' = 'cod_muni_6_dig', 'ano')) %>%  
  select('ano','codufmun','qt_sus','populacao') %>% 
  group_by(ano,codufmun,populacao) %>%   
  summarise(leitos_sus = sum(qt_sus)) %>% 
  mutate(leitos_cada_100_mil_ha = round((leitos_sus/populacao)*100000,0))

# Saldo Leitos a cada 100 mil hab CFM

leitos_cfm <- dados.lt %>% 
  dplyr::filter(codufmun %in% cfm_muni_20, # filtra CFM
                ano %in% c(2008, 2018)) %>% 
  select(ano, codufmun, leitos_cada_100_mil_ha) %>% 
  pivot_wider(names_from = ano, values_from = leitos_cada_100_mil_ha) %>% 
  replace(is.na(.), 0) %>% 
  rename(ano_2018 = '2018',
         ano_2008 = '2008') %>% 
  mutate(saldo = ano_2018 - ano_2008) %>% 
  left_join(cidades.brasil.nome, by = c('codufmun' = 'cod_muni_6_dig')) %>% 
  select(muni, codufmun, ano_2008, ano_2018, saldo)
  

# Saldo Leitos a cada 100 mil hab CFH

leitos_cfh <- dados.lt %>% 
  dplyr::filter(codufmun %in% cfh_muni_20, # filtra CFH
                ano %in% c(2008, 2018)) %>% 
  select(ano, codufmun, leitos_cada_100_mil_ha) %>% 
  pivot_wider(names_from = ano, values_from = leitos_cada_100_mil_ha) %>% 
  replace(is.na(.), 0) %>% 
  rename(ano_2018 = '2018',
         ano_2008 = '2008') %>% 
  mutate(saldo = ano_2018 - ano_2008) %>% 
  left_join(cidades.brasil.nome, by = c('codufmun' = 'cod_muni_6_dig')) %>% 
  select(muni, codufmun, ano_2008, ano_2018, saldo)


# 5 - Quantidade de médicos-------------------------------------------------------------------------------------






