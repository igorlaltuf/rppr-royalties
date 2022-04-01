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


estab_cfh <- dados.st %>% 
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



estab_cfm <- dados.st %>% 
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


# 5 - Quantidade de médicos (todos os tipos) -------------------------------------------------------------------------------------

cbo.2002 <- read.csv2('input/cbo_2002_ocupacao.csv') %>%   
  janitor::clean_names() %>% 
  dplyr::filter(codigo >= 225103 & # filtrar cargos de médicos independente da especialização
                codigo <= 225355) %>% 
  mutate(codigo = as.character(codigo)) 

cbo.1994 <- read.csv2('input/cbo94-datasus.csv', encoding = 'UTF-8') %>%     
  filter(str_detect(profissao, "Médico|Medicos")) 

cbo.2007.10 <- read.csv2('input/cbo2007-10-conv.csv', encoding = 'UTF-8') 

lista.de.arquivos <- list.files(path = "input/CNES-PF/", recursive = TRUE,
                                pattern = "\\.dbc$", 
                                full.names = TRUE)

# dados do mês de dezembro para cada ano de 2006 a 2020
# ano <- c(2006:2020)
marc <- c(1:135)
i <- 1
while (i <= length(lista.de.arquivos)) {
  #pop$cod_muni6 <- as.numeric(substr(pop$cod_muni,1,6))
  
  x <- read.dbc::read.dbc(lista.de.arquivos[i]) %>%  
    janitor::clean_names() %>%
    mutate(codufmun = as.character(codufmun),
           tpgestao = as.character(tpgestao),
           cbo = as.character(cbo),
           prof_sus = as.numeric(as.character(prof_sus)),
           competen = as.character(competen)) %>% 
    mutate(ano = as.numeric(substr(competen,1,4))) %>% 
    select(codufmun, ano, tpgestao, cbo, prof_sus)
  
  # classificação pela cbo94
  ano.2006 <- x %>%
    dplyr::filter(cbo %in% cbo.1994$cod_cbo_94) %>% # filtrar cargos de médicos independente da especialização
    group_by(codufmun, ano) %>%
    summarise(qtd_med_sus = sum(prof_sus)) %>%
    left_join(pop, by = c('ano', 'codufmun' = 'cod_muni_6_dig')) %>%    
    mutate(med_sus_100_mil_hab = (qtd_med_sus/populacao)*100000)
  
  # classificação segundo a Tabela de conversão da classificação Brasileira de Ocupações 
  # disponibilizada pelo Ministério do Trabalho e Emprego (MTE).
  # http://www.sbpc.org.br/upload/noticias_setor/320110927123631.pdf
  anos2007.2010 <- x %>%
    dplyr::filter(cbo %in% cbo.2007.10$cod_antigo) %>% # filtrar cargos de médicos independente da especialização
    group_by(codufmun, ano) %>% # usar tpgestao aqui para saber os médicos do municipio, estado ou duplo
    summarise(qtd_med_sus = sum(prof_sus)) %>%
    left_join(pop, by = c('ano', 'codufmun' = 'cod_muni_6_dig')) %>%    
    mutate(med_sus_100_mil_hab = (qtd_med_sus/populacao)*100000)
  
  anos2011.2020 <- x %>%
    dplyr::filter(cbo %in% cbo.2002$codigo) %>% # filtrar cargos de médicos independente da especialização
    group_by(codufmun, ano) %>%
    summarise(qtd_med_sus = sum(prof_sus)) %>%
    left_join(pop, by = c('ano', 'codufmun' = 'cod_muni_6_dig')) %>%   
    mutate(med_sus_100_mil_hab = (qtd_med_sus/populacao)*100000)
  
  x <- rbind(ano.2006, anos2007.2010, anos2011.2020)
  
  assign(paste("cnes.pf",marc[i], sep="."), x)
  i <- i + 1
}

dados <- do.call(rbind, lapply(paste0("cnes.pf.",1:135), get))

medic_cfh <- dados %>% 
  dplyr::filter(ano %in% c(2008,2018),
                codufmun %in% cfh_muni_20) %>% 
  select(codufmun, ano, muni, med_sus_100_mil_hab) %>% 
  pivot_wider(names_from = ano, values_from = med_sus_100_mil_hab) %>% 
  rename(ano_2018 = '2018',
         ano_2008 = '2008') %>% 
  mutate(saldo = ano_2018 - ano_2008)
  

medic_cfm <- dados %>% 
  dplyr::filter(ano %in% c(2008,2018),
                codufmun %in% cfm_muni_20) %>% 
  select(codufmun, ano, muni, med_sus_100_mil_hab) %>% 
  pivot_wider(names_from = ano, values_from = med_sus_100_mil_hab) %>% 
  rename(ano_2018 = '2018',
         ano_2008 = '2008') %>% 
  mutate(saldo = ano_2018 - ano_2008)



# 6 - Tabela síntese ------------------------------------------------------------------------------------------
estab_cfm <- estab_cfm %>% 
  rename('estab_2008' = ano_2008,
         'estab_2018' = ano_2018,
         'estab_saldo' = saldo)

leitos_cfm <- leitos_cfm %>% 
  rename('leitos_2008' = ano_2008,
         'leitos_2018' = ano_2018,
         'leitos_saldo' = saldo)

medic_cfm <- medic_cfm %>% 
  rename('medic_2008' = ano_2008,
         'medic_2018' = ano_2018,
         'medic_saldo' = saldo)

cfm_sintese <- left_join(estab_cfm, leitos_cfm) %>% 
  left_join(medic_cfm)

write.csv2(cfm_sintese, file = 'output/sintese_cfm.csv', row.names = F)

estab_cfh <- estab_cfh %>% 
  rename('estab_2008' = ano_2008,
         'estab_2018' = ano_2018,
         'estab_saldo' = saldo)

leitos_cfh <- leitos_cfh %>% 
  rename('leitos_2008' = ano_2008,
         'leitos_2018' = ano_2018,
         'leitos_saldo' = saldo)

medic_cfh <- medic_cfh %>% 
  rename('medic_2008' = ano_2008,
         'medic_2018' = ano_2018,
         'medic_saldo' = saldo)

cfh_sintese <- left_join(estab_cfh, leitos_cfh) %>% 
  left_join(medic_cfh)

write.csv2(cfh_sintese, file = 'output/sintese_cfh.csv', row.names = F)


# 7 - Leitos de internação vinculados ao SUS
# leitos_uti_2008 <- read.csv2('input/leitos_uti_2008.csv') %>% 
#   janitor::clean_names() %>% 
#   rename('leitos_uti_sus_2008' = quantidade_sus) %>% 
#   mutate(ano = 2008) %>% 
#   mutate(cod_ibge = substr(municipio, 1, 6)) %>% 
#   left_join(pop, by = c('cod_ibge' = 'cod_muni_6_dig', 'ano')) %>% 
#   mutate(leitos_uti_sus_2008_100_mil_hab = (leitos_uti_sus_2008/populacao)*100000) %>% 
#   select(6,4,2,9)
# 
# leitos_uti_2018 <- read.csv2('input/leitos_uti_2018.csv')%>% 
#   janitor::clean_names() %>% 
#   rename('leitos_uti_sus_2018' = quantidade_sus) %>% 
#   mutate(ano = 2018) %>% 
#   mutate(cod_ibge = substr(municipio, 1, 6)) %>% 
#   left_join(pop, by = c('cod_ibge' = 'cod_muni_6_dig', 'ano')) %>% 
#   mutate(leitos_uti_sus_2018_100_mil_hab = (leitos_uti_sus_2018/populacao)*100000) %>% 
#   select(6,4,2,9)
# 
# leitos_uti <- left_join(leitos_uti_2008, leitos_uti_2018) 
# 
#   mutate(cod_ibge = substr(municipio, 1, 6)) 
#   












