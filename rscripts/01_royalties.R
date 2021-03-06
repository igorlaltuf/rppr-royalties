# Royalties CFM e CFH deflacionados de 2008 at� 2018 em valores de 2020 na AMZL.
# Deflacionamento pela m�dia anual do IPCA.

# CFM - Compensa��o Financeira pela Explora��o de Recursos Minerais
# CFH - Compensa��o Financeira pela Explora��o de Recursos H�dricos para Gera��o de Energia El�trica

# 1 importar dados e declarar vari�veis iniciais ----------------------------------------

rm(list=ls()) 

source('Rscripts/00_bibliotecas.R')

options(scipen = 999) # remove nota��o cient�fica

# cidades Amaz�nia Legal
cidades.amazonia.legal.nome <- read_excel(path = 'input/municipios_amazonia_legal.xlsx')

cidades.amazonia.legal <- as.vector(cidades.amazonia.legal.nome$cod_muni)

ipca <- read_excel('input/ipca_indice.xlsx')

ipca.2020 <- ipca$media_numero_indice_ipca[27]

# 2 - Criar, deflacionar e salvar dados de royalties das amostras --------------------------

# Royalties de minera��o, energia e petr�leo (exclui FEP) de 2008 at� 2018
royalties <- read_csv2('Input/transferencias_para_munic�pios_1990_2020_utf8.csv') %>% 
  
  janitor::clean_names() %>% 
  
  mutate(valor_consolidado = as.numeric(gsub('[[:punct:] ]+','', substr(valor_consolidado, 3, 100)))/100) %>%  # remove o R$, ponto e v�rgula da string, converte em n�mero e divide por 100 p/ incluir decimal
  
  dplyr::filter(!transferencia %in% c('Royalties - FEP', 'Royalties - PEA')) %>%  # exclui estas duas categorias
  
  left_join(ipca) %>% 
  
  mutate(valor_real = valor_consolidado * ipca.2020 / media_numero_indice_ipca) %>% # deflacionar
  
  select(5,1,2,3,4,8,7) 

# Royalties da minera��o -----------------------------------------------------
royalties_cfm <- royalties %>% 
 
   dplyr::filter(transferencia %in% 'Royalties - CFM',
                 
                ano %in% c(2008:2018),
                
                codigo_ibge %in% cidades.amazonia.legal) %>% 
  
  select(1:3,5,6) %>% 
  
  rename('royalt_cfm_real' = 'valor_real',
         
         'royalt_cfm_nominal' = 'valor_consolidado') %>% 
  
  mutate(royalt_cfm_real = round(royalt_cfm_real, 2))

write.csv2(royalties_cfm, 'temp/royalties_mineracao_2008_2018_amzl.csv', row.names = F, fileEncoding = 'UTF-8')


# Royalties da gera��o de energia el�trica  -----------------------------------
royalties_cfh <-  royalties %>% 
  
  dplyr::filter(transferencia %in% 'Royalties - CFH',
                
                ano %in% c(2008:2018),
                
                codigo_ibge %in% cidades.amazonia.legal) %>%  
  
  select(1:3,5,6) %>% 
  
  rename('royalt_cfh_real' = 'valor_real',
         
         'royalt_cfh_nominal' = 'valor_consolidado') %>% 
  
  mutate(royalt_cfh_real = round(royalt_cfh_real, 2))

write.csv2(royalties_cfh, 'temp/royalties_energia_2008_2018_amzl.csv', row.names = F,  fileEncoding = 'UTF-8')
