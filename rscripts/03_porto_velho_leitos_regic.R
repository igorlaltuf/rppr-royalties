# Leitos de internação vinculados ao SUS 
# Leitos de internação são aqueles para quem precisa ficar mais de 24h no hospital (COVID).
# https://portal.cfm.org.br/noticias/covid-19-interrompe-decada-de-queda-em-leitos-de-internacao-no-sistema-unico-de-saude-sus/

# Remover dados de 2021, já que não tenho a estimativa populacional


rm(list=ls()) 
# carregar scripts
source('Rscripts/00_bibliotecas.R')
source('Rscripts/00_funcoes_globais.R')

# dados populacionais
pop <- read_csv2('input/pop_muni_2000-2020.csv') # População - 2001 - 2020
cod_muni <- as.character(str_sub(pop$cod_muni, 1, 6))
pop$cod_muni <- cod_muni
pop$ano <- as.character(pop$ano)

# obs: os dados são do mês de dezembro de cada ano, com exceção de 2021 em que os dados usados foram do mês de novembro.
# leitos de internação da CNES
leitos <- read_csv2('input/leitos_de_internação_2005-2021.csv') %>% 
  janitor::clean_names() %>% 
  select(1:17) # remove 2021 (não tenho dados populacionais ainda)

cod_muni <- str_sub(leitos$municipio, 1, 6)
leitos$municipio <- str_sub(leitos$municipio, 8, 50)
leitos$cod_muni <- cod_muni

leitos <- leitos[, c(18,1:17)] # reordenar colunas
colnames(leitos) <- c('cod_muni','muni','2005','2006','2007','2008','2009','2010','2011','2012','2013','2014','2015','2016','2017','2018','2019','2020')
leitos <- pivot_longer(leitos,
                       cols = starts_with("20"),
                       names_to = 'ano',
                       values_to = 'leitos_internacao_sus') 

leitos$leitos_internacao_sus <- as.numeric(leitos$leitos_internacao_sus)

# remover NAs
is.na(leitos$leitos_internacao_sus)
leitos$leitos_internacao_sus[is.na(leitos$leitos_internacao_sus)] <- 0


# reunir os dados
dados <- left_join(leitos, pop, by = c('cod_muni','ano')) %>% 
  rename('muni' = 'muni.x', 'muni_uf' = 'muni.y') %>% 
  mutate(leitos_cada_100_mil_ha = (leitos_internacao_sus/populacao)*100000) %>% 
  dplyr::filter(ano %in% as.character(c(2008:2018)))


# Regic de saúde de alta complexidade com destino em Porto Velho (RO)
porto_velho_regic <- c("1100015","1100023","1100049","1100080","1100098","1100106","1100114","1100122","1100130","1100148","1100155",
                       "1100254","1100262","1100288","1100304","1100320","1100403","1100338","1100346","1100379","1100452","1100502",
                       "1100601","1101104","1100700","1100908","1101005","1101203","1101302","1101435","1101476","1101492","1101500",
                       "1101559","1101609","1101708","1101757","1101807","1300144","1301704","1302405") %>% 
  substr(1,6)


# Adicionar Porto Velho à lista de municípios acima (Porto Velho + sua regic)
porto_velho_regic <- append(porto_velho_regic, '110020')

# Qtd de leitos em Porto Velho (RO)
leitos.pv <- dados %>% 
  dplyr::filter(cod_muni %in% "110020") %>% 
  group_by(ano) %>% 
  summarise(populacao = sum(populacao),
            leitos_internacao_sus = sum(leitos_internacao_sus)) %>% 
  mutate(leitos_cada_100_mil_ha = (leitos_internacao_sus/populacao)*100000)

# Qtd de leitos em Porto Velho (RO) + sua REGIC de atendimentos de saúde de alta complexidade
leitos.pv.e.regic <- dados %>% 
  dplyr::filter(cod_muni %in% porto_velho_regic) %>% 
  group_by(ano) %>% 
  summarise(populacao = sum(populacao),
            leitos_internacao_sus = sum(leitos_internacao_sus)) %>% 
  mutate(leitos_cada_100_mil_ha = (leitos_internacao_sus/populacao)*100000)



# fazer gráfico PV e REGIC
grafico.leitos <- ggplot() +
  geom_bar(leitos.pv.e.regic, mapping = aes(x = ano, y = leitos_cada_100_mil_ha), col = 'grey', fill = 'grey', stat = 'identity') +
  labs(y = 'Quantidade de leitos a cada\n100 mil habitantes', x = 'Ano') +
  theme_classic()+
  theme(plot.caption = element_text(hjust = 0, face= "italic"), #Default is hjust=1
        plot.title = element_text(hjust = 0.5))

grafico.leitos 
ggsave(plot = grafico.leitos,'output/pv_e_regic_leitos_internação.png', width = 9, height = 6, dpi = 600, scale = .6)


# fazer gráfico PV apenas
grafico.leitos.pv <- ggplot() +
  geom_bar(leitos.pv, mapping = aes(x = ano, y = leitos_cada_100_mil_ha), col = 'grey', fill = 'grey', stat = 'identity') +
  labs(y = 'Quantidade de leitos a cada\n100 mil habitantes', x = 'Ano') +
  theme_classic()+
  theme(plot.caption = element_text(hjust = 0, face= "italic"), #Default is hjust=1
        plot.title = element_text(hjust = 0.5))

grafico.leitos.pv 
ggsave(plot = grafico.leitos.pv,'output/pv_leitos_internação.png', width = 9, height = 6, dpi = 600, scale = .6)

