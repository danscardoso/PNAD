# Parâmetros da análise
ano = 2019
trimestre = 3

#####################################################################################
pacotes = c("PNADcIBGE", "dplyr")       # Lista de pacotes

for (pacote in pacotes){
  
  # O require falha quando o pacote não está instalado, com isso eu sei que tem que instalar
  if (!require(pacote, character.only = TRUE)){
    install.packages(pacote)
  }
  
  # Aqui carrega o pacote
  library(pacote, character.only = TRUE)
}


#####################################################################################
## Procurando o arquivo, caso não ache, então baixa do IBGE
nome_arquivo = paste( c("PNAD_", ano, "_", trimestre, ".txt"), collapse="" )
if(!file.exists(nome_arquivo)){
  dados_PNAD_brutos <- get_pnadc(year = ano, quarter = trimestre, design = FALSE)
  
  write.table(x=dados_PNAD_brutos, sep="\t", quote = TRUE, file = nome_arquivo,row.names = FALSE)
} else {
    dados_PNAD_brutos = read.csv(nome_arquivo, sep="\t")
}

#####################################################################################
# VD4020 = Rendimento mensal efetivo de todos os trabalhos para pessoas de 14 anos ou mais de idade (apenas para pessoas que
# receberam em dinheiro, produtos ou mercadorias em qualquer trabalho)


# A ideia aqui é somar as remunerações das pessoas do mesmo domicílio em um dado momento. Para isso eu pego as informações de
# cada domicílio e guardo nessa variavel aqui a soma das remuneracoes.

## OBS:descobri que dá pau por conta do jeito que o DPLYR lê os dados do IBGE. Um domicilio com alguem sem renda vira: NA + 1000 = NA
# para evitar isso, troco is NA por 0 na mão antes de fazer isso
dados_PNAD_brutos$rendaPessoal <- ifelse( is.na(dados_PNAD_brutos$VD4020),0,dados_PNAD_brutos$VD4020  )

remun_domicilio <- dados_PNAD_brutos %>% 
  group_by( Ano, Trimestre, UPA, Estrato, V1008 ) %>%
  summarise( sum(rendaPessoal) )

names(remun_domicilio)[names(remun_domicilio)=='sum(rendaPessoal)']<-"Proxy_Renda_Domiciliar"# Mudando o nome da variavel para facilitar a minha vida

#####################################################################################
# Adicionando a variável de renda domiciliar calculada ali em cima e consolidando os domicilios (2 entrevistas em um mesmo
# domicilio viram 1 registro só)
dados_PNAD_trabalhados <- inner_join(dados_PNAD_brutos, remun_domicilio,
                                     by = c("Ano", "Trimestre", "UPA", "Estrato", "V1008")) %>%
                          filter(V2005 == "Pessoa responsável pelo domicílio")

####################################################################################
#### Escolhendo a métrica de riqueza
####  * 1a opcao = renda domiciliar dividido por numero de pessoas 
####  * 2a opcao = renda domiciliar sem divisão

opcao = 1

if (opcao == 1){
  valores <- dados_PNAD_trabalhados$Proxy_Renda_Domiciliar / dados_PNAD_trabalhados$VD2003
} else {
  valores <- dados_PNAD_trabalhados$Proxy_Renda_Domiciliar
}

quantil_99_porcento = quantile(valores, 0.99, na.rm = TRUE)

pnad_1_porcento <- dados_PNAD_trabalhados %>%
  filter(valores >= quantil_99_porcento)

pnad_1_porcento %>%
  group_by(V4014) %>%
  summarise( n=n()) %>%
  mutate(freq = 100*n / sum(n))

####################################################################################
# Investigando aposentadoria (versão alfa)

pnad_1_porcento$dummy_idade_alta <- ifelse( pnad_1_porcento$V2009 > 65, 1, 0)

pnad_1_porcento %>%
  group_by(V4014, dummy_idade_alta) %>%
  summarise( n=n()) %>%
  mutate(freq = 100*n / sum(n))

####################################################################################
# Investigando se a pessoa faz parte do 1% ou não

if (opcao == 1){
  dados_PNAD_trabalhados$dummy_pertenceao1porcento <- ifelse( dados_PNAD_trabalhados$Proxy_Renda_Domiciliar / dados_PNAD_trabalhados$VD2003 > quantil_99_porcento, 1, 0)
} else {
  dados_PNAD_trabalhados$dummy_pertenceao1porcento <- ifelse( dados_PNAD_trabalhados$Proxy_Renda_Domiciliar > quantil_99_porcento, 1, 0)
}

dados_PNAD_trabalhados %>%
  group_by(dummy_pertenceao1porcento, V4014) %>%
  summarise( n=n()) %>%
  mutate(freq = 100*n / sum(n))



#####################################################################################
# Gráfico da parada de Pem
x = seq(0.000, 1, by=0.001)

output = data.frame()
for ( v in x ) {
  output <- rbind( output, c( v, quantile(valores, v, na.rm = TRUE)))
}

output

plot(x,y)


UPA = "110000016"
dom = 1

a <- dados_PNAD_trabalhados[ dados_PNAD_trabalhados$UPA == UPA & dados_PNAD_trabalhados$V1008 == dom  ,]
b <- dados_PNAD_brutos[ dados_PNAD_brutos$UPA == UPA & dados_PNAD_brutos$V1008 == dom  ,]

a$VD4020
b$VD4020

a$Proxy_Renda_Domiciliar
b$Proxy_Renda_Domiciliar

summary(dados_PNAD_trabalhados$Proxy_Renda_Domiciliar)
