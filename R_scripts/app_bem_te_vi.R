#Environment empty
rm(list = ls())
## ==========================================================================================
##                           APP BEM-TE-VI NA SAÚDE
##                                  UNIFESP
## ==========================================================================================

# Install and Load the packages
chooseCRANmirror(graphics = FALSE)
install.packages('tinytex')
tinytex::install_tinytex()
install.packages('knitr')
library(knitr)
install.packages('rmarkdown')
library(rmarkdown)
install.packages("eeptools")
library(eeptools)    
library(dplyr)
library(ggplot2)
library(tidyr)
library(tidyverse)
library(lubridate)

#############################################
## FICHA DE COLETA DE DADOS_1
#############################################

## =============================================================================
## DATA TRIAGEM
## =============================================================================
# Get current date
data_triagem <- Sys.Date()
print(paste("Data de Triagem:", data_triagem))

## =============================================================================
## Nome Completo
## =============================================================================
nome <- trimws(readline(prompt = 'Digite nome completo: '))
print(nome)

## =============================================================================
## Cartão Nacional de Saúde (CNS)
## =============================================================================
num_cns <- readline(prompt = 'Digite o número CNS: ')
num_cns <- ifelse(nchar(num_cns) == 15 | num_cns == '0', num_cns, NA)
print(num_cns)

## =============================================================================
## Endereço (Rua, numero e bairro)
## =============================================================================
endereco <- trimws(readline(prompt = 'Digite endereço: '))
print(endereco)

## =============================================================================
## Área de abrangência da UBS (1-SIM; 2- NÃO)
## =============================================================================
area_ubs <- as.numeric(gsub("[^1-2]", "",readline(prompt = 'Digite se pertence a esta UBS: ')))
print(area_ubs)

## =============================================================================
## Telefone (ddd) numero
## =============================================================================
numero_telefone <- as.numeric(gsub("[^0-9]", "", readline(prompt = 'Digite o número de telefone: ')))
print(numero_telefone)

## =============================================================================
## Tipo de Atendimento (1 - Agendada; 2 -Intercorrência)
## =============================================================================
tipo_atendimento <- as.numeric(gsub("[^1-2]", "",readline(prompt = 'Digite tipo de atendimento: ')))
print(tipo_atendimento)

## =============================================================================
## Gênero (1- Homem; 2 - Mulher, 3 - Pessoa Não Binária)
## =============================================================================
genero <- as.numeric(gsub("[^1-3]", "",readline(prompt = 'Digite genero: ')))
print(genero)

## =============================================================================
## Data Nascimento e Idade
## =============================================================================
data_nascimento <- strptime(readline(prompt = "Digite a data de nascimento (dd-mm-yyyy): "), format = "%d-%m-%Y")
data_nascimento <- as.Date(data_nascimento)  # Convert to Date class to remove time zone
print(data_nascimento)
idade <- age_calc(data_nascimento,          # Convert birth to age
                  data_triagem,
                  units = "years")
print(idade)    # Print age with decimals

## =============================================================================
## Prioridade no atendimento
## =============================================================================
# Variáveis
idade <- as.numeric(readline(prompt = 'Digite a idade: '))
deficiencia <- readline(prompt = 'Possui alguma deficiência? (S/N): ')
gravidez <- readline(prompt = 'Está grávida? (S/N): ')
amamentando <- readline(prompt = 'Está amamentando? (S/N): ')
acompanhado_bebe <- readline(prompt = 'Está acompanhado por um bebê? (S/N): ')

# Verificação de prioridade
prioridade <- ifelse(
  idade >= 60 | tolower(deficiencia) == 's' | tolower(gravidez) == 's' | tolower(amamentando) == 's' | tolower(acompanhado_bebe) == 's',
  1,
  2
)
# Saída
cat('## Prioridade no atendimento (1 - SIM; 2 - NÃO) ##\n')
service_priority <- as.numeric(gsub("[^1-2]", "",readline(prompt = 'Informe a resposta (1 - SIM; 2 - NÃO): ')))
# Resultado
print(prioridade)

## ===============================================================================================================
## Categoria da Consulta: (1-ADULTO/IDOSO | 2 - PRÉ-NATAL | 3 - PUERICULTURA | 4 - GRUPO DE SEGUIMENTO | 5 - OUTRO
## ===============================================================================================================
cat_consulta <- as.numeric(gsub("[^1-5]", "",readline(prompt = 'Digite categoria atendimento: ')))
print(cat_consulta)

## ===============================================================================================================
## Frequência de uso da UBS
## ===============================================================================================================
# Criar um data frame para armazenar informações de atendimento
dados_atendimento <- data.frame(
  num_cns = character(),
  data_triagem = as.Date(character()),
  stringsAsFactors = FALSE
)

if (!is.na(num_cns)) {
  data_triagem <- Sys.Date()
  dados_atendimento <- rbind(dados_atendimento, data.frame(num_cns, data_triagem))
  print(paste("Atendimento registrado para CNS:", num_cns, "em", format(data_triagem, "%Y-%m-%d")))
} else {
  print("Número CNS inválido. Por favor, digite um número CNS válido com 15 dígitos ou '0'.")
}

# Print contagens de atendimento por mês
if (nrow(dados_atendimento) > 0) {
  print("Contagens de atendimento por mês:")
  contagens_mensais <- table(format(dados_atendimento$data_triagem, "%Y-%m-%d"))
  print(contagens_mensais)
} else {
  print("Nenhum registro de atendimento encontrado.")
}

#############################################
## FICHA DE COLETA DE DADOS_2
#############################################

## ===============================================================================================================
## Peso | Altura | IMC | Circunfrência do Abdomen | PA sistólica e diastólica (mmHg)
## ===============================================================================================================

peso <- as.numeric(readline(prompt = 'Digite peso (em Kg): '))
print(peso)
altura <- as.numeric(readline(prompt = 'Digite altura em metros: '))
imc <- peso / (altura * altura)
print(paste("IMC calculado:", imc))
# Define IMC categories
cat_imc <- cut(
  imc,
  breaks = c(0, 15.99, 16.99, 18.49, 24.99, 29.99, 34.99, 39.99, 100),
  labels = c(
    "Magreza grau III",
    "Magreza grau II",
    "Magreza grau I",
    "Eutrofia",
    "Sobrepeso",
    "Obesidade grau I",
    "Obesidade grau II",
    "Obesidade grau III"
  ),
  include.lowest = TRUE
)
# Print the IMC category
print(paste("Categoria IMC:", as.character(cat_imc)))

abdomen <- as.numeric(readline(prompt = 'Digite a circunferência abdominal em cm: '))
print(paste("Circunferência abdominal:", abdomen, "cm"))
# Define cardiovascular risk categories
cat_risco_cv <- cut(
  abdomen,
  breaks = c(69.00, 101.99, Inf),
  labels = c("Sem risco CV", "Com risco CV"),
  include.lowest = TRUE
)
# Print the cardiovascular risk category
print(paste("Categoria de Risco Cardiovascular:", as.character(cat_risco_cv)))

pa_sistolica <- as.numeric(readline(prompt = 'Digite a PA sistólica: '))
pa_diastolica <- as.numeric(readline(prompt = 'Digite a PA diastólica: '))
print(paste("PA:", pa_sistolica, "x", pa_diastolica, "em mmHg"))

# Assume pa_sistolica and pa_diastolica are already calculated
pa_sistolica <- as.numeric(readline(prompt = 'Digite a PA sistólica: '))
pa_diastolica <- as.numeric(readline(prompt = 'Digite a PA diastólica: '))
print(paste("PA:", pa_sistolica, "x", pa_diastolica, "em mmHg"))

# Define arterial tension categories
cat_pa <- ifelse(pa_sistolica < 129 & pa_diastolica < 84, "PA normal",
                 ifelse(pa_sistolica > 130 & pa_diastolica > 85, "PA alterada",
                        NA_character_))

# Print the arterial tension category
print(paste("Categoria de Tensão Arterial:", as.character(cat_pa)))

#############################################
## FICHA DE COLETA DE DADOS_3
#############################################

## ===============================================================================================================
## Peso | Altura | IMC | PA sistólica e diastólica (mmHg) | DUm | IG | DPP | Numero Consultas PN
## ===============================================================================================================
peso_1a_consulta <- as.numeric(readline(prompt = 'Digite peso (em Kg): '))
peso_atual <- as.numeric(readline(prompt = 'Digite peso (em Kg): '))
print(peso)
(var_peso <- peso_atual - peso_1a_consulta)
print(paste("Variação de peso em gramas:", var_peso * 1000, "g"))
altura_gest <- as.numeric(readline(prompt = 'Digite altura em metros: '))
imc_gest <- peso_atual / (altura_gest * altura_gest)
print(paste("IMC calculado:", imc_gest))
# Define IMC categories
cat_imc_gest <- cut(
  imc,
  breaks = c(0, 15.99, 16.99, 18.49, 24.99, 29.99, 34.99, 39.99, 100),
  labels = c(
    "Magreza grau III",
    "Magreza grau II",
    "Magreza grau I",
    "Eutrofia",
    "Sobrepeso",
    "Obesidade grau I",
    "Obesidade grau II",
    "Obesidade grau III"
  ),
  include.lowest = TRUE
)
# Print the IMC category
print(paste("Categoria IMC:", as.character(cat_imc_gest)))

# Assume pa_sistolica and pa_diastolica are already calculated
pa_sistolica_gest <- as.numeric(readline(prompt = 'Digite a PA sistólica: '))
pa_diastolica_gest <- as.numeric(readline(prompt = 'Digite a PA diastólica: '))
print(paste("PA:", pa_sistolica_gest, "x", pa_diastolica_gest, "em mmHg"))

# Define arterial tension categories
cat_pa_gest <- ifelse(pa_sistolica_gest < 129 & pa_diastolica_gest < 84, "PA normal",
                 ifelse(pa_sistolica_gest > 130 & pa_diastolica_gest > 85, "PA alterada",
                        NA_character_))

# Print the arterial tension category
print(paste("Categoria de Tensão Arterial:", as.character(cat_pa_gest)))

dum_ig <- strptime(readline(prompt = "Digite a Data da última menstruação (dd-mm-yyyy): "), format = "%d-%m-%Y")
dum_ig <- as.Date(dum_ig)
print(paste("Data da última menstruação:", dum_ig))
# Calculate gestational age in weeks
ig_atual <- as.numeric(difftime(data_triagem, dum_ig, units = "weeks"))
# Print the gestational age
print(paste("Idade Gestacional atual:", ig_atual, "em semanas"))

# Calculate estimated due date (dpp)
estimated_due_date <- dum_ig - months(3) + years(1) + days(7)
dpp <- as.character(estimated_due_date)
# Print the estimated due date
print(paste("Data Provável do Parto:", dpp))

# Criar um data frame para armazenar informações de atendimento
num_consultas_pn <- data.frame(
  num_cns = character(),
  data_triagem = as.Date(character()),
  stringsAsFactors = FALSE
)

if (!is.na(num_cns)) {
  data_triagem <- Sys.Date()
  num_consultas_pn <- rbind(num_consultas_pn, data.frame(num_cns, data_triagem))
  print(paste("Atendimento registrado para CNS:", num_cns, "em", format(data_triagem, "%Y-%m-%d")))
} else {
  print("Número CNS inválido. Por favor, digite um número CNS válido com 15 dígitos ou '0'.")
}

# Print contagens de atendimento por mês
if (nrow(num_consultas_pn) > 0) {
  print("Contagens de atendimento por mês:")
  contagens_mensais <- table(format(num_consultas_pn$data_triagem, "%Y-%m-%d"))
  print(contagens_mensais)
} else {
  print("Nenhum registro de atendimento encontrado.")
}

#############################################
## FICHA DE COLETA DE DADOS_4
#############################################

## ===============================================================================================================
## Peso | Comprimento | IMC Infantil | Perimetro Cefálico (cm) 
## ===============================================================================================================
peso_1a_consulta_ped <- as.numeric(readline(prompt = 'Digite peso (em Kg): '))
peso_atual_ped <- as.numeric(readline(prompt = 'Digite peso (em Kg): '))
print(peso_atual_ped)
(var_peso_ped <- peso_atual_ped - peso_1a_consulta_ped)
print(paste("Variação de peso em gramas:", var_peso_ped * 1000, "g"))
comprimento <- as.numeric(readline(prompt = 'Digite comprimento em cm: '))
print(paste("Comprimento:", comprimento , "em cm"))
altura_ped <- as.numeric(readline(prompt = 'Digite altura em metros: '))
imc_infantil <- peso_atual_ped / (altura_ped * altura_ped)
print(paste("IMC infantil:", imc_infantil))
perimetro_cefalico <- as.numeric(readline(prompt = 'Digite Perimetro Cefálico em cm: '))
print(paste("Perimetro cefálico:", perimetro_cefalico, "cm"))

# Replace 'your_script.R' with the actual filename of your R script
#knit2pdf('app_bem_te_vi.R')

# Replace 'app_bem_te_vi.R' with the actual filename of your R script
#render('app_bem_te_vi.R', output_format = 'pdf_document')


