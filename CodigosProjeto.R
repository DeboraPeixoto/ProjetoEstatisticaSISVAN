install.packages("ggplot2")
library("ggplot2")



## TAMANHO BASE UTILIZADA
df = sisvan_estado_nutricional_2021
dim(df)

## Quantas pessoas foram analisadas neste relatório?
n_distinct(df$CO_ACOMPANHAMENTO)

## Qual a quantidade de pessoas por faixa de idade?
ggplot(df, aes(DS_FASE_VIDA)) + geom_bar()+ coord_flip()

table(df$DS_FASE_VIDA)

## Qual a distribuição de idade?
summary(df$NU_IDADE_ANO)

ggplot(df, aes(NU_IDADE_ANO)) + geom_boxplot() 


## Qual a distribuição de altura?
summary(df$NU_ALTURA_ROUND)

ggplot(df, aes(NU_ALTURA_ROUND)) + geom_boxplot()

## Qual a quantidade de pessoas por sexo?

ggplot(df, aes(SG_SEXO, fill = SG_SEXO)) +
  geom_bar() +
  scale_fill_manual(values = c("F" = "purple", "M" = "darkgreen"))

table(df$SG_SEXO)	


## Qual a quantidade de pessoas por raça ou cor?
ggplot(df, aes(DS_RACA_COR, fill = "orange")) + geom_bar()

table(df$DS_RACA_COR)

## Qual a quantidade de pessoas por UF?
ggplot(df, aes(SG_UF)) + geom_bar()

table(df$SG_UF)

## Dado que o maior volume de pessoas estão categorizadas como adultas, representando 55,4% da base, qual a quantidade de adultos por estado nutricional?
## GRAFICO BARRAS

estado_nutri <- table(sisvan_estado_nutricional_2021$CO_ESTADO_NUTRI_ADULTO)
estado_nutri <- as.data.frame(estado_nutri)

estado_nutri$Var1 <- iconv(estado_nutri$Var1, from="UTF-8", to="LATIN1")
na_indices <- is.na(estado_nutri$Var1)

# Substituir os NA por um novo valor, por exemplo, "Valor Ausente"
estado_nutri$Var1[na_indices] <- "Adequado ou Eutrofico"
print(estado_nutri)

ggplot(data = estado_nutri, aes(x =Var1, y=Freq)) +
  # Adicionar a camada de barras
  geom_bar(stat = "identity", position = "stack") +
  
  # Adicionar título e rótulos dos eixos
  labs(title = "Gráfico de Barras",
       x = "Categoria",
       y = "Contagem")


## Qual a quantidade de adultos fora da normalidade de peso (baixo peso, sobrepeso e obesidade)?
table(df$CO_ESTADO_NUTRI_ADULTO)

## Qual a quantidade de idosos fora da normalidade de peso (baixo peso e sobrepeso)?
table(df$CO_ESTADO_NUTRI_IDOSO)

## Análise bivariada
filtered_data <- sisvan_estado_nutricional_2021[!is.na(sisvan_estado_nutricional_2021$PESO.X.IDADE) & sisvan_estado_nutricional_2021$PESO.X.IDADE != "", ]
tab <- table(filtered_data$SG_UF, filtered_data$PESO.X.IDADE)
tab_df <- as.data.frame(tab)

## Probabilidade
summary(sisvan_estado_nutricional_2021$CO_CNES)
df <- sisvan_estado_nutricional_2021
df[df==''] <- NA

estado_nutri <- table(df$CO_ESTADO_NUTRI_ADULTO)

table(df$CO_ESTADO_NUTRI_ADULTO)

Total_Adultos_Categorizados_Nutricionalmente <- sum(table(df$CO_ESTADO_NUTRI_ADULTO))

## ADEQUADO OU EUTRÓFICO
Total_Adultos_Adequado_ou_Eutrofico <- 1839
Prob_Adequado_ou_Eutrofico <- Total_Adultos_Adequado_ou_Eutrofico / Total_Adultos_Categorizados_Nutricionalmente
print(Prob_Adequado_ou_Eutrofico)

## BAIXO PESO
Total_Adultos_Baixo_Peso <- 125
Prob_Baixo_Peso <- Total_Adultos_Baixo_Peso / Total_Adultos_Categorizados_Nutricionalmente
print(Prob_Baixo_Peso)

## OBESIDADE GRAU 1
Total_Adultos_Obesidade_Grau_I <- 1311
Prob_Obesidade_Grau_I <- Total_Adultos_Obesidade_Grau_I / Total_Adultos_Categorizados_Nutricionalmente
print(Prob_Obesidade_Grau_I)


## OBESIDADE GRAU 2
Total_Adultos_Obesidade_Grau_II <- 555
Prob_Obesidade_Grau_II <- Total_Adultos_Obesidade_Grau_II / Total_Adultos_Categorizados_Nutricionalmente
print(Prob_Obesidade_Grau_II)

## OBESIDADE GRAU 3
Total_Adultos_Obesidade_Grau_III <- 237
Prob_Obesidade_Grau_II <- Total_Adultos_Obesidade_Grau_III / Total_Adultos_Categorizados_Nutricionalmente
print(Prob_Obesidade_Grau_II)


## SOBREPESO
Total_Adultos_Sobrepeso <- 2165
Prob_Sobrepeso <- Total_Adultos_Sobrepeso / Total_Adultos_Categorizados_Nutricionalmente
print(Prob_Sobrepeso)


################### MULHERES / HOMENS

## Quantidade de homens adultos sobrepeso
Homens_Adultos_Sobrepeso <- subset(df, df$CO_ESTADO_NUTRI_ADULTO == "Sobrepeso" & df$SG_SEXO == "M")
Quantidade_Homens_Adultos_Sobrepeso <- nrow(Homens_Adultos_Sobrepeso)
Quantidade_Homens_Adultos_Sobrepeso


## Quantidade de mulheres adultas sobrepeso
Mulheres_Adultas_Sobrepeso <- subset(df, df$CO_ESTADO_NUTRI_ADULTO == "Sobrepeso" & df$SG_SEXO == "F")
Quantidade_Mulheres_Adultas_Sobrepeso <- nrow(Mulheres_Adultas_Sobrepeso)
Quantidade_Mulheres_Adultas_Sobrepeso


Prob_Homem_Aleatorio_Sobrepeso <- Quantidade_Homens_Adultos_Sobrepeso / (Quantidade_Homens_Adultos_Sobrepeso + Quantidade_Mulheres_Adultas_Sobrepeso)
Prob_Homem_Aleatorio_Sobrepeso

Prob_Mulher_Aleatoria_Sobrepeso <- Quantidade_Mulheres_Adultas_Sobrepeso / (Quantidade_Homens_Adultos_Sobrepeso + Quantidade_Mulheres_Adultas_Sobrepeso)
Prob_Mulher_Aleatoria_Sobrepeso


## Quantidade de homens adultos obesidade I
Homens_Adultos_Obesidade_Grau_I <- subset(df, df$CO_ESTADO_NUTRI_ADULTO == "Obesidade Grau I" & df$SG_SEXO == "M")
Quantidade_Homens_Adultos_Obesidade_Grau_I <- nrow(Homens_Adultos_Obesidade_Grau_I)
Quantidade_Homens_Adultos_Obesidade_Grau_I


## Quantidade de mulheres adultas obesidade I
Mulheres_Adultas_Obesidade_Grau_I <- subset(df, df$CO_ESTADO_NUTRI_ADULTO == "Obesidade Grau I" & df$SG_SEXO == "F")
Quantidade_Mulheres_Adultas_Obesidade_Grau_I <- nrow(Mulheres_Adultas_Obesidade_Grau_I)
Quantidade_Mulheres_Adultas_Obesidade_Grau_I


Prob_Homem_Aleatorio_Obesidade_Grau_I <- Quantidade_Homens_Adultos_Obesidade_Grau_I / (Quantidade_Homens_Adultos_Obesidade_Grau_I + Quantidade_Mulheres_Adultas_Obesidade_Grau_I)
Prob_Homem_Aleatorio_Obesidade_Grau_I

Prob_Mulher_Aleatoria_Obesidade_Grau_I <- Quantidade_Mulheres_Adultas_Obesidade_Grau_I / (Quantidade_Homens_Adultos_Obesidade_Grau_I + Quantidade_Mulheres_Adultas_Obesidade_Grau_I)
Prob_Mulher_Aleatoria_Obesidade_Grau_I


## Quantidade de homens adultos obesidade II
Homens_Adultos_Obesidade_Grau_II <- subset(df, df$CO_ESTADO_NUTRI_ADULTO == "Obesidade Grau II" & df$SG_SEXO == "M")
Quantidade_Homens_Adultos_Obesidade_Grau_II <- nrow(Homens_Adultos_Obesidade_Grau_II)
Quantidade_Homens_Adultos_Obesidade_Grau_II


## Quantidade de mulheres adultas obesidade II
Mulheres_Adultas_Obesidade_Grau_II <- subset(df, df$CO_ESTADO_NUTRI_ADULTO == "Obesidade Grau II" & df$SG_SEXO == "F")
Quantidade_Mulheres_Adultas_Obesidade_Grau_II <- nrow(Mulheres_Adultas_Obesidade_Grau_II)
Quantidade_Mulheres_Adultas_Obesidade_Grau_II


Prob_Homem_Aleatorio_Obesidade_Grau_II <- Quantidade_Homens_Adultos_Obesidade_Grau_II / (Quantidade_Homens_Adultos_Obesidade_Grau_II + Quantidade_Mulheres_Adultas_Obesidade_Grau_II)
Prob_Homem_Aleatorio_Obesidade_Grau_II

Prob_Mulher_Aleatoria_Obesidade_Grau_II <- Quantidade_Mulheres_Adultas_Obesidade_Grau_II / (Quantidade_Homens_Adultos_Obesidade_Grau_II + Quantidade_Mulheres_Adultas_Obesidade_Grau_II)
Prob_Mulher_Aleatoria_Obesidade_Grau_II



## Quantidade de homens adultos obesidade III
Homens_Adultos_Obesidade_Grau_III <- subset(df, df$CO_ESTADO_NUTRI_ADULTO == "Obesidade Grau III" & df$SG_SEXO == "M")
Quantidade_Homens_Adultos_Obesidade_Grau_III <- nrow(Homens_Adultos_Obesidade_Grau_III)
Quantidade_Homens_Adultos_Obesidade_Grau_III


## Quantidade de mulheres adultas obesidade III
Mulheres_Adultas_Obesidade_Grau_III <- subset(df, df$CO_ESTADO_NUTRI_ADULTO == "Obesidade Grau III" & df$SG_SEXO == "F")
Quantidade_Mulheres_Adultas_Obesidade_Grau_III <- nrow(Mulheres_Adultas_Obesidade_Grau_III)
Quantidade_Mulheres_Adultas_Obesidade_Grau_III


Prob_Homem_Aleatorio_Obesidade_Grau_III <- Quantidade_Homens_Adultos_Obesidade_Grau_III / (Quantidade_Homens_Adultos_Obesidade_Grau_III + Quantidade_Mulheres_Adultas_Obesidade_Grau_III)
Prob_Homem_Aleatorio_Obesidade_Grau_III

Prob_Mulher_Aleatoria_Obesidade_Grau_III <- Quantidade_Mulheres_Adultas_Obesidade_Grau_III / (Quantidade_Homens_Adultos_Obesidade_Grau_III + Quantidade_Mulheres_Adultas_Obesidade_Grau_III)
Prob_Mulher_Aleatoria_Obesidade_Grau_III

## Invervalo de confiança 
#Filtrando as amostras por fase da vida
imc_adultos = df$DS_IMC_ROUND[df$DS_FASE_VIDA == "ADULTO"]
imc_idosos = df$DS_IMC_ROUND[df$DS_FASE_VIDA == "IDOSO"]

#Consultando o sumário estatístico dos dados filtrados
summary(imc_adultos)
summary(imc_idosos)

#Selecionando aleatoriamente 100 amostras de cada conjunto
amostra_adulto <- head(sample(imc_adultos, 100), 100)
amostra_idoso <- head(sample(imc_idosos, 100), 100)

#Sumário estatístico das amostras
summary(amostra_adulto)
summary(amostra_idoso)

#Verificação da distribuição dos dados
qqline(amostra_adulto)
qqline(amostra_idoso)


#Medidas resumo
#Adultos: Média -> 28.65
#Adultos: Desvio padrão -> 5.44
#Idosos: Média -> 27.92
#Idosos: Desvio padrão -> 5.30
sd(amostra_adulto)
5.442769

sd(amostra_idoso)
5.302334

#Intervalo de confiança de 95%
z_star_95 <- qnorm(0.975)
#adultos
menor_a = (28.65) - z_star_95 * (5.44/ sqrt(100))
	27.58378

maior_a = (28.65) + z_star_95 * (5.44/ sqrt(100))
	29.71622


#idosos
menor_i = (27.92) - z_star_95 * (5.30/ sqrt(100))
	26.88122

maior_i = (27.92) + z_star_95 * (5.30/ sqrt(100))
	28.95878


## Teste de hipótese
#H0: mu = 28.65
#H1: mu != 28.65
#nível de significância: 5%

#Teste t
t.test(amostra_adulto, mu = 28.65, alternative = "two.sided", conf.level = 0.95)
  One Sample t-test

data:  amostra_adulto
t = -0.044, df = 99, p-value = 0.965
alternative hypothesis: true mean is not equal to 28.65
95 percent confidence interval:
 27.58378 29.71622
sample estimates:
mean of x 
   28.65

#H0: mu = 27.92
#H1: mu != 27.92
#nível de significância: 5%

#Teste t
t.test(amostra_idoso, mu = 27.92, alternative = "two.sided", conf.level = 0.95)
  One Sample t-test

data:  amostra_idoso
t = 0.044, df = 99, p-value = 0.965
alternative hypothesis: true mean is not equal to 27.92
95 percent confidence interval:
 26.88122 28.95878
sample estimates:
mean of x 
   27.92

#H0: mu = 28.65
#H1: mu != 28.65
#nível de significância: 5%

#Teste t
t.test(amostra_adulto, mu = 28.65, alternative = "two.sided", conf.level = 0.95)
  One Sample t-test

data:  amostra_adulto
t = -0.044, df = 99, p-value = 0.965
alternative hypothesis: true mean is not equal to 28.65
95 percent confidence interval:
 27.58378 29.71622
sample estimates:
mean of x 
   28.65

#H0: mu = 27.92
#H1: mu != 27.92
#nível de significância: 5%

#Teste t
t.test(amostra_idoso, mu = 27.92, alternative = "two.sided", conf.level = 0.95)
  One Sample t-test
  
