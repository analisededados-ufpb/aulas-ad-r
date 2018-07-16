########################################
# Criando vetores e tabelas
########################################

# Vetores

alunos <- c("john", "paul", "george", "ringo")
notas_unidade1 <- c(10, 8.5, 5, 5)
notas_unidade2 <- c(9, 10, 1, 4)
notas_unidade3 <- c(8, 10, 3, 6)

# Data frame (tabela)

notas_alunos <- data.frame(aluno = alunos, nota1 = notas_unidade1, nota2 = notas_unidade2,
                           nota3 = notas_unidade3)

notas_alunos

########################################
# Como calcular a média de cada aluno?
########################################

# Usando for (vamos aprender formas melhores no futuro)

medias_alunos <- c()

for (i in 1:nrow(notas_alunos)) { # para cada linha da tabela
  linha <- notas_alunos[i, ]
  aluno <- linha$aluno
  media <- mean(c(linha$nota1, linha$nota2, linha$nota3))
  print(paste("Media para", aluno, "=", media))
  medias_alunos <- c(medias_alunos, media)
}

medias_alunos

# Adicionando nova coluna à tabela

notas_alunos$media <- medias_alunos

notas_alunos


#####################################################################
# Como indicar se um aluno foi aprovado, reprovado ou foi pra final?
#####################################################################

# Criando uma funcao

gera_situacao_aluno <- function(media) {
  if (media >= 5) {
    return("aprovado")
  }
  return("reprovado")
}

# Exemplos
gera_situacao_aluno(9)
gera_situacao_aluno(5)
gera_situacao_aluno(1)

# Gerando situacao para cada aluno
situacao_alunos <- c()
for (i in 1:nrow(notas_alunos)) { # para cada linha da tabela
  linha <- notas_alunos[i, ]
  situacao_alunos <- c(situacao_alunos, gera_situacao_aluno(linha$media))
}

notas_alunos$situacao <- as.factor(situacao_alunos)
notas_alunos

# Esse for é realmente necessario?

# Isso eh possivel?

gera_situacao_aluno(media = c(9, 5, 1)) # nao, so mostra resultado para a primeira media

# Criando funcoes que trabalham com vetores

gera_situacao_aluno_vetorizada <- function(media) {
  situacao <- ifelse(media >= 5, "aprovado", "reprovado")
  return(situacao)
}

# Agora sim!

gera_situacao_aluno_vetorizada(media = c(9, 5, 1))


# Ao inves do for, podemos fazer:

notas_alunos$situacao <- gera_situacao_aluno_vetorizada(notas_alunos$media)

# Vetorizacao eh muito usada em R. Muito mais eficiente e limpo.

# Desempenho de obter situacao para 1 milhao de medias
x <- runif(n = 1000000, min = 0, max = 10) # gerando 1 milhao de medias aleatorias

# Tempo para calcular usando o for
system.time(sit1 <- {
  res <- character(length(x))
  for (i in seq_along(x)) {
    res[i] <- gera_situacao_aluno(x[i])
  }
  res
})

# Tempo para calcular usando a funcao vetorizada
system.time(
  sit2 <- gera_situacao_aluno_vetorizada(x)
)


#####################################################################
# Calculando estatisticas gerais e filtrando dados
#####################################################################

## Estatisticas

# Quantos alunos a turma tem?

nrow(notas_alunos) # calcula quantidade de linhas da tabela
length(notas_alunos$aluno) # calcula tamanho de um vetor


# Qual foi a média da turma?

mean(notas_alunos$media)

# Qual prova foi mais dificil?

mean(notas_alunos$nota1)
mean(notas_alunos$nota2)
mean(notas_alunos$nota3)


# Qual foi a maior e a menor média da turma?

max(notas_alunos$media)
min(notas_alunos$media)


# Quantos alunos foram aprovados, reprovados e foram para a prova final?

table(notas_alunos$situacao)

# Sumario geral de estatisticas do data frame

summary(notas_alunos)


## Filtros

# Como filtrar apenas o primeiro aluno?

notas_alunos[1, ]


# Como filtrar apenas as notas de paul?

notas_alunos[notas_alunos$aluno == "paul", ]


# Como filtrar apenas a media de ringo?

notas_alunos[notas_alunos$aluno == "ringo", "media"]
notas_alunos[notas_alunos$aluno == "ringo", 5] # coluna 5
notas_alunos[notas_alunos$aluno == "ringo", ]$media


# Quais alunos foram aprovados?

notas_alunos[notas_alunos$situacao == "aprovado", ]$aluno


# Quais alunos tiraram a maior e a menor media da turma?

notas_alunos[which.max(notas_alunos$media), ] # maior media
notas_alunos[which.min(notas_alunos$media), ] # menor media


##############
## Ordenacao #
##############

# Obtendo indices ordenados da menor para a maior media

order(notas_alunos$media)

# Obtendo indices ordenados da maior para a menor media

order(notas_alunos$media, decreasing = TRUE)

# Para obter o data frame dos alunos ordenado da maior a para a menor media

notas_alunos[order(notas_alunos$media, decreasing = TRUE), ]

# Outra forma de obter apenas as medias ordenadas

sort(notas_alunos$media)

####################
# Graficos basicos #
####################

# Histograma
hist(notas_alunos$media)

# Media por aluno
plot(notas_alunos$aluno, notas_alunos$media)

# Ranking das medias
plot(order(notas_alunos$media), notas_alunos$media)

# Pizza
pie(table(notas_alunos$situacao))

# Para as proximas atividades, instalar pacotes rodando:
# install.packages("tidyverse")