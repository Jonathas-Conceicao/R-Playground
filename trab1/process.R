#! /usr/bin/Rscript
library(ggplot2)

process <- function(file = "clean_data.csv") {
  table <- read.csv(file, header=TRUE)
  table
}

bar_graph <- function(df,
                     col=1,
                     title="Titulo",
                     x="Eixo X",
                     y="Contagem",
                     conf=labs(),
                     conf2=aes()) {
  df <- data.frame(sort(df[,col]))
  names(df) <- c("V1")
  ggplot(df, aes(x=V1)) +
    labs(title = title) +
    labs(x = x) +
    labs(y = y) +
    conf +
    ## labs(colour = "Implementation") +
    geom_bar(conf2, position = "dodge") +
    geom_text(
        aes(label=..count..),
        stat="count",
        position = position_dodge(0.9),
        vjust = 0
    )
}

t <- process()
bar_graph(t,
         1,
         "Curso",
         "",
         conf=coord_flip())
bar_graph(t,
         2,
         "Ingresso",
         "ano/semestre")
bar_graph(t,
         3,
         "Consome Álcool",
         "")
bar_graph(t,
         4,
         "Pratica Atividade Física",
         "")
bar_graph(t,
         6,
         "Tem alguma prática espiritual, filosófica ou religião",
         "")
bar_graph(t,
         8,
         "Tempo médio de uso diário do smartphone em minutos",
         "Número de horas")
bar_graph(t,
         9,
         "Número de disciplinas cursadas",
         "")
bar_graph(t,
         10,
         "Já pensou em desistir do curso",
         "")
bar_graph(t,
         11,
         "Satisfação com a estrutura da UFPel",
         "Nível")

