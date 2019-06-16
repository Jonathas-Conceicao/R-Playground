#! /usr/bin/Rscript
library(ggplot2)

process <- function(file = "clean_data.csv") {
  table <- read.csv(file, header=TRUE)
  table
}

bar_graph <- function(df,
                     col=1,
                     title="Titulo",
                     xlab="Eixo X",
                     ylab="Contagem",
                     text_offset=2,
                     conf=labs()) {
  df <- data.frame(sort(df[,col]))
  names(df) <- c("V1")
  ggplot(df, aes(x=V1)) +
    conf +
    labs(title = title) +
    labs(x = xlab) +
    labs(y = ylab) +
    geom_bar() +
    geom_text(
        aes(label=..count.., y=..count..+text_offset),
        stat="count",
        position = position_dodge(0.9)
    )
}

t <- process()

bar_graph(
    t,
    1,
    "Curso",
    "",
    conf=coord_flip()
)

bar_graph(
    t,
    2,
    "Ingresso",
    "ano/semestre",
    text_offset=1,
)

bar_graph(
    t,
    3,
    "Consumo de Álcool",
    "Frequência"
)

bar_graph(
    t,
    4,
    "Pratica Atividade Física",
    "",
    text_offset=1
)

bar_graph(
    t,
    6,
    "Tem alguma prática espiritual, filosófica ou religião",
    ""
)

bar_graph(
    t,
    8,
    "Tempo médio de uso diário do smartphone",
    "Número de horas",
    text_offset=1
)

bar_graph(
    t,
    9,
    "Número de disciplinas cursadas",
    ""
)

bar_graph(
    t,
    10,
    "Já pensou em desistir do curso",
    ""
)

bar_graph(
    t,
    11,
    "Satisfação com a estrutura da UFPel",
    "Nível",
    text_offset=1,
    conf=scale_x_continuous(
        breaks=seq(0, 10, 2)
    )
)
