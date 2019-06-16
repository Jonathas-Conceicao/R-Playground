#! /usr/bin/Rscript
library(ggplot2)

process <- function(file = "clean_data.csv") {
  table <- read.csv(file, header=TRUE)
  table
}

base_graph <- function(df,
                       col=1,
                       title="Titulo",
                       xlab="Eixo X",
                       ylab="Contagem",
                       conf=labs()) {
  df <- data.frame(sort(df[,col]))
  names(df) <- c("V1")
  ggplot(df, aes(V1, ..count..)) +
    conf +
    labs(title = title) +
    labs(x = xlab) +
    labs(y = ylab)
}

bar_graph <- function(df,
                     col=1,
                     title="Titulo",
                     xlab="Eixo X",
                     ylab="Contagem",
                     conf=labs(),

                     text_offset=2) {
  base_graph(df, col, title, xlab, ylab, conf) +
    geom_bar() +
    geom_text(
        aes(label=..count.., y=..count..+text_offset),
        stat="count",
        position = position_dodge(0.9)
    )
}

bar_summary_graph <- function(df,
                     col=1,
                     title="Titulo",
                     xlab="Eixo X",
                     ylab="Contagem",
                     conf=labs(),
                     text_offset=2) {
  aux <- data.frame(sort(df[,col]))
  names(aux) <- c("V1")
  median <- median(aux$V1)
  mean <- mean(aux$V1)
  bar_graph(df, col, title, xlab, ylab, conf, text_offset) +
    geom_vline(xintercept=median, colour="red") + 
    geom_vline(xintercept=mean, colour="blue")
}

box_graph <- function(df,
                       col=1,
                       title="Titulo",
                       xlab="Eixo X",
                       ylab="Eixo Y",
                       conf=labs()) {
  df <- data.frame(sort(df[,col]))
  names(df) <- c("V1")
  ggplot(df) +
    conf +
    labs(title = title) +
    labs(x = xlab) +
    labs(y = ylab) +
    scale_x_continuous(limits=c(-2,2)) +
    geom_boxplot(aes(y=V1)) +
    theme(axis.text.x=element_blank())
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
    ""
)

bar_graph(
    t,
    6,
    "Tem alguma prática espiritual, filosófica ou religião",
    ""
)

bar_summary_graph(
    t,
    8,
    "Tempo médio de uso diário do smartphone",
    "Número de horas",
    text_offset=1,
)

box_graph(
    t,
    9,
    "Disciplinas cursadas",
    "",
    "Quantidade"
)

bar_graph(
    t,
    10,
    "Já pensou em desistir do curso",
    ""
)

bar_summary_graph(
    t,
    11,
    "Satisfação com a estrutura da UFPel",
    "Nível",
    text_offset=1,
    conf=scale_x_continuous(
        breaks=seq(0, 10, 2)
    )
)
