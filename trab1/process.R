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
    ## labs(title = "") +
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
    scale_x_continuous(limits=c(-3,3)) +
    geom_boxplot(aes(y=V1)) +
    theme(
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()
    )
}

print_confidence_interval <- function(vec,
                                      name
                                      ) {
  avg <- mean(vec)
  sdev <- sd(vec)
  n <- length(vec)
  error <- qt(0.975,df=n-1)*sdev/sqrt(n)

  print(sprintf("Intervalo para %s: [%.4f, %.4f]", name, avg-error, avg+error))
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
    "Pratica alguma atividade física",
    ""
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

## Groups

ggplot(t, aes(x=t[,1], group=t[,10], fill=t[,10])) +
  scale_fill_discrete(name="Já pensou em\ndesistir do curso") +
  labs(title = "Curso") +
  labs(x = "") +
  labs(y = "Contagem") +
  coord_flip() +
  geom_bar() +
  theme(legend.position = c(0.85, 0.9))

ggplot(t, aes(x=t[,10], group=t[,10], fill=t[,10])) +
  scale_fill_discrete(name="Já pensou em\ndesistir do curso") +
  labs(title = "Quantas disciplinas já cursou") +
  labs(x = "Já pensou em\ndesistir do curso") +
  labs(y = "Contagem") +
  geom_boxplot(
      aes(y=t[,9]),
      width=0.2
  ) +
  theme(
      legend.position = c(0.15, 0.9),
      axis.title.x=element_blank(),
      axis.text.x=element_blank(),
      axis.ticks.x=element_blank()
  )

ggplot(t, aes(x=t[,4], group=t[,4], fill=t[,4])) +
  scale_fill_discrete(name="Pratica alguma\natividade física") +
  labs(title = "Tempo médio de uso diário do smartphone") +
  labs(x = "") +
  labs(y = "Número de horas") +
  geom_boxplot(
      aes(y=t[,8]),
      width=0.2
  ) +
  theme(
      legend.position = c(0.15, 0.9),
      axis.title.x=element_blank(),
      axis.text.x=element_blank(),
      axis.ticks.x=element_blank()
  )

ggplot(t, aes(x=t[,3], group=t[,3], fill=t[,3])) +
  labs(title = "Satisfação com a estrutura da UFPel") +
  labs(x = "Consumo de álcool") +
  labs(y = "Nível") +
  scale_y_continuous(
      breaks=seq(0, 10, 2)
  ) +
  geom_boxplot(
      aes(y=t[,11]),
      width=0.4
  ) +
  guides(fill=FALSE)

print_confidence_interval(t[,8], "tempo médio de uso diário do smartphone em horas")
print_confidence_interval(t[,9], "quantas disciplinas já cursou")
print_confidence_interval(t[,11], "nível de satisfação com a infraestrutura da UFPel")

t.test(
    t[t[,1]=='Ciência da Computação',][,8],
    t[t[,1]=='Engenharia de Computação',][,8],
    alternative = "two.sided"
)

t.test(
    t[t[,1]=='Ciência da Computação',][,9],
    t[t[,1]=='Engenharia de Computação',][,9],
    alternative = "two.sided"
)

t.test(
    t[t[,1]=='Ciência da Computação',][,11],
    t[t[,1]=='Engenharia de Computação',][,11],
    alternative = "two.sided"
)

chisq.test(t[,10], t[,9])

chisq.test(t[,4], t[,8])
