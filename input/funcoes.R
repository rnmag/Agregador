library(rstan)
library(lubridate)
library(ragg)
library(tidyverse)


# Banco de dados e parâmetros -------------------------------------------------

pesquisas <- read_delim("input/pesquisas_agregador.csv",
                        delim = ";",
                        locale = locale(decimal_mark = ",",
                                        encoding = "WINDOWS-1252")) |>
  mutate(candidato = case_when(candidato == "João Doria" ~ "Doria",
                               candidato == "Sergio Moro" ~ "Moro",
                               candidato == "Simone Tebet" ~ "Tebet",
                               TRUE ~ candidato),
         instituto_id = as.integer(as.factor(instituto)),
         pesquisa_id = paste(instituto, "-", dia),
         percentual = as.numeric(percentual) / 100,
         margem_candidato = 2 * 1.96 * sqrt(percentual * (1 - percentual) / qtd_entrevistas),
         dia = dmy(dia)) |>
  filter(dia >= floor_date(today() - years(1), "month"),
         ambito == "Brasil",
         tipo == "estimulada",
         tipo_voto == "votos totais",
         cargo == "presidente",
         percentual > 0) |>
  # t_pesquisa indica o tempo decorrido (em dias) desde a primeira pesquisa.
  # Assim, t_pesquisa = 1 é o dia da primeira pesquisa na análise. Por isso
  # é criado depois do filtro de data.
  group_by(candidato) |>
  mutate(t_pesquisa = as.integer(dia - min(dia) + 1),
         metodologia = case_when(instituto %in% c("FSB", "Ponteio", "Ideia Big Data", "PoderData", "Ipespe", "Futura", "Ranking", "Gerp") ~ "Telefônica",
                                 instituto %in% c("Sensus", "MDA", "Quaest", "Ipec", "Paraná Pesquisas", "Vox Populi", "Datafolha", "Datatempo") ~ "Presencial",
                                 instituto %in% c("Atlas") ~ "Online",
                                 TRUE ~ NA_character_)) |>
  ungroup() |>
  select(pesquisa_id,
         dia,
         instituto,
         turno,
         candidato,
         sigla_partido,
         voto_nao_valido,
         percentual,
         ambito,
         cargo,
         tipo,
         tipo_voto,
         margem_pesquisa,
         qtd_entrevistas,
         metodologia,
         instituto_id,
         margem_candidato,
         t_pesquisa) |>
  arrange(dia, turno)

# Candidatos que vão aparecer nos gráficos
candidatos_1t <- c("Lula",
                   "Ciro",
                   "Tebet",
                   "Bolsonaro")

candidatos_2t <- c("Lula",
                   "Bolsonaro")



# Definição de funções --------------------------------------------------------

rodar_modelo <- function(bd, arquivo_stan) {

  # Simulações
  m <- stan(arquivo_stan,
            model_name = "Agregador Ponteio",
            data = list(total_dias = max(bd$t_pesquisa),
                        n_pesquisas = nrow(bd),
                        percentual = bd$percentual,
                        n_dias = bd$t_pesquisa,
                        sigma = bd$margem_candidato,
                        instituto_id = bd$instituto_id,
                        n_institutos = length(unique(bd$instituto_id))),
            cores = parallel::detectCores(),
            chains = 1,
            warmup = 500,
            iter = 2000)

  # Extração dos resultados
  mu <- rstan::extract(m)$mu |>
    # O modelo do Stan retorna uma matriz que tem as simulações nas linhas e os
    # dias nas colunas. A ideia aqui é calcular, para cada coluna (dia), a simu-
    # lação mediana, assim como os limites para o intervalo de credibilidade
    as_tibble() |>
    summarize(across(everything(),
                     list(li = function(x) quantile(x, .025),
                          mediana = median,
                          ls = function(x) quantile(x, .975)))) |>
    pivot_longer(everything(),
                 names_to = c("t", ".value"),
                 names_sep = "_") |>
    mutate(dia = seq.Date(min(bd$dia), max(bd$dia), by = "day"),
           percentual_estimado = scales::label_percent(1)(mediana)) |>
    select(dia, percentual_estimado, li, mediana, ls) |>
    left_join(bd, by = "dia") |>
    fill(metodologia) |>
    arrange(dia)

  rm(m)

  return(mu)
}



agregador_ponteio <- function(turno) {

  set.seed(1234)

  if (turno == 1) {

    cat("\nIniciando as simulações do primeiro turno...\n")

    resultado_agregador_1t <- pesquisas |>
      filter(turno == 1 & candidato %in% candidatos_1t) |>
      group_by(candidato) |>
      nest() |>
      mutate(modelos = map(data, rodar_modelo, arquivo_stan = "input/modelo.stan")) |>
      unnest(modelos) |>
      ungroup() |>
      select(-data)

    save(resultado_agregador_1t, file = "output/resultados_1t.RData")

    return(resultado_agregador_1t)

  } else if (turno == 2) {

    cat("\nIniciando as simulações do segundo turno...\n")

    resultado_agregador_2t <- pesquisas |>
      filter(turno == 2 & candidato %in% candidatos_2t) |>
      group_by(candidato) |>
      nest() |>
      mutate(modelos = map(data, rodar_modelo, arquivo_stan = "input/modelo.stan")) |>
      unnest(modelos) |>
      ungroup() |>
      select(-data)

    save(resultado_agregador_2t, file = "output/resultados_2t.RData")

    return(resultado_agregador_2t)

  } else {
    stop("Obrigatório definir 1 ou 2 turno")
  }
}



grafico_agregador <- function(bd, turno) {

  if (turno == 1) {
    bd <- resultado_agregador_1t
    grafico_out <- "output/agregador_1t.png"
    cores <- c("Lula" = "#CF4446",
               "Bolsonaro" = "#446AAF",
               "Ciro" = "#35B779",
               "Tebet" = "#F7D13D")
    titulo_grafico <- "Agregador de Pesquisas - 1º Turno"

  } else if (turno == 2) {
    bd <- resultado_agregador_2t
    grafico_out <- "output/agregador_2t.png"
    cores <- c("Lula" = "#CF4446",
               "Bolsonaro" = "#446AAF")
    titulo_grafico <- "Agregador de Pesquisas - 2º Turno"

  } else {
    stop("Obrigatório definir 1 ou 2 turno")
  }

  p <- bd |>
    ggplot(aes(x = dia,
               y = mediana)) +
    # Definição dos geoms
    geom_line(aes(color = candidato),
              lineend = "round",
              size = 1.5) +
    geom_ribbon(aes(ymin = li,
                    ymax = ls,
                    fill = candidato),
                alpha = 0.1) +
    geom_point(aes(y = percentual,
                   color = candidato,
                   shape = metodologia),
               size = 2,
               alpha = .5) +
    geom_text(data = bd |>
                     group_by(candidato) |>
                     slice_max(dia, n = 1, with_ties = FALSE),
              aes(label = percentual_estimado,
                  color = candidato),
              size = 6,
              family = "Fira Sans",
              fontface = "bold",
              hjust = -.25,
              show.legend = FALSE) +
    coord_cartesian(clip = "off") + # percentuais podem passar a borda do gráfico
    # Legendas
    scale_fill_manual(values = cores) +
    scale_colour_manual(values = cores) +
    scale_shape_manual(values = c(3, 1, 19)) +
    guides(color = guide_legend(order = 1),
           fill = guide_legend(order = 1),
           shape = guide_legend(order = 2)) +
    # Eixos
    scale_y_continuous(expand = expansion(mult = c(.02, .02)),
                       labels = scales::label_percent(1)) +
    scale_x_date(expand = expansion(mult = c(.025, 0)),
                 limits = c(min(bd$dia), max(bd$dia)),
                 date_breaks = "1 month",
                 labels = function(x) {
                  if_else(is.na(lag(x)) | year(lag(x)) != year(x),
                          paste0(str_to_title(month(x, label = TRUE)), "\n", year(x)),
                          paste0(str_to_title(month(x, label = TRUE))))
                                      }) +
    # Acabamento
    theme_bw() +
    labs(title = titulo_grafico,
         subtitle = "ponteiopolitica.com",
         caption = paste("Estimativas baseadas em",
                         length(unique(bd$pesquisa_id)),
                         "pesquisas publicadas entre",
                         str_to_title(format(min(bd$dia), "%B/%y")),
                         "e",
                         str_to_title(format(max(bd$dia), "%B/%y")),
                         "\nInstitutos:",
                         paste(unique(sort(bd$instituto)), collapse = ", ")),
         color = "Candidatos",
         fill = "Candidatos",
         shape = "Metodologia") +
    theme(text = element_text(family = "Fira Sans"),
          plot.title = element_text(face = "bold", size = 18, hjust = .5),
          plot.subtitle = element_text(face = "italic", hjust = .5, color = "#777777"),
          plot.caption = element_text(size = 7, color = "#777777"),
          plot.margin = margin(.1, 1.5, .1, .1, "cm"),
          panel.grid.minor = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.border = element_blank(),
          axis.line.x.bottom = element_line(color = "black", size = 1),
          axis.title = element_blank(),
          axis.ticks = element_blank(),
          axis.text.x = element_text(size = 11),
          axis.text.y = element_text(size = 11),
          legend.title = element_text(face = "bold", size = 9),
          legend.text = element_text(size = 9),
          legend.position = "top",
          legend.box = "vertical",
          legend.spacing.y = unit(-10, "pt"),
          legend.background = element_rect(fill = "transparent"))

  ggsave(grafico_out,
         p,
         device = agg_png,
         dpi = "retina",
         width = 9.12,
         height = 5.98,
         units = "in")

  print(p)

}
