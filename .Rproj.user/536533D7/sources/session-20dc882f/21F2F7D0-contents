library(basedosdados)
library(tidyverse)

set_billing_id("flamengadas")

# Obrigado, Base dos Dados, me poupou de um web scrapping
# Para carregar o dado direto no R
df <- basedosdados::read_sql("SELECT ano_campeonato, data, rodada, time_man, time_vis, colocacao_man, colocacao_vis, gols_man, gols_vis
                             from basedosdados.mundo_transfermarkt_competicoes.brasileirao_serie_a
                             WHERE ano_campeonato > 2020")

str(df)

# Pontuação a cada rodada
pontuacao <- df %>%
  filter(colocacao_man == 1 | colocacao_vis == 1 |
           time_man == 'Flamengo' | time_vis == 'Flamengo') %>%
  mutate(
    Flamengo = case_when(
      time_man == 'Flamengo' ~ case_when(
        gols_man > gols_vis ~ 3, # Vitória
        gols_man < gols_vis ~ 0, # Derrota
        gols_man == gols_vis ~ 1 # Empate
      ),
      time_vis == 'Flamengo' ~ case_when(
        gols_man > gols_vis ~ 0, # Derrota
        gols_man < gols_vis ~ 3, # Vitória
        gols_man == gols_vis ~ 1 # Empate
      )
    ),
    Lider = case_when(
      colocacao_man == 1 ~ case_when(
        gols_man > gols_vis ~ 3, # Vitória
        gols_man < gols_vis ~ 0, # Derrota
        gols_man == gols_vis ~ 1 # Empate
      ),
      colocacao_vis == 1 ~ case_when(
        gols_man > gols_vis ~ 0, # Derrota
        gols_man < gols_vis ~ 3, # Vitória
        gols_man == gols_vis ~ 1 # Empate
      )
    ),
    pos_fla = case_when(
      time_man == 'Flamengo' ~ colocacao_man,
      time_vis == 'Flamengo' ~ colocacao_vis,
      time_vis != 'Flamengo' & time_man != 'Flamengo' ~ 0)
  ) %>%
  group_by(ano_campeonato, rodada) %>%
  summarise(Flamengo = na.omit(Flamengo),
            Lider = na.omit(Lider),
            pos_fla = max(pos_fla))

# Gráfico por resultado
pontuacao %>%
  pivot_longer(Flamengo:Lider) %>%
  mutate(Resultado = case_when(
    value == 0 ~ 'D',
    value == 1 ~ 'E',
    value == 3 ~ 'V'
  )) %>%
  mutate(value = value + 0.3) %>%
  mutate(value = ifelse(name == 'Lider', -1, 1) * value) %>%
  ggplot(aes(x = as.integer(rodada), y = value,
             group = name, fill = Resultado)) + 
  geom_col(position = 'stack') +
  geom_hline(yintercept = 0) +
  facet_wrap(~ano_campeonato, nrow = 3)

# Tabela auxiliar consolidando os resultados nos termos mais convenientes            
Auxiliar <- pontuacao %>%
  mutate(resultados = case_when(
    Flamengo == 3 & Lider == 3 ~ 'Líder ganha, Flamengo ganha',
    Flamengo == 3 & Lider != 3 ~ 'Líder tropeça, Flamengo ganha',
    Flamengo != 3 & Lider != 3 ~ 'Líder tropeça, Flamengo tropeça',
    Flamengo != 3 & Lider == 3 ~ 'Líder ganha, Flamengo tropeça'
  )) %>% 
  mutate(Tropecos = case_when(
    Lider == 3 ~ 'Sem tropeço do líder',
    Lider != 3 & Flamengo == 3 ~ 'Flamengo aproveita tropeço',
    Lider != 3 & Flamengo != 3 ~ 'Flamengo desperdiça tropeço'
    )) %>%
  select(-c(Flamengo, Lider))

# Percentual de aproveitamento dos tropeços
estatistica <- Auxiliar %>%
  filter(Tropecos != 'Sem tropeço do líder') %>%
  {table(.$Tropecos) / nrow(.)}
estatistica # LÁGRIMAS

# Vamos lá...
pontuacao %>%
  pivot_longer(Flamengo:Lider) %>%
  left_join(Auxiliar) %>% # Juntando com a tabela auxiliar, com as
  # categorias relevantes
  mutate(value = value + 0.3) %>% # Dei um shift de .3 só pra derrota aparecer
  mutate(value = ifelse(name == 'Lider', -1, 1) * value) %>%
  # Inverte eixo do líder
  ggplot(aes(x = as.integer(rodada), y = value,
             group = name, fill = Tropecos)) + 
  geom_hline(yintercept = c(c(-3,-1,0) - .3, c(0, 1, 3) + .3),
             linewidth = .5, alpha = .3, color = 'grey30') +
  geom_col(position = 'stack') +
  facet_wrap(~ano_campeonato, nrow = 3) +
  scale_y_continuous(breaks=c(c(-3,-1,0) - .3, c(0, 1, 3) + .3),
                   labels=c(paste0('Líder ',
                                   c('Ganha', 'Empata', 'Perde')),
                            paste0('Flamengo ',
                                   c('Perde', 'Empata', 'Ganha')))) +
  scale_x_continuous(breaks = 1:38) +
  scale_fill_manual(values = c("Flamengo aproveita tropeço" = 'red2',
                               "Flamengo desperdiça tropeço" = 'red4',
                               'Sem tropeço do líder' = 'gray50')) +
  labs(y = '', fill = '', x = 'Rodada',
       title = 'Um time que não briga por porra nenhuma',
       subtitle = paste0('Flamengo desperdiçou ', round(estatistica[2]*100, 1),
                         '% das chances de se aproximar do líder desde 2021'),
       caption = 'github.com/johannmarques') +
  theme_minimal() +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        strip.text = element_text(size = 12, face = 'bold'),
        text=element_text(family="lato"),
        legend.position = 'bottom',
        plot.background = element_rect(fill = 'white'))

ggsave('3AnosSemBrasileiro.png')
