install.packages("dplyr")
install.packages("ggplot2")
install.packages("tidyverse")
install.packages("readxl")

library(dplyr)
library(ggplot2)
library(tidyverse)
library(readxl)

analisebd <- read_xlsx("C:/Users/ander/Desktop/Base Diaria.xlsx", sheet = "base_exec") # nolint

uo_data <- data.frame(
  COD = c(101, 201, 202, 500, 501, 600, 604, 613, 614, 800, 805, 1000,
          1010, 1011, 1013, 1014, 1018, 1019, 1022, 1023, 2000, 2100, 2200,
          2300, 2301, 2302, 2304, 2400, 2500, 2503, 2504, 2505, 2700, 2702,
          2703, 2704, 2705, 2706, 2708, 2711, 2800, 2805, 2806, 2808,
          2809, 2810, 3000, 3100, 3101, 3102, 3103, 3200, 3300, 3301, 3302,
          3303, 3304, 4001, 4002),
  SIGLA = c('CM', 'GP', 'GVP', 'PGM', 'FPGM', 'SMPOG', 'PRODABEL', 'FUFIN', 'BHPREV',# nolint
            'SMFA', 'FMAATM', 'SMASAC', 'FUMUSAN', 'FMAS', 'FMDCA', 'FMAE', 'FMI', 'FMPDM', 'FMTE',# nolint
            'FMDM', 'SMGO', 'SMSP', 'SMED', 'SMSA', 'HMOB', 'FMS', 'FUMSD', 'CTGM', 'SMMA',# nolint
            'FDA', 'FPQMAMA', 'FPMZB', 'SMOBI', 'SUDECAP', 'URBEL', 'FMHP', 'FS', 'FOUBHM','SLU',# nolint
            'FUMPDEC', 'SMDE', 'BELOTUR', 'FUMDEBH', 'FUMTUR', 'FMPDC', 'FMT',# nolint
            'SMEL', 'SMC', 'FPC', 'FPPC-BH', 'FMC', 'SMAICS', 'SMPU', 'FC', 'SUMOB', 'FMU', 'BHTRANS',  # nolint
            'EGM-SMFA', 'EGM-SMPOG') # nolint
)


#mostra a data de atualização da base diária
dataAtual_basediaria <- analisebd[2, 29] # nolint
print(dataAtual_basediaria)

# Criar uma nova coluna unindo COD e SIGLA
uo_data <- uo_data %>%
  mutate(COD_SIGLA = paste(COD, SIGLA, sep = " - "))

# Verificar colunas de analisebd
print(colnames(analisebd))

# Verificar colunas de uo_data
print(colnames(uo_data))

# Faça o merge para criar uma coluna COD_SIGLA correspondente
analisebd <- analisebd %>%
  left_join(uo_data %>% select(COD, COD_SIGLA), by = c("UO" = "COD"))

# Substitua a coluna UO por COD_UO
analisebd <- analisebd %>%
  rename(COD_UO = COD_SIGLA) %>%
  select(-UO)

# Verifique o resultado
print(colnames(analisebd))
print(head(analisebd))

# Calcular o percentual de VL_EMPENHADO em relação a VL_COTA e consolidar por COD_UO # nolint
consulta1 <- analisebd %>%
  filter(COD_UO %in% c('3100 - SMC', '3101 - FPC', '3102 - FPPC-BH', '3103 - FMC')) %>% #retirando o filtro, retorna todos os resultados
  group_by(COD_UO) %>%
  summarise(
    VL_EMPENHADO = sum(VL_EMPENHADO, na.rm = TRUE),
    VL_COTA = sum(VL_COTA, na.rm = TRUE),
    PERC_EXECUTADO = (sum(VL_EMPENHADO, na.rm = TRUE) / sum(VL_COTA, na.rm = TRUE)) * 100 # nolint
  )

print(consulta1)

# Criar o gráfico
ggplot(consulta1, aes(x = MES, y = Valor, color = Tipo)) +
  geom_line() +
  facet_wrap(~COD_UO) +
  labs(
    title = "VL_EMPENHADO e VL_COTA por Mês",
    x = "Mês",
    y = "Valor",
    color = "Tipo"
  ) +
  theme_minimal()

 # nolint