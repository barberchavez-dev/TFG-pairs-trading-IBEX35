
# TFG - Estudio de estrategias de pairs trading en el IBEX 35 mediante análisis de correlación y cointegración.
# Autor: Lisa Estela Barber Chavez

# 1. LIBRERÍAS

library(knitr)
library(tidyverse)
library(quantmod)
library(PerformanceAnalytics)
library(corrplot)
library(xts)
library(lubridate)
library(readr)
library(dplyr)
library(zoo)
library(purrr)
library(tseries)
library(ggplot2)
library(tidyr)

# 2. CARGA DE DATOS

ACS   <- read_csv("data/ACS.csv")
ACX   <- read_csv("data/ACX.csv")
AENA  <- read_csv("data/AENA.csv")
AMS   <- read_csv("data/AMS.csv")
ANA   <- read_csv("data/ANA.csv")
ANE   <- read_csv("data/ANE.csv")
BBVA  <- read_csv("data/BBVA.csv")
BKT   <- read_csv("data/BKT.csv")
CABK  <- read_csv("data/CABK.csv")
CLNX  <- read_csv("data/CLNX.csv")
COL   <- read_csv("data/COL.csv")
ELE   <- read_csv("data/ELE.csv")
ENG   <- read_csv("data/ENG.csv")
FDR   <- read_csv("data/FDR.csv")
FER   <- read_csv("data/FER.csv")
GRF   <- read_csv("data/GRF.csv")
IBE   <- read_csv("data/IBE.csv")
IAG   <- read_csv("data/IAG.csv")
IDR   <- read_csv("data/IDR.csv")
ITX   <- read_csv("data/ITX.csv")
LOG   <- read_csv("data/LOG.csv")
MAP   <- read_csv("data/MAP.csv")
MRL   <- read_csv("data/MRL.csv")
MT    <- read_csv("data/MT.csv")
NTGY  <- read_csv("data/NTGY.csv")
PUIGb <- read_csv("data/PUIGb.csv")
REDE  <- read_csv("data/REDE.csv")
REP   <- read_csv("data/REP.csv")
ROVI  <- read_csv("data/ROVI.csv")
SAB   <- read_csv("data/SAB.csv")
SAN   <- read_csv("data/SAN.csv")
SCYR  <- read_csv("data/SCYR.csv")
SLR   <- read_csv("data/SLR.csv")
TEF   <- read_csv("data/TEF.csv")
UNI   <- read_csv("data/UNI.csv")


# 3. LISTA CONJUNTA

datos_IBEX <- list(
  ACS, ACX, AENA, AMS, ANA, ANE, BBVA, BKT, CABK, CLNX,
  COL, ELE, ENG, FDR, FER, GRF, IBE, IAG, IDR, ITX,
  LOG, MAP, MRL, MT, NTGY, PUIGb, REDE, REP, ROVI,
  SAB, SAN, SCYR, SLR, TEF, UNI
)


# 4. REVISIÓN INICIAL 

sapply(datos_IBEX, nrow)   # número de observaciones
lapply(datos_IBEX, names)  # nombre de columnas
str(datos_IBEX[[1]])       # estructura de un dataset


# 5. HOMOGENEIZACIÓN DE NOMBRES DE COLUMNAS

datos_IBEX <- lapply(datos_IBEX, function(x) {
  names(x) <- c("Fecha", "Último", "Apertura", "Máximo", "Mínimo", "Vol.", "% var.")
  x
})


# 6. CONVERSIÓN DE LA VARIABLE FECHA

datos_IBEX <- lapply(datos_IBEX, function(x) {
  if (grepl("\\.", x$Fecha[1])) {
    x$Fecha <- as.Date(x$Fecha, format = "%d.%m.%Y")
  } else if (grepl("/", x$Fecha[1])) {
    x$Fecha <- as.Date(x$Fecha, format = "%m/%d/%Y")
  }
  x
})

str(datos_IBEX[[1]]$Fecha)  # verificación


# 7. CONVERSIÓN DE VARIABLES NUMÉRICAS

datos_IBEX <- lapply(datos_IBEX, function(x) {
  x$Último   <- as.numeric(gsub(",", ".", gsub("\\.", "", x$Último)))
  x$Apertura <- as.numeric(gsub(",", ".", gsub("\\.", "", x$Apertura)))
  x$Máximo   <- as.numeric(gsub(",", ".", gsub("\\.", "", x$Máximo)))
  x$Mínimo   <- as.numeric(gsub(",", ".", gsub("\\.", "", x$Mínimo)))
  x
})


# 8. ORDENACIÓN CRONOLÓGICA

datos_IBEX <- lapply(datos_IBEX, function(x) {
  x <- x[order(x$Fecha), ]
  x
})


# 9. CÁLCULO DE RENTABILIDADES LOGARÍTMICAS

datos_IBEX <- lapply(datos_IBEX, function(x) {
  x$rendimiento <- c(NA, diff(log(x$Último)))
  x
})

head(datos_IBEX[[1]])


# 10. CONSTRUCCIÓN DE LA BASE DE CORRELACIONES

tickers <- c(
  "ACS","ACX","AENA","AMS","ANA","ANE","BBVA","BKT","CABK","CLNX",
  "COL","ELE","ENG","FDR","FER","GRF","IBE","IAG","IDR","ITX",
  "LOG","MAP","MRL","MT","NTGY","PUIGb","REDE","REP","ROVI",
  "SAB","SAN","SCYR","SLR","TEF","UNI"
)

rentabilidades_IBEX <- Map(function(x, nombre) {
  df <- x[, c("Fecha", "rendimiento")]
  names(df)[2] <- nombre
  df
}, datos_IBEX, tickers)

base_correlaciones <- Reduce(function(x, y) {
  full_join(x, y, by = "Fecha")
}, rentabilidades_IBEX)

base_correlaciones <- base_correlaciones %>%
  arrange(Fecha)

head(base_correlaciones)


# 11. MATRIZ DE CORRELACIONES

matriz_rendimientos <- base_correlaciones %>%
  select(-Fecha)

matriz_correlacion <- cor(matriz_rendimientos,
                          use = "pairwise.complete.obs",
                          method = "pearson")

round(matriz_correlacion, 2)

corrplot(matriz_correlacion,
         method = "circle",
         type = "upper",
         order = "hclust",
         addrect = 4,
         tl.col = "black",
         tl.cex = 0.6,
         cl.cex = 0.7,
         diag = FALSE)


# 12. IDENTIFICACIÓN DE PARES CON ALTA CORRELACIÓN

cor_df <- as.data.frame(as.table(matriz_correlacion))
colnames(cor_df) <- c("Activo1", "Activo2", "Correlacion")

cor_df <- cor_df %>%
  filter(Activo1 != Activo2) %>%
  rowwise() %>%
  mutate(par = paste(sort(c(Activo1, Activo2)), collapse = "-")) %>%
  distinct(par, .keep_all = TRUE) %>%
  select(-par)

top_pares <- cor_df %>%
  arrange(desc(Correlacion)) %>%
  head(10)

top_pares


# 13. SELECCIÓN DE 5 PARES 

pares <- list(
  c("SAB",  "CABK"),
  c("CABK", "BKT"),
  c("REDE", "ENG"),
  c("REDE", "ELE"),
  c("MRL",  "COL")
)


# 14. GRÁFICOS DE RENTABILIDADES POR PAR

for (par in pares) {
  nombre1 <- par[1]
  nombre2 <- par[2]

  datos_par <- base_correlaciones %>%
    select(Fecha, all_of(c(nombre1, nombre2))) %>%
    pivot_longer(-Fecha, names_to = "Activo", values_to = "Rendimiento")

  print(
    ggplot(datos_par, aes(x = Fecha, y = Rendimiento, color = Activo)) +
      geom_line() +
      theme_minimal() +
      labs(title = paste("Rendimientos:", nombre1, "vs", nombre2))
  )
}

# Zoom 2020: REDE vs ENG
zoom_2020 <- base_correlaciones %>%
  filter(Fecha >= "2020-01-01" & Fecha <= "2020-12-31") %>%
  select(Fecha, REDE, ENG) %>%
  pivot_longer(-Fecha, names_to = "Activo", values_to = "Rendimiento")

ggplot(zoom_2020, aes(x = Fecha, y = Rendimiento, color = Activo)) +
  geom_line(alpha = 0.8) +
  theme_minimal() +
  labs(title = "Zoom 2020: Rendimientos REDE vs ENG", x = "Fecha", y = "Rendimiento")

# Zoom 2024: SAB vs CABK (periodo OPA BBVA)
zoom_opa <- base_correlaciones %>%
  filter(Fecha >= "2024-01-01" & Fecha <= "2024-12-31") %>%
  select(Fecha, SAB, CABK) %>%
  pivot_longer(-Fecha, names_to = "Activo", values_to = "Rendimiento")

ggplot(zoom_opa, aes(x = Fecha, y = Rendimiento, color = Activo)) +
  geom_line(alpha = 0.8) +
  theme_minimal() +
  labs(title = "Zoom 2024: Rendimientos SAB vs CABK (periodo OPA BBVA)", x = "Fecha", y = "Rendimiento")

# Zoom 2020: MRL vs COL
zoom_mrl_col <- base_correlaciones %>%
  filter(Fecha >= "2020-01-01" & Fecha <= "2020-12-31") %>%
  select(Fecha, MRL, COL) %>%
  pivot_longer(-Fecha, names_to = "Activo", values_to = "Rendimiento")

ggplot(zoom_mrl_col, aes(x = Fecha, y = Rendimiento, color = Activo)) +
  geom_line(alpha = 0.8) +
  theme_minimal() +
  labs(title = "Zoom 2020: Rendimientos MRL vs COL", x = "Fecha", y = "Rendimiento")


# 15. SPREADS SIMPLES 

for (par in pares) {
  nombre1 <- par[1]
  nombre2 <- par[2]

  spread_df <- base_correlaciones %>%
    mutate(spread = .data[[nombre1]] - .data[[nombre2]])

  print(
    ggplot(spread_df, aes(x = Fecha, y = spread)) +
      geom_line(color = "blue") +
      theme_minimal() +
      labs(title = paste("Spread:", nombre1, "-", nombre2))
  )
}


# 16. CORRELACIÓN MÓVIL

pares_sel <- list(
  c("SAB",  "CABK"),
  c("CABK", "BKT"),
  c("REDE", "ENG"),
  c("REDE", "ELE"),
  c("MRL",  "COL")
)

ventanas <- c(10, 30, 60)

for (v in ventanas) {
  for (par in pares_sel) {
    activo1 <- par[1]
    activo2 <- par[2]

    datos_par <- base_correlaciones %>%
      select(Fecha, all_of(c(activo1, activo2))) %>%
      drop_na()

    correlacion_movil <- tibble(
      Fecha = datos_par$Fecha,
      CorrMovil = rollapply(
        data = datos_par[, c(activo1, activo2)],
        width = v,
        FUN = function(z) cor(z[,1], z[,2], use = "complete.obs"),
        by.column = FALSE,
        fill = NA,
        align = "right"
      )
    )

    print(
      ggplot(correlacion_movil, aes(x = Fecha, y = CorrMovil)) +
        geom_line() +
        theme_minimal() +
        labs(
          title = paste0("Correlación móvil (ventana=", v, " días): ", activo1, " vs ", activo2),
          x = "Fecha", y = "Correlación"
        )
    )
  }
}


# 17. COINTEGRACIÓN: TEST ADF SOBRE RESIDUOS

names(datos_IBEX) <- c(
  "ACS","ACX","AENA","AMS","ANA","ANE","BBVA","BKT","CABK","CLNX",
  "COL","ELE","ENG","FDR","FER","GRF","IBE","IAG","IDR","ITX",
  "LOG","MAP","MRL","MT","NTGY","PUIGb","REDE","REP","ROVI",
  "SAB","SAN","SCYR","SLR","TEF","UNI"
)

tickers <- names(datos_IBEX)

precios_cierre <- datos_IBEX[[tickers[1]]] %>%
  select(Fecha, Último) %>%
  rename(!!tickers[1] := Último)

for (t in tickers[-1]) {
  df <- datos_IBEX[[t]] %>%
    select(Fecha, Último) %>%
    rename(!!t := Último)
  precios_cierre <- full_join(precios_cierre, df, by = "Fecha")
}

precios_cierre <- precios_cierre %>%
  arrange(Fecha)

# Función test de cointegración
test_cointegracion <- function(df_precios, activo1, activo2) {
  datos <- df_precios %>%
    select(Fecha, all_of(c(activo1, activo2))) %>%
    drop_na()

  modelo  <- lm(datos[[activo1]] ~ datos[[activo2]])
  residuos <- resid(modelo)
  adf     <- adf.test(residuos)

  tibble(
    Activo1      = activo1,
    Activo2      = activo2,
    Beta         = coef(modelo)[2],
    p_valor_adf  = adf$p.value
  )
}

resultados_cointegracion <- bind_rows(
  test_cointegracion(precios_cierre, "SAB",  "CABK"),
  test_cointegracion(precios_cierre, "CABK", "BKT"),
  test_cointegracion(precios_cierre, "REDE", "ENG"),
  test_cointegracion(precios_cierre, "REDE", "ELE"),
  test_cointegracion(precios_cierre, "MRL",  "COL")
)

resultados_cointegracion


# 18. SPREAD AJUSTADO Y Z-SCORE

spread_zscore <- function(df_precios, activo1, activo2) {
  datos <- df_precios %>%
    select(Fecha, all_of(c(activo1, activo2))) %>%
    drop_na()

  modelo <- lm(datos[[activo1]] ~ datos[[activo2]])
  beta   <- coef(modelo)[2]

  datos <- datos %>%
    mutate(spread = .data[[activo1]] - beta * .data[[activo2]]) %>%
    mutate(
      media_30 = rollmean(spread, 30, fill = NA, align = "right"),
      sd_30    = rollapply(spread, 30, sd, fill = NA, align = "right"),
      zscore   = (spread - media_30) / sd_30
    )

  return(list(datos = datos, beta = beta))
}

sab_cabk <- spread_zscore(base_correlaciones, "SAB",  "CABK")
cabk_bkt <- spread_zscore(base_correlaciones, "CABK", "BKT")
rede_eng <- spread_zscore(base_correlaciones, "REDE", "ENG")
rede_ele <- spread_zscore(base_correlaciones, "REDE", "ELE")
mrl_col  <- spread_zscore(base_correlaciones, "MRL",  "COL")

# Gráficos z-score
for (par in list(
  list(datos = sab_cabk$datos, nombre = "SAB - CABK"),
  list(datos = cabk_bkt$datos, nombre = "CABK - BKT"),
  list(datos = rede_eng$datos, nombre = "REDE - ENG"),
  list(datos = rede_ele$datos, nombre = "REDE - ELE"),
  list(datos = mrl_col$datos,  nombre = "MRL - COL")
)) {
  print(
    ggplot(par$datos, aes(x = Fecha, y = zscore)) +
      geom_line() +
      geom_hline(yintercept = c(-2, 0, 2), linetype = c("dashed", "solid", "dashed")) +
      theme_minimal() +
      labs(title = paste("Z-score del spread", par$nombre))
  )
}


# 19. BACKTESTING 

backtest_pairs <- function(df, entrada = 2, salida = 0.5) {
  df <- df %>%
    mutate(
      señal = case_when(
        zscore >  entrada  ~ -1,
        zscore < -entrada  ~  1,
        abs(zscore) < salida ~ 0,
        TRUE ~ NA_real_
      )
    )

  posicion <- numeric(nrow(df))
  for (i in 2:nrow(df)) {
    if (!is.na(df$señal[i])) {
      posicion[i] <- df$señal[i]
    } else {
      posicion[i] <- posicion[i - 1]
    }
  }

  df$posicion <- posicion

  df <- df %>%
    mutate(
      retorno_spread     = c(NA, diff(spread)),
      retorno_estrategia = lag(posicion) * retorno_spread,
      retorno_acum       = cumsum(replace_na(retorno_estrategia, 0))
    )

  return(df)
}

bt_sab_cabk <- backtest_pairs(sab_cabk$datos)
bt_cabk_bkt <- backtest_pairs(cabk_bkt$datos)
bt_rede_eng <- backtest_pairs(rede_eng$datos)
bt_rede_ele <- backtest_pairs(rede_ele$datos)
bt_mrl_col  <- backtest_pairs(mrl_col$datos)

# Gráficos rentabilidad acumulada
for (par in list(
  list(datos = bt_sab_cabk, nombre = "SAB - CABK"),
  list(datos = bt_cabk_bkt, nombre = "CABK - BKT"),
  list(datos = bt_rede_eng, nombre = "REDE - ENG"),
  list(datos = bt_rede_ele, nombre = "REDE - ELE"),
  list(datos = bt_mrl_col,  nombre = "MRL - COL")
)) {
  print(
    ggplot(par$datos, aes(x = Fecha, y = retorno_acum)) +
      geom_line() +
      theme_minimal() +
      labs(title = paste("Rentabilidad acumulada estrategia", par$nombre))
  )
}
