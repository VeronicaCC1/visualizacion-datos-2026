# ============================================================
# CENTRO DE CONTROL DE MOVILIDAD URBANA — CIUDAD NOVA
# Réplica en R/Shiny del dashboard original en Python/Dash
# Diseñado para ejecutarse en Posit Cloud
# ============================================================

options(shiny.maxRequestSize = 100 * 1024^2)

paquetes <- c("shiny", "plotly", "dplyr", "tidyr", "DT", "htmltools", "lubridate")
faltantes <- paquetes[!paquetes %in% rownames(installed.packages())]
if (length(faltantes) > 0) {
  install.packages(faltantes, repos = "https://cloud.r-project.org")
}

library(shiny)
library(plotly)
library(dplyr)
library(tidyr)
library(DT)
library(htmltools)
library(lubridate)

set.seed(42)

N_REGISTROS <- 100000
FECHA_INICIO <- as.Date("2025-09-01")
FECHA_FIN <- as.Date("2026-08-31")

ZONAS <- c(
  "Centro", "Norte", "Sur", "Oriente", "Occidente",
  "Zona Industrial", "Zona Universitaria", "Zona Residencial"
)

TRANSPORTES <- c(
  "Automóvil", "Motocicleta", "Autobús", "Metro",
  "Bicicleta", "Taxi", "Caminata"
)

DIAS <- c("Lunes", "Martes", "Miércoles", "Jueves",
          "Viernes", "Sábado", "Domingo")

MESES <- c("Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio",
           "Julio", "Agosto", "Septiembre", "Octubre", "Noviembre", "Diciembre")

COLORES_TRANSPORTE <- c(
  "Automóvil" = "#4C78A8",
  "Motocicleta" = "#F58518",
  "Autobús" = "#54A24B",
  "Metro" = "#E45756",
  "Bicicleta" = "#72B7B2",
  "Taxi" = "#EECA3B",
  "Caminata" = "#B279A2"
)

# ============================================================
# GENERACIÓN DE DATOS SIMULADOS
# ============================================================

fechas <- seq(FECHA_INICIO, FECHA_FIN, by = "day")

prob_hora <- c(
  0.018, 0.012, 0.010, 0.010, 0.014, 0.030,
  0.055, 0.080, 0.070, 0.045, 0.040, 0.040,
  0.045, 0.042, 0.040, 0.045, 0.060, 0.085,
  0.095, 0.080, 0.060, 0.045, 0.035, 0.024
)
prob_hora <- prob_hora / sum(prob_hora)

df <- data.frame(
  fecha = sample(fechas, N_REGISTROS, replace = TRUE),
  hora = sample(0:23, N_REGISTROS, replace = TRUE, prob = prob_hora),
  zona_origen = sample(
    ZONAS, N_REGISTROS, replace = TRUE,
    prob = c(0.18, 0.13, 0.12, 0.11, 0.11, 0.10, 0.10, 0.15)
  ),
  zona_destino = sample(
    ZONAS, N_REGISTROS, replace = TRUE,
    prob = c(0.18, 0.13, 0.12, 0.11, 0.11, 0.10, 0.10, 0.15)
  ),
  tipo_transporte = sample(
    TRANSPORTES, N_REGISTROS, replace = TRUE,
    prob = c(0.27, 0.16, 0.18, 0.13, 0.08, 0.11, 0.07)
  ),
  stringsAsFactors = FALSE
)

# Evitar origen = destino
mismo_origen <- df$zona_origen == df$zona_destino
while (any(mismo_origen)) {
  idx <- which(mismo_origen)
  df$zona_destino[idx] <- sample(ZONAS, length(idx), replace = TRUE)
  mismo_origen <- df$zona_origen == df$zona_destino
}

df$dia_semana_num <- as.integer(format(df$fecha, "%u")) - 1
df$dia_semana <- DIAS[df$dia_semana_num + 1]
df$mes_num <- as.integer(format(df$fecha, "%m"))
df$mes <- MESES[df$mes_num]
df$mes_periodo <- format(df$fecha, "%Y-%m")

# Demanda por hora
efecto_hora <- c(
  `0` = 0.25, `1` = 0.18, `2` = 0.16, `3` = 0.15,
  `4` = 0.20, `5` = 0.35, `6` = 0.75, `7` = 1.30,
  `8` = 1.45, `9` = 0.90, `10` = 0.70, `11` = 0.72,
  `12` = 0.82, `13` = 0.78, `14` = 0.70, `15` = 0.76,
  `16` = 0.95, `17` = 1.25, `18` = 1.55, `19` = 1.42,
  `20` = 1.05, `21` = 0.78, `22` = 0.58, `23` = 0.40
)
df$factor_hora <- unname(efecto_hora[as.character(df$hora)])
df$es_fin_semana <- df$dia_semana_num >= 5
df$factor_dia <- ifelse(df$es_fin_semana, 0.72, 1.0)

factor_zona <- c(
  "Centro" = 1.25, "Norte" = 1.00, "Sur" = 1.05, "Oriente" = 0.98,
  "Occidente" = 0.95, "Zona Industrial" = 1.18,
  "Zona Universitaria" = 1.10, "Zona Residencial" = 0.90
)

congestion_zona <- c(
  "Centro" = 1.15, "Norte" = 0.92, "Sur" = 1.02, "Oriente" = 0.98,
  "Occidente" = 0.94, "Zona Industrial" = 1.28,
  "Zona Universitaria" = 1.08, "Zona Residencial" = 0.82
)

df$factor_zona <- unname(factor_zona[df$zona_origen])
df$factor_congestion_zona <- unname(congestion_zona[df$zona_origen])

base_demanda <- 2.2 * df$factor_hora * df$factor_dia * df$factor_zona

es_publico <- df$tipo_transporte %in% c("Autobús", "Metro")
base_demanda <- base_demanda + ifelse(es_publico, df$factor_hora * 0.9, 0)

dia_anual <- as.numeric(df$fecha - FECHA_INICIO)
estacionalidad <- 1 + 0.08 * sin(2 * pi * dia_anual / 365.25)
base_demanda <- base_demanda * estacionalidad

df$cantidad_viajes <- pmax(1, rpois(N_REGISTROS, pmin(pmax(base_demanda, 0.5), 20)))

# Distancias
centroides <- data.frame(
  zona = ZONAS,
  x = c(0.0, 2.8, -2.5, 3.4, -3.3, -4.5, 1.0, 2.1),
  y = c(0.0, 3.2, -3.0, -0.8, 0.7, -1.8, 2.5, -2.6)
)

x_origen <- centroides$x[match(df$zona_origen, centroides$zona)]
y_origen <- centroides$y[match(df$zona_origen, centroides$zona)]
x_destino <- centroides$x[match(df$zona_destino, centroides$zona)]
y_destino <- centroides$y[match(df$zona_destino, centroides$zona)]

distancia_base <- sqrt((x_destino - x_origen)^2 + (y_destino - y_origen)^2) * 1.65 + 1.2

df$distancia_km <- round(
  pmin(pmax(distancia_base * rlnorm(N_REGISTROS, 0, 0.16), 0.8), 25),
  2
)

# Índice de tráfico
pico <- 0.55 * df$factor_hora +
  0.35 * df$factor_congestion_zona +
  ifelse(df$es_fin_semana, -0.35, 0.20)

pico <- pico + ifelse(
  df$zona_origen == "Zona Industrial" & df$hora >= 6 & df$hora <= 18,
  0.55, 0
)

indice <- 42 + 19 * pico + rnorm(N_REGISTROS, 0, 8)

anomalia_1 <- df$fecha >= as.Date("2026-02-10") &
  df$fecha <= as.Date("2026-02-12") &
  df$hora >= 17 & df$hora <= 20

anomalia_2 <- df$fecha >= as.Date("2026-06-18") &
  df$fecha <= as.Date("2026-06-19") &
  df$hora >= 7 & df$hora <= 9

indice <- indice + ifelse(anomalia_1, 24, 0) + ifelse(anomalia_2, 18, 0)

df$indice_trafico <- round(pmin(pmax(indice, 5), 100), 2)

df$nivel_congestion <- cut(
  df$indice_trafico,
  breaks = c(-Inf, 35, 55, 75, Inf),
  labels = c("Baja", "Media", "Alta", "Crítica"),
  right = TRUE
)
df$nivel_congestion <- as.character(df$nivel_congestion)

# Velocidad
velocidad_base <- c(
  "Automóvil" = 46, "Motocicleta" = 50, "Autobús" = 31,
  "Metro" = 58, "Bicicleta" = 18, "Taxi" = 43, "Caminata" = 5.2
)
df$velocidad_base <- unname(velocidad_base[df$tipo_transporte])

reduccion_congestion <- ifelse(
  df$tipo_transporte %in% c("Automóvil", "Motocicleta", "Autobús", "Taxi"),
  0.48 * df$indice_trafico,
  ifelse(df$tipo_transporte == "Bicicleta",
         0.08 * df$indice_trafico, 0)
)

df$velocidad_promedio_kmh <- df$velocidad_base -
  reduccion_congestion + rnorm(N_REGISTROS, 0, 3.5)

df$velocidad_promedio_kmh <- round(
  pmin(pmax(df$velocidad_promedio_kmh, 3), 65), 2
)

# Duración
tiempo_base <- (df$distancia_km / df$velocidad_promedio_kmh) * 60

tiempo_extra <- ifelse(
  df$tipo_transporte %in% c("Automóvil", "Motocicleta", "Autobús", "Taxi"),
  df$indice_trafico * 0.08,
  df$indice_trafico * 0.015
)

df$duracion_minutos <- round(
  pmin(pmax(tiempo_base + tiempo_extra + rnorm(N_REGISTROS, 1.5, 2.0), 3), 120),
  2
)

# Accidentes e incidentes
riesgo <- 0.00035 * df$cantidad_viajes *
  (1 + df$indice_trafico / 65)

riesgo <- riesgo * ifelse(df$tipo_transporte == "Motocicleta", 1.65, 1.0)
riesgo <- riesgo * ifelse(df$tipo_transporte %in% c("Automóvil", "Taxi"), 1.20, 1.0)
riesgo <- pmin(pmax(riesgo, 0), 0.12)

df$accidentes <- rbinom(N_REGISTROS, 1, riesgo)

riesgo_incidente <- 0.012 +
  0.006 * (df$indice_trafico / 100) +
  0.002 * df$cantidad_viajes

riesgo_incidente <- riesgo_incidente *
  ifelse(df$zona_origen == "Zona Industrial", 1.30, 1.0)

riesgo_incidente <- pmin(pmax(riesgo_incidente, 0), 0.20)

df$incidentes <- rpois(N_REGISTROS, riesgo_incidente)

if (sum(anomalia_1) > 0) {
  df$incidentes[anomalia_1] <- df$incidentes[anomalia_1] +
    rbinom(sum(anomalia_1), 2, 0.35)
}
if (sum(anomalia_2) > 0) {
  df$incidentes[anomalia_2] <- df$incidentes[anomalia_2] +
    rbinom(sum(anomalia_2), 2, 0.25)
}

# Pasajeros
ocupacion <- c(
  "Automóvil" = 1.55, "Motocicleta" = 1.10, "Autobús" = 32,
  "Metro" = 120, "Bicicleta" = 1, "Taxi" = 1.65, "Caminata" = 1
)

df$pasajeros <- pmax(
  1,
  as.integer(round(
    df$cantidad_viajes * unname(ocupacion[df$tipo_transporte]) *
      rnorm(N_REGISTROS, 1, 0.12)
  ))
)

# Emisiones
emision_por_km <- c(
  "Automóvil" = 0.192, "Motocicleta" = 0.103, "Autobús" = 0.085,
  "Metro" = 0.035, "Bicicleta" = 0.0, "Taxi" = 0.210, "Caminata" = 0.0
)

df$emisiones_co2_kg <- df$distancia_km *
  df$cantidad_viajes *
  unname(emision_por_km[df$tipo_transporte]) *
  rnorm(N_REGISTROS, 1, 0.08)

df$emisiones_co2_kg <- round(pmax(df$emisiones_co2_kg, 0), 3)

# Costo
tarifa_base <- c(
  "Automóvil" = 4200, "Motocicleta" = 2800, "Autobús" = 3200,
  "Metro" = 3500, "Bicicleta" = 900, "Taxi" = 6500, "Caminata" = 0
)

incremento_km <- ifelse(
  df$tipo_transporte %in% c("Automóvil", "Taxi", "Motocicleta"),
  500, 180
)

df$costo_promedio <- unname(tarifa_base[df$tipo_transporte]) +
  df$distancia_km * incremento_km +
  rnorm(N_REGISTROS, 0, 500)

df$costo_promedio <- round(pmax(df$costo_promedio, 0), 0)

# Satisfacción
satisfaccion <- 86 -
  0.24 * df$indice_trafico -
  0.10 * df$duracion_minutos +
  ifelse(
    df$tipo_transporte %in% c("Metro", "Bicicleta", "Caminata"),
    5, 0
  ) +
  rnorm(N_REGISTROS, 0, 5)

df$satisfaccion_usuario <- round(
  pmin(pmax(satisfaccion, 25), 100), 1
)

df$fecha_hora <- as.POSIXct(df$fecha) + df$hora * 3600
df$hora_texto <- sprintf("%02d:00", df$hora)
df$periodo <- format(df$fecha, "%Y-%m")

df <- df[order(df$fecha_hora), ]
rownames(df) <- NULL

# ============================================================
# FUNCIONES AUXILIARES
# ============================================================

filtrar_datos <- function(fecha_inicio, fecha_fin, zonas, transportes,
                          dias, meses, niveles) {
  datos <- df %>%
    filter(fecha >= as.Date(fecha_inicio),
           fecha <= as.Date(fecha_fin))

  if (length(zonas) > 0) {
    datos <- datos %>% filter(zona_origen %in% zonas)
  }
  if (length(transportes) > 0) {
    datos <- datos %>% filter(tipo_transporte %in% transportes)
  }
  if (length(dias) > 0) {
    datos <- datos %>% filter(dia_semana %in% dias)
  }
  if (length(meses) > 0) {
    datos <- datos %>% filter(mes %in% meses)
  }
  if (length(niveles) > 0) {
    datos <- datos %>% filter(nivel_congestion %in% niveles)
  }

  datos
}

fmt_num <- function(x, dec = 0) {
  format(round(x, dec), big.mark = ".", decimal.mark = ",",
         scientific = FALSE, trim = TRUE, nsmall = dec)
}

fmt_fecha <- function(x) {
  format(as.Date(x), "%d/%m/%Y")
}

tema_plot <- function(fig, titulo = "", altura = 360) {
  fig %>%
    layout(
      template = "plotly_dark",
      height = altura,
      paper_bgcolor = "rgba(0,0,0,0)",
      plot_bgcolor = "rgba(0,0,0,0)",
      font = list(family = "Arial", color = "#E8EEF5"),
      title = list(text = titulo, x = 0.02, xanchor = "left",
                   font = list(size = 17)),
      margin = list(l = 50, r = 35, t = 55, b = 45),
      legend = list(orientation = "h", yanchor = "bottom",
                    y = 1.01, xanchor = "left", x = 0),
      hoverlabel = list(bgcolor = "#17212B", font = list(size = 12))
    )
}

kpi_card <- function(titulo, valor, subtitulo = "") {
  div(class = "kpi-card",
      div(class = "kpi-title", titulo),
      div(class = "kpi-value", valor),
      div(class = "kpi-subtitle", subtitulo))
}

generar_insights <- function(datos) {
  if (nrow(datos) == 0) {
    return(div(class = "insight-empty",
               "No hay datos para los filtros seleccionados."))
  }

  congestion_zona <- datos %>%
    group_by(zona_origen) %>%
    summarise(valor = mean(indice_trafico), .groups = "drop") %>%
    arrange(desc(valor))

  zona <- congestion_zona$zona_origen[1]
  zona_valor <- congestion_zona$valor[1]

  viajes_hora <- datos %>%
    group_by(hora) %>%
    summarise(viajes = sum(cantidad_viajes), .groups = "drop")

  hora <- viajes_hora$hora[which.max(viajes_hora$viajes)]
  hora_viajes <- max(viajes_hora$viajes)

  viajes_transporte <- datos %>%
    group_by(tipo_transporte) %>%
    summarise(viajes = sum(cantidad_viajes), .groups = "drop") %>%
    arrange(desc(viajes))

  transporte <- viajes_transporte$tipo_transporte[1]

  viajes_dia <- datos %>%
    group_by(dia_semana) %>%
    summarise(viajes = sum(cantidad_viajes), .groups = "drop") %>%
    right_join(data.frame(dia_semana = DIAS), by = "dia_semana") %>%
    replace_na(list(viajes = 0))

  dia <- viajes_dia$dia_semana[which.max(viajes_dia$viajes)]

  emisiones_transporte <- datos %>%
    group_by(tipo_transporte) %>%
    summarise(emisiones = sum(emisiones_co2_kg), .groups = "drop") %>%
    arrange(desc(emisiones))

  mayor_emisor <- emisiones_transporte$tipo_transporte[1]
  mayor_emisor_valor <- emisiones_transporte$emisiones[1]

  evolucion <- datos %>%
    group_by(fecha) %>%
    summarise(emisiones = sum(emisiones_co2_kg), .groups = "drop") %>%
    arrange(fecha)

  cambio <- 0
  if (nrow(evolucion) >= 14) {
    mitad <- floor(nrow(evolucion) / 2)
    primera <- mean(evolucion$emisiones[1:mitad])
    segunda <- mean(evolucion$emisiones[(mitad + 1):nrow(evolucion)])
    if (primera != 0) cambio <- (segunda - primera) / primera * 100
  }

  tendencia <- if (cambio > 2) {
    paste0("aumentaron ", round(cambio, 1), "%")
  } else if (cambio < -2) {
    paste0("disminuyeron ", round(abs(cambio), 1), "%")
  } else {
    "se mantuvieron relativamente estables"
  }

  datos_insight <- list(
    c("01", "Mayor congestión",
      paste0(zona, " registra el índice promedio más alto: ",
             round(zona_valor, 1), "/100.")),
    c("02", "Hora de mayor demanda",
      paste0(sprintf("%02d", hora), ":00 concentra aproximadamente ",
             fmt_num(hora_viajes), " viajes.")),
    c("03", "Transporte dominante",
      paste0(transporte, " es el medio con mayor cantidad de viajes.")),
    c("04", "Día con mayor demanda",
      paste0(dia, " presenta la mayor demanda acumulada.")),
    c("05", "Principal fuente de CO₂",
      paste0(mayor_emisor, " aporta aproximadamente ",
             fmt_num(mayor_emisor_valor, 1), " kg de CO₂.")),
    c("06", "Tendencia de emisiones",
      paste0("En la comparación temporal, las emisiones ", tendencia, "."))
  )

  tagList(lapply(datos_insight, function(x) {
    div(class = "insight-item",
        div(class = "insight-number", x[1]),
        div(
          div(class = "insight-title", x[2]),
          div(class = "insight-text", x[3])
        )
    )
  }))
}

# ============================================================
# INTERFAZ
# ============================================================

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      body {
        margin: 0;
        background: #081018;
        color: #E8EEF5;
        font-family: Arial, Helvetica, sans-serif;
      }

      .dashboard {
        min-height: 100vh;
        background:
          radial-gradient(circle at 85% 0%, rgba(37,99,235,0.12), transparent 30%),
          radial-gradient(circle at 0% 25%, rgba(14,165,233,0.07), transparent 25%),
          #081018;
        padding: 28px 38px 45px 38px;
      }

      .dashboard-header {
        display: flex;
        justify-content: space-between;
        align-items: flex-end;
        border-bottom: 1px solid #263442;
        padding-bottom: 25px;
        margin-bottom: 24px;
      }

      .eyebrow {
        color: #5CC8FF;
        letter-spacing: 3px;
        font-size: 11px;
        font-weight: 700;
        margin-bottom: 8px;
      }

      h1 {
        margin: 0;
        font-size: 30px;
        letter-spacing: 0.5px;
        font-weight: 800;
      }

      .subtitle {
        margin: 9px 0 0 0;
        color: #98A9BA;
        font-size: 15px;
      }

      .header-period {
        text-align: right;
        padding: 12px 18px;
        border: 1px solid #263442;
        background: rgba(16,24,32,0.78);
        border-radius: 12px;
      }

      .period-label {
        color: #7F92A4;
        font-size: 10px;
        letter-spacing: 1.5px;
        font-weight: 700;
      }

      .period-value {
        color: #FFFFFF;
        font-size: 14px;
        margin-top: 5px;
        font-weight: 700;
      }

      .period-records {
        color: #5CC8FF;
        font-size: 11px;
        margin-top: 5px;
      }

      .section-label {
        font-size: 12px;
        letter-spacing: 2px;
        color: #7890A5;
        font-weight: 800;
        margin: 28px 0 11px 3px;
      }

      .panel, .half-panel {
        background: rgba(14,23,32,0.88);
        border: 1px solid #263442;
        border-radius: 14px;
        box-shadow: 0 12px 35px rgba(0,0,0,0.18);
      }

      .graph-panel {
        padding: 10px 12px 4px 12px;
      }

      .filters-panel {
        padding: 18px;
      }

      .filters-grid {
        display: grid;
        grid-template-columns: 1.25fr repeat(5, 1fr);
        gap: 12px;
      }

      .filter-item label {
        display: block;
        color: #8EA1B3;
        font-size: 11px;
        margin-bottom: 6px;
        font-weight: 700;
      }

      .kpi-grid {
        display: grid;
        grid-template-columns: repeat(7, 1fr);
        gap: 11px;
      }

      .kpi-card {
        background: linear-gradient(145deg, rgba(20,34,47,0.98), rgba(12,21,30,0.98));
        border: 1px solid #293B4B;
        border-radius: 14px;
        padding: 17px 15px;
        min-height: 105px;
        position: relative;
        overflow: hidden;
      }

      .kpi-card:before {
        content: '';
        position: absolute;
        left: 0;
        top: 0;
        width: 4px;
        height: 100%;
        background: #36BDF8;
      }

      .kpi-title {
        color: #7890A5;
        font-size: 10px;
        letter-spacing: 1.2px;
        font-weight: 800;
      }

      .kpi-value {
        color: #F6FAFD;
        font-size: 22px;
        font-weight: 800;
        margin-top: 10px;
        white-space: nowrap;
      }

      .kpi-subtitle {
        color: #71879A;
        font-size: 10px;
        margin-top: 6px;
      }

      .two-columns {
        display: grid;
        grid-template-columns: 1fr 1fr;
        gap: 14px;
      }

      .half-panel {
        padding: 10px 12px 4px 12px;
      }

      .chart-description {
        color: #7F92A4;
        font-size: 12px;
        margin: 3px 10px 0 10px;
      }

      .insights-grid {
        display: grid;
        grid-template-columns: repeat(3, 1fr);
        gap: 12px;
      }

      .insight-item {
        display: flex;
        gap: 13px;
        background: #101A24;
        border: 1px solid #273847;
        border-radius: 12px;
        padding: 16px;
        min-height: 82px;
      }

      .insight-number {
        color: #36BDF8;
        font-size: 12px;
        font-weight: 900;
        padding-top: 2px;
      }

      .insight-title {
        color: #EAF3F9;
        font-weight: 800;
        font-size: 13px;
        margin-bottom: 6px;
      }

      .insight-text {
        color: #899CAD;
        font-size: 12px;
        line-height: 1.5;
      }

      .insight-empty {
        padding: 20px;
        color: #8A9CAE;
      }

      .table-panel {
        padding: 14px;
      }

      .footer {
        text-align: center;
        color: #536678;
        font-size: 10px;
        letter-spacing: 0.7px;
        margin-top: 35px;
      }

      .form-control, .selectize-control .selectize-input,
      .selectize-control.multi .selectize-input {
        background: #111C26 !important;
        color: #E8EEF5 !important;
        border: 1px solid #314353 !important;
        border-radius: 8px !important;
      }

      .selectize-dropdown {
        background: #111C26 !important;
        color: #E8EEF5 !important;
        border: 1px solid #314353 !important;
      }

      .selectize-dropdown .option {
        color: #E8EEF5 !important;
      }

      .selectize-dropdown .active {
        background: #203342 !important;
      }

      .selectize-input .item {
        color: #E8EEF5 !important;
      }

      .date-input input {
        background: #111C26 !important;
        color: #E8EEF5 !important;
      }

      .irs-bar, .irs-from, .irs-to, .irs-single {
        background: #36BDF8 !important;
        border-color: #36BDF8 !important;
      }

      .dataTables_wrapper {
        color: #DDE6EF !important;
      }

      table.dataTable tbody tr {
        background: #101820 !important;
        color: #DDE6EF !important;
      }

      table.dataTable thead th {
        background: #182430 !important;
        color: #FFFFFF !important;
      }

      table.dataTable tbody tr:hover {
        background: #203342 !important;
      }

      @media (max-width: 1200px) {
        .filters-grid { grid-template-columns: repeat(3, 1fr); }
        .kpi-grid { grid-template-columns: repeat(4, 1fr); }
        .insights-grid { grid-template-columns: repeat(2, 1fr); }
      }

      @media (max-width: 800px) {
        .dashboard { padding: 18px; }
        .dashboard-header {
          flex-direction: column;
          align-items: flex-start;
          gap: 18px;
        }
        .header-period { text-align: left; }
        .filters-grid, .two-columns, .insights-grid, .kpi-grid {
          grid-template-columns: 1fr;
        }
      }
    "))
  ),

  div(class = "dashboard",

      div(class = "dashboard-header",
          div(
            div(class = "eyebrow", "NOVA MOBILITY INTELLIGENCE"),
            h1("CENTRO DE CONTROL DE MOVILIDAD URBANA"),
            p("Panel ejecutivo de movilidad inteligente — Ciudad Nova",
              class = "subtitle")
          ),
          div(class = "header-period",
              div(class = "period-label", "PERÍODO ANALIZADO"),
              div(class = "period-value",
                  paste0(format(FECHA_INICIO, "%d/%m/%Y"), " — ",
                         format(FECHA_FIN, "%d/%m/%Y"))),
              div(class = "period-records",
                  paste0(format(N_REGISTROS, big.mark = "."), " registros simulados"))
          )
      ),

      div(class = "section-label", "FILTROS DE ANÁLISIS"),

      div(class = "filters-grid panel filters-panel",

          div(class = "filter-item",
              tags$label("Rango de fechas"),
              dateRangeInput(
                "filtro_fechas", NULL,
                start = FECHA_INICIO, end = FECHA_FIN,
                min = FECHA_INICIO, max = FECHA_FIN,
                format = "dd/mm/yyyy",
                separator = " hasta "
              )
          ),

          div(class = "filter-item",
              tags$label("Zona"),
              selectizeInput("filtro_zona", NULL,
                             choices = ZONAS, selected = ZONAS,
                             multiple = TRUE,
                             options = list(plugins = list("remove_button")))
          ),

          div(class = "filter-item",
              tags$label("Tipo de transporte"),
              selectizeInput("filtro_transporte", NULL,
                             choices = TRANSPORTES, selected = TRANSPORTES,
                             multiple = TRUE,
                             options = list(plugins = list("remove_button")))
          ),

          div(class = "filter-item",
              tags$label("Día de la semana"),
              selectizeInput("filtro_dia", NULL,
                             choices = DIAS, selected = DIAS,
                             multiple = TRUE,
                             options = list(plugins = list("remove_button")))
          ),

          div(class = "filter-item",
              tags$label("Mes"),
              selectizeInput("filtro_mes", NULL,
                             choices = MESES, selected = MESES,
                             multiple = TRUE,
                             options = list(plugins = list("remove_button")))
          ),

          div(class = "filter-item",
              tags$label("Nivel de congestión"),
              selectizeInput(
                "filtro_congestion", NULL,
                choices = c("Baja", "Media", "Alta", "Crítica"),
                selected = c("Baja", "Media", "Alta", "Crítica"),
                multiple = TRUE,
                options = list(plugins = list("remove_button"))
              )
          )
      ),

      div(class = "section-label", "INDICADORES CLAVE DE DESEMPEÑO"),
      uiOutput("contenedor_kpis"),

      div(class = "section-label", "EVOLUCIÓN TEMPORAL"),
      div(class = "panel graph-panel",
          plotlyOutput("grafico_evolucion", height = "430px")
      ),

      div(class = "section-label", "MAPA DE CALOR DE CONGESTIÓN"),
      div(class = "panel graph-panel",
          p("Índice promedio de tráfico por día de la semana y hora del día.",
            class = "chart-description"),
          plotlyOutput("grafico_heatmap", height = "400px")
      ),

      div(class = "section-label", "ANÁLISIS POR ZONA"),
      div(class = "two-columns",
          div(class = "half-panel", plotlyOutput("grafico_zonas", height = "390px")),
          div(class = "half-panel", plotlyOutput("grafico_zonas_treemap", height = "390px"))
      ),

      div(class = "section-label", "ANÁLISIS DE TRANSPORTE"),
      div(class = "two-columns",
          div(class = "half-panel", plotlyOutput("grafico_donut", height = "390px")),
          div(class = "half-panel", plotlyOutput("grafico_transporte_barras", height = "390px"))
      ),

      div(class = "panel graph-panel",
          plotlyOutput("grafico_boxplot", height = "430px")
      ),

      div(class = "section-label", "ANÁLISIS DE EMISIONES DE CO₂"),
      div(class = "two-columns",
          div(class = "half-panel", plotlyOutput("grafico_emisiones_transporte", height = "390px")),
          div(class = "half-panel", plotlyOutput("grafico_emisiones_zona", height = "390px"))
      ),

      div(class = "panel graph-panel",
          plotlyOutput("grafico_emisiones_tiempo", height = "390px")
      ),

      div(class = "section-label", "SEGURIDAD E INCIDENTES"),
      div(class = "two-columns",
          div(class = "half-panel", plotlyOutput("grafico_incidentes_tiempo", height = "390px")),
          div(class = "half-panel", plotlyOutput("grafico_incidentes_hora", height = "390px"))
      ),

      div(class = "section-label", "INSIGHTS PRINCIPALES"),
      div(class = "insights-grid",
          uiOutput("contenedor_insights")
      ),

      div(class = "section-label", "DETALLE DE REGISTROS"),
      div(class = "panel table-panel",
          p("La tabla responde a todos los filtros seleccionados. Permite búsqueda, ordenamiento y paginación.",
            class = "chart-description"),
          DTOutput("tabla_detalle")
      ),

      div(class = "footer",
          "Ciudad Nova • Centro de Control de Movilidad • Datos simulados para fines analíticos"
      )
  )
)

# ============================================================
# SERVIDOR
# ============================================================

server <- function(input, output, session) {

  datos_filtrados <- reactive({
    req(input$filtro_fechas)
    filtrar_datos(
      input$filtro_fechas[1],
      input$filtro_fechas[2],
      input$filtro_zona,
      input$filtro_transporte,
      input$filtro_dia,
      input$filtro_mes,
      input$filtro_congestion
    )
  })

  output$contenedor_kpis <- renderUI({
    datos <- datos_filtrados()

    if (nrow(datos) == 0) {
      return(
        div(class = "kpi-grid",
            kpi_card("TOTAL DE VIAJES", "0", "Sin registros"),
            kpi_card("VELOCIDAD PROMEDIO", "— km/h", "Sin registros"),
            kpi_card("TIEMPO PROMEDIO", "— min", "Sin registros"),
            kpi_card("ÍNDICE DE CONGESTIÓN", "— /100", "Sin registros"),
            kpi_card("EMISIONES CO₂", "— kg", "Sin registros"),
            kpi_card("INCIDENTES", "0", "Sin registros"),
            kpi_card("SATISFACCIÓN", "— /100", "Sin registros")
        )
      )
    }

    total_viajes <- sum(datos$cantidad_viajes)
    velocidad <- mean(datos$velocidad_promedio_kmh)
    duracion <- mean(datos$duracion_minutos)
    congestion <- mean(datos$indice_trafico)
    emisiones <- sum(datos$emisiones_co2_kg)
    incidentes <- sum(datos$incidentes)
    accidentes <- sum(datos$accidentes)
    satisfaccion <- mean(datos$satisfaccion_usuario)

    div(class = "kpi-grid",
        kpi_card("TOTAL DE VIAJES", fmt_num(total_viajes),
                 paste0(nrow(datos), " registros filtrados")),
        kpi_card("VELOCIDAD PROMEDIO", paste0(round(velocidad, 1), " km/h"),
                 "Meta referencial > 35 km/h"),
        kpi_card("TIEMPO PROMEDIO", paste0(round(duracion, 1), " min"),
                 "Por desplazamiento"),
        kpi_card("ÍNDICE DE CONGESTIÓN", paste0(round(congestion, 1), " /100"),
                 "Menor es mejor"),
        kpi_card("EMISIONES CO₂", paste0(fmt_num(emisiones, 1), " kg"),
                 "Estimación agregada"),
        kpi_card("INCIDENTES", fmt_num(incidentes),
                 paste0(fmt_num(accidentes), " accidentes")),
        kpi_card("SATISFACCIÓN", paste0(round(satisfaccion, 1), " /100"),
                 "Percepción estimada")
    )
  })

  output$grafico_evolucion <- renderPlotly({
    datos <- datos_filtrados()
    req(nrow(datos) > 0)

    diario <- datos %>%
      group_by(fecha) %>%
      summarise(
        viajes = sum(cantidad_viajes),
        velocidad = mean(velocidad_promedio_kmh),
        congestion = mean(indice_trafico),
        emisiones = sum(emisiones_co2_kg),
        .groups = "drop"
      )

    fig <- plot_ly(diario, x = ~fecha)

    fig <- fig %>% add_lines(
      y = ~viajes, name = "Viajes",
      line = list(width = 2.5),
      hovertemplate = paste("%{x|%d/%m/%Y}<br>Viajes: %{y:,.0f}<extra></extra>")
    )

    fig <- fig %>% add_lines(
      y = ~emisiones, name = "CO₂ (kg)",
      line = list(width = 2),
      hovertemplate = paste("%{x|%d/%m/%Y}<br>CO₂: %{y:,.1f} kg<extra></extra>")
    )

    fig <- fig %>% add_lines(
      y = ~velocidad, name = "Velocidad",
      yaxis = "y2", line = list(width = 2, dash = "dot"),
      hovertemplate = paste("%{x|%d/%m/%Y}<br>Velocidad: %{y:.1f} km/h<extra></extra>")
    )

    fig <- fig %>% add_lines(
      y = ~congestion, name = "Congestión",
      yaxis = "y2", line = list(width = 2, dash = "dash"),
      hovertemplate = paste("%{x|%d/%m/%Y}<br>Congestión: %{y:.1f}/100<extra></extra>")
    )

    fig <- fig %>% layout(
      yaxis = list(title = "Viajes / emisiones"),
      yaxis2 = list(title = "Velocidad / congestión",
                    overlaying = "y", side = "right")
    )

    tema_plot(fig, "Evolución diaria de la movilidad", 430)
  })

  output$grafico_heatmap <- renderPlotly({
    datos <- datos_filtrados()
    req(nrow(datos) > 0)

    heat <- datos %>%
      group_by(dia_semana, hora) %>%
      summarise(indice = mean(indice_trafico), .groups = "drop") %>%
      complete(dia_semana = DIAS, hora = 0:23) %>%
      pivot_wider(names_from = hora, values_from = indice) %>%
      arrange(match(dia_semana, DIAS))

    z <- as.matrix(heat[, -1])
    colnames(z) <- sprintf("%02d:00", 0:23)

    fig <- plot_ly(
      z = z,
      x = sprintf("%02d:00", 0:23),
      y = DIAS,
      type = "heatmap",
      zmin = 0, zmax = 100,
      colorscale = list(
        list(0.0, "#2ECC71"),
        list(0.35, "#F1C40F"),
        list(0.65, "#E67E22"),
        list(1.0, "#E74C3C")
      ),
      colorbar = list(title = "Índice"),
      hovertemplate = paste(
        "Día: %{y}<br>Hora: %{x}<br>",
        "Índice: %{z:.1f}<extra></extra>"
      )
    )

    tema_plot(fig, "Mapa de calor: congestión por día y hora", 400)
  })

  output$grafico_zonas <- renderPlotly({
    datos <- datos_filtrados()
    req(nrow(datos) > 0)

    zona_stats <- datos %>%
      group_by(zona_origen) %>%
      summarise(
        viajes = sum(cantidad_viajes),
        velocidad = mean(velocidad_promedio_kmh),
        congestion = mean(indice_trafico),
        incidentes = sum(incidentes),
        emisiones = sum(emisiones_co2_kg),
        .groups = "drop"
      ) %>%
      arrange(viajes)

    fig <- plot_ly(
      zona_stats,
      y = ~zona_origen,
      x = ~viajes,
      type = "bar",
      orientation = "h",
      hovertemplate = paste("%{y}<br>Viajes: %{x:,.0f}<extra></extra>")
    ) %>%
      layout(xaxis = list(title = "Cantidad de viajes"))

    tema_plot(fig, "Demanda de movilidad por zona", 390)
  })

  output$grafico_zonas_treemap <- renderPlotly({
    datos <- datos_filtrados()
    req(nrow(datos) > 0)

    zona_stats <- datos %>%
      group_by(zona_origen) %>%
      summarise(
        viajes = sum(cantidad_viajes),
        velocidad = mean(velocidad_promedio_kmh),
        congestion = mean(indice_trafico),
        incidentes = sum(incidentes),
        emisiones = sum(emisiones_co2_kg),
        .groups = "drop"
      )

    fig <- plot_ly(
      zona_stats,
      type = "treemap",
      labels = ~zona_origen,
      values = ~viajes,
      parents = "",
      marker = list(
        colors = ~congestion,
        colorscale = list(
          list(0, "#2ECC71"),
          list(0.35, "#F1C40F"),
          list(0.65, "#E67E22"),
          list(1, "#E74C3C")
        ),
        cmin = 0, cmax = 100,
        colorbar = list(title = "Congestión")
      ),
      hovertemplate = paste(
        "<b>%{label}</b><br>",
        "Viajes: %{value:,.0f}<br>",
        "Congestión: %{color:.1f}<extra></extra>"
      )
    )

    tema_plot(fig, "Mapa jerárquico: demanda y congestión", 390)
  })

  output$grafico_donut <- renderPlotly({
    datos <- datos_filtrados()
    req(nrow(datos) > 0)

    stats <- datos %>%
      group_by(tipo_transporte) %>%
      summarise(
        viajes = sum(cantidad_viajes),
        emisiones = sum(emisiones_co2_kg),
        velocidad = mean(velocidad_promedio_kmh),
        duracion = mean(duracion_minutos),
        .groups = "drop"
      )

    fig <- plot_ly(
      stats,
      labels = ~tipo_transporte,
      values = ~viajes,
      type = "pie",
      hole = 0.62,
      marker = list(colors = unname(COLORES_TRANSPORTE[stats$tipo_transporte])),
      textinfo = "percent",
      hovertemplate = paste(
        "<b>%{label}</b><br>",
        "Viajes: %{value:,.0f}<br>",
        "Participación: %{percent}<extra></extra>"
      )
    )

    tema_plot(fig, "Participación por tipo de transporte", 390)
  })

  output$grafico_transporte_barras <- renderPlotly({
    datos <- datos_filtrados()
    req(nrow(datos) > 0)

    stats <- datos %>%
      group_by(tipo_transporte) %>%
      summarise(
        viajes = sum(cantidad_viajes),
        emisiones = sum(emisiones_co2_kg),
        velocidad = mean(velocidad_promedio_kmh),
        duracion = mean(duracion_minutos),
        .groups = "drop"
      )

    fig <- plot_ly(stats, x = ~tipo_transporte) %>%
      add_bars(y = ~velocidad, name = "Velocidad",
               hovertemplate = "%{x}<br>%{y:.1f} km/h<extra></extra>") %>%
      add_bars(y = ~duracion, name = "Duración",
               hovertemplate = "%{x}<br>%{y:.1f} min<extra></extra>") %>%
      layout(barmode = "group", yaxis = list(title = "Valor promedio"))

    tema_plot(fig, "Velocidad y duración por transporte", 390)
  })

  output$grafico_boxplot <- renderPlotly({
    datos <- datos_filtrados()
    req(nrow(datos) > 0)

    fig <- plot_ly(
      datos,
      x = ~tipo_transporte,
      y = ~duracion_minutos,
      type = "box",
      color = ~tipo_transporte,
      colors = unname(COLORES_TRANSPORTE),
      boxmean = TRUE,
      hovertemplate = paste(
        "%{x}<br>Duración: %{y:.1f} min<extra></extra>"
      )
    ) %>%
      layout(yaxis = list(title = "Duración del viaje (minutos)"))

    tema_plot(fig, "Distribución de duración por medio de transporte", 430)
  })

  output$grafico_emisiones_transporte <- renderPlotly({
    datos <- datos_filtrados()
    req(nrow(datos) > 0)

    emisiones <- datos %>%
      group_by(tipo_transporte) %>%
      summarise(emisiones = sum(emisiones_co2_kg), .groups = "drop") %>%
      arrange(emisiones)

    fig <- plot_ly(
      emisiones,
      y = ~tipo_transporte,
      x = ~emisiones,
      type = "bar",
      orientation = "h",
      hovertemplate = paste(
        "%{y}<br>CO₂: %{x:,.1f} kg<extra></extra>"
      )
    ) %>%
      layout(xaxis = list(title = "Emisiones estimadas de CO₂ (kg)"))

    tema_plot(fig, "Emisiones por tipo de transporte", 390)
  })

  output$grafico_emisiones_zona <- renderPlotly({
    datos <- datos_filtrados()
    req(nrow(datos) > 0)

    emisiones <- datos %>%
      group_by(zona_origen) %>%
      summarise(emisiones_co2_kg = sum(emisiones_co2_kg), .groups = "drop") %>%
      arrange(emisiones_co2_kg)

    fig <- plot_ly(
      emisiones,
      y = ~zona_origen,
      x = ~emisiones_co2_kg,
      type = "bar",
      orientation = "h",
      hovertemplate = paste(
        "%{y}<br>CO₂: %{x:,.1f} kg<extra></extra>"
      )
    ) %>%
      layout(xaxis = list(title = "Emisiones estimadas (kg)"))

    tema_plot(fig, "Emisiones acumuladas por zona", 390)
  })

  output$grafico_emisiones_tiempo <- renderPlotly({
    datos <- datos_filtrados()
    req(nrow(datos) > 0)

    emisiones <- datos %>%
      group_by(fecha) %>%
      summarise(emisiones_co2_kg = sum(emisiones_co2_kg), .groups = "drop")

    fig <- plot_ly(
      emisiones,
      x = ~fecha,
      y = ~emisiones_co2_kg,
      type = "scatter",
      mode = "lines",
      fill = "tozeroy",
      line = list(width = 2.5),
      hovertemplate = paste(
        "%{x|%d/%m/%Y}<br>CO₂: %{y:,.1f} kg<extra></extra>"
      )
    ) %>%
      layout(yaxis = list(title = "CO₂ (kg)"))

    tema_plot(fig, "Evolución diaria de emisiones", 390)
  })

  output$grafico_incidentes_tiempo <- renderPlotly({
    datos <- datos_filtrados()
    req(nrow(datos) > 0)

    eventos <- datos %>%
      group_by(fecha) %>%
      summarise(
        incidentes = sum(incidentes),
        accidentes = sum(accidentes),
        .groups = "drop"
      )

    fig <- plot_ly(eventos, x = ~fecha) %>%
      add_lines(
        y = ~incidentes, name = "Incidentes",
        line = list(width = 2.5),
        hovertemplate = paste(
          "%{x|%d/%m/%Y}<br>Incidentes: %{y:,.0f}<extra></extra>"
        )
      ) %>%
      add_lines(
        y = ~accidentes, name = "Accidentes",
        line = list(width = 2),
        hovertemplate = paste(
          "%{x|%d/%m/%Y}<br>Accidentes: %{y:,.0f}<extra></extra>"
        )
      ) %>%
      layout(yaxis = list(title = "Eventos"))

    tema_plot(fig, "Evolución temporal de incidentes y accidentes", 390)
  })

  output$grafico_incidentes_hora <- renderPlotly({
    datos <- datos_filtrados()
    req(nrow(datos) > 0)

    eventos <- datos %>%
      group_by(hora) %>%
      summarise(
        incidentes = sum(incidentes),
        accidentes = sum(accidentes),
        .groups = "drop"
      )

    fig <- plot_ly(eventos, x = ~hora) %>%
      add_bars(
        y = ~incidentes, name = "Incidentes",
        hovertemplate = paste(
          "Hora: %{x}:00<br>Incidentes: %{y:,.0f}<extra></extra>"
        )
      ) %>%
      add_lines(
        y = ~accidentes, name = "Accidentes",
        yaxis = "y2", mode = "lines+markers",
        hovertemplate = paste(
          "Hora: %{x}:00<br>Accidentes: %{y:,.0f}<extra></extra>"
        )
      ) %>%
      layout(
        yaxis = list(title = "Incidentes"),
        yaxis2 = list(title = "Accidentes",
                      overlaying = "y", side = "right")
      )

    tema_plot(fig, "Eventos de seguridad por hora", 390)
  })

  output$contenedor_insights <- renderUI({
    generar_insights(datos_filtrados())
  })

  output$tabla_detalle <- renderDT({
    datos <- datos_filtrados()

    if (nrow(datos) == 0) {
      return(datatable(data.frame(Mensaje = "No hay registros para los filtros seleccionados."),
                       options = list(pageLength = 15)))
    }

    tabla <- datos %>%
      arrange(desc(fecha_hora)) %>%
      slice_head(n = 5000) %>%
      transmute(
        Fecha = format(fecha, "%d/%m/%Y"),
        Hora = hora_texto,
        Día = dia_semana,
        Origen = zona_origen,
        Destino = zona_destino,
        Transporte = tipo_transporte,
        Viajes = cantidad_viajes,
        `Velocidad km/h` = velocidad_promedio_kmh,
        `Distancia km` = distancia_km,
        `Duración min` = duracion_minutos,
        Congestión = nivel_congestion,
        `Índice tráfico` = indice_trafico,
        Accidentes = accidentes,
        Incidentes = incidentes,
        `CO₂ kg` = emisiones_co2_kg,
        Pasajeros = pasajeros,
        Costo = costo_promedio,
        Satisfacción = satisfaccion_usuario
      )

    datatable(
      tabla,
      rownames = FALSE,
      filter = "top",
      extensions = "Scroller",
      options = list(
        pageLength = 15,
        scrollX = TRUE,
        scrollY = "600px",
        deferRender = TRUE,
        scroller = TRUE
      ),
      class = "compact stripe hover"
    )
  })
}

shinyApp(ui, server)
