library(jsonlite)
library(dplyr)
library(data.table)
library(ggplot2)
library(readxl)



ruta <- readline(prompt = "Ingrese la ruta del archivo datos_Tp1.xlsx: ")
print("Accediendo al archivo...")
datos <- read_excel(ruta, sheet = 2)

# Limpiamos y preparamos los datos
datos <- datos %>%
  # Agregamos una columna con la duracion del viaje en minutos
  mutate(
    Duracion = as.numeric(difftime(HoraFin, HoraInicio, units = "mins"))
  ) %>%
  # Corregimos las duraciones negativas (viajes que cruzan la medianoche)
  # Si Duracion < 0, le sumamos los minutos de un día (24 * 60 = 1440)
  mutate(
    Duracion = ifelse(Duracion < 0, Duracion + 1440, Duracion)
  )

# Ahora que todas las duraciones están corregidas, eliminamos las filas
# donde la duración sea NA (probablemente porque HoraInicio o HoraFin eran NA)
datos <- datos[!is.na(datos$Duracion), ]


# Filtramos valores atípicos (outliers)
# Asumimos que ningún viaje individual puede durar más de 24 horas.
datos <- datos %>%
  filter(Duracion <= 1440)
log_conexion <- file("log_viajes.txt", "w") # "w" = write (sobrescribir)
cat("--- INICIO DEL LOG DE PROCESAMIENTO ---\n", file = log_conexion)

lista_motivos <- c(
  "Al Trabajo", "Belleza personal (peluquería, manicura, peinado)",
  "Buscar o dejar algo", "Comer o tomar algo", "Culto, prácticas religión",
  "De compras", "De salud", "Dejar/Buscar a Alguien", "Otra cosa",
  "Por estudio", "Por Trabajo", "Practicar deportes, correr, caminar",
  "Recreación", "Trámites", "Ver a alguien", "Volver a Casa"
)
#Desagregar motivo Otra cosa
motivos_detalle <- c(
  "Parada de colectivo",
  "Lugares de culto, iglesia, etc.",
  "Comercio de ropa u otro tipo de comercio de compras",
  "Casa de parientes",
  "Parques naturales o plaza / Campo",
  "Estación de ómnibus o trenes",
  "Clubes deportivos",
  "Gimnasio y spa",
  "Casa de amigos",
  "Casa de padres",
  "Teatro o cine",
  "Lugar de trabajo",
  "Cementerio",
  "Escuela / campus / establecimiento educativo",
  "Restaurante",
  "Casa de novio/a",
  "Supermercado / despensa / verdulería / carnicería / dietética",
  "Boliche",
  "Organismos públicos",
  "Taller mecánico",
  "Sindicato / gremio / asociación civil",
  "Organismos de servicios privados",
  "Salón de fiestas",
  "Banco u otra entidad financiera",
  "Fuera de Zona Urbana",
  "Farmacia",
  "Hospital / clínica / centro de salud",
  "Usina / gas del Estado / obras sanitarias / otros servicios públicos",
  "Casino",
  "Casa de cliente"
)

porcentajes <- c(
  32.85,
   9.03,
   6.50,
   5.42,
   4.69,
   4.33,
   4.33,
   2.89,
   2.89,
   2.53,
   2.53,
   2.17,
   1.81,
   1.81,
   1.81,
   1.44,
   1.44,
   1.44,
   1.44,
   1.08,
   1.08,
   1.08,
   1.08,
   1.08,
   1.08,
   0.72,
   0.36,
   0.36,
   0.36,
   0.36
)

prob_mot <- porcentajes / sum(porcentajes)   # suma = 1
prob_acum_mot <- cumsum(prob_mot)           # vector de probabilidades acumuladas

#funcion para proceso estocastico
sortear_motivo <- function() {
  u <- runif(1)                # 1) Genera un número entre 0 y 1
  idx <- which(u <= prob_acum_mot)[1]   # 2) Busca en qué intervalo cae
  motivos_detalle[idx]         # 3) Devuelve el motivo correspondiente
}
reasignar_otra_cosa <- function(df) {
  idx <- which(df$`Motivo del Viaje` == "Otra cosa")
  
  if (length(idx) == 0) return(df)  # si no hay nada que cambiar
  
  # Generar los nuevos motivos estocásticos
  nuevos <- sapply(idx, function(i) sortear_motivo())
  
  # Reemplazar en el dataset
  df$`Motivo del Viaje`[idx] <- paste("Otra cosa:", nuevos)
  
  return(df)
}
datos <- reasignar_otra_cosa(datos)


lugaresBase <- c("Volver a Casa", "Al Trabajo", "Por Trabajo", "Por estudio")

# (Esta lista se usa para la condición de corte del loop)
lugaresBase_CIERRE <- c("Volver a Casa")

# --- B. Acumuladores de Resultados (Globales)
promedioPorPersona <- data.frame(
  persona = character(),
  motivo = character(),
  tiempo = numeric(),
  n = numeric()
)

journeys4y5_pendientes <- list()


tiempoViajes_journey_actual <- c()
actividades_journey_actual <- data.frame(
  motivo = character(),
  tiempo = numeric(),
  persona = character()
)

#Actualizar el promedio por motivo de viaje
journalTipo1 <- function(tiempoViajes, actividades) 
{
  if (nrow(actividades) == 0) return() # No procesar si no hay actividad
  sumaViajes<-sum(tiempoViajes)
  indice <- which(promedioPorPersona$persona == actividades$persona[1] & promedioPorPersona$motivo == actividades$motivo[1])
  if (length(indice) > 0) {
    # Ya existe la fila de esa persona con ese motivo → actualizamos
    promedioPorPersona$tiempo[indice] <<- promedioPorPersona$tiempo[indice] + sumaViajes
    promedioPorPersona$n[indice] <<- promedioPorPersona$n[indice] + 1
  } else {
    # No existe → agregamos una nueva fila
    promedioPorPersona <<- add_row(promedioPorPersona,
      persona = actividades$persona[1],
      motivo = actividades$motivo[1],
      tiempo = sumaViajes,
      n = 1,
    )
  }
}

journalTipo2 <- function(tiempoViajes, actividades)
{
  if (nrow(actividades) == 0) return() # No procesar si no hay actividad
  
  total_viajes<-sum(tiempoViajes)
  total_actividades<-sum(actividades$tiempo)
   # Calcular peso por motivo (según tiempo relativo de actividad)
  actividades <- actividades %>%
    mutate(
      peso = tiempo / total_actividades,
      tiempoPonderado = peso * total_viajes,
    )

  for (i in 1:(nrow(actividades))) {  
    motivo_i <- actividades$motivo[i]
    tiempo_i <- as.numeric(actividades$tiempoPonderado[i])
    
    # Buscamos si ya hay fila de esa persona y motivo
    idx <- which(promedioPorPersona$persona == actividades$persona[1] & promedioPorPersona$motivo == motivo_i)
    if (length(idx) > 0) {
      # Ya existe → actualizamos
      promedioPorPersona$tiempo[idx] <- promedioPorPersona$tiempo[idx] + tiempo_i
      promedioPorPersona$n[idx] <- promedioPorPersona$n[idx] + 1
    } else {
      # No existe → agregamos nueva fila
      promedioPorPersona <- add_row(
      promedioPorPersona,
        persona = actividades$persona[i],
        motivo = motivo_i,
        tiempo = tiempo_i,
        n = 1
      )
    }
  }
}

journalTipo3 <- function(tiempoViajes, actividades)
{
  journalTipo1(tiempoViajes, actividades)
}

journalTipo4 <- function(tiempoViajes, actividades)
{
  if (nrow(actividades) == 0) return() # No procesar si no hay actividad
  
  base <- actividades$motivo[which(actividades$motivo %in% lugaresBase)]
  if (length(base) == 0) return() # No se encontró la base, error
  base <- base[1] # Asegurarse de que sea un solo valor
  
  idx_base <- which(promedioPorPersona$persona == actividades$persona[1] & promedioPorPersona$motivo == base)
  if (length(idx_base) > 0 && promedioPorPersona$n[idx_base] > 0) {
    tiempoBase <- promedioPorPersona$tiempo[idx_base] / promedioPorPersona$n[idx_base]
  } else {
    tiempoBase <- 0
  }
  
  sumaViajes <- max(0, sum(tiempoViajes) - tiempoBase)
  
  actividad <- actividades$motivo[which(!actividades$motivo %in% lugaresBase)]
  if (length(actividad) == 0) return()
  actividad <- actividad[1]
  
  idx_act <- which(promedioPorPersona$persona == actividades$persona[1] & promedioPorPersona$motivo == actividad)
  if (length(idx_act) == 0) {
    promedioPorPersona <<- add_row(promedioPorPersona,
                                    persona = actividades$persona[1],
                                    motivo = actividad,
                                    tiempo = sumaViajes,
                                    n = 1
    )
  } else {
    promedioPorPersona$tiempo[idx_act] <<- promedioPorPersona$tiempo[idx_act] + sumaViajes
    promedioPorPersona$n[idx_act] <<- promedioPorPersona$n[idx_act] + 1
  }
}

journalTipo5 <- function(tiempoViajes, actividades)
{
  if (nrow(actividades) < 2) return() # Necesita al menos base + actividad
  
  base <- actividades$motivo[which(actividades$motivo %in% lugaresBase)]
  if (length(base) == 0) return()
  base <- base[1]
  idx_base <- which(promedioPorPersona$persona == actividades$persona[1] & promedioPorPersona$motivo == base)
  if (length(idx_base) > 0 && promedioPorPersona$n[idx_base] > 0) {
    tiempoBase <- promedioPorPersona$tiempo[idx_base] / promedioPorPersona$n[idx_base]
  } else {
    tiempoBase <- 0
  }
  
  sumaViajes <- max(0, sum(tiempoViajes) - tiempoBase)
  
  actividades_no_base <- actividades %>% filter(!motivo %in% lugaresBase)
  if (nrow(actividades_no_base) == 0) return() 
  
  total_actividades<-sum(actividades_no_base$tiempo)
  if (total_actividades == 0) return() 
  
  # Calcular peso por motivo (según tiempo relativo de actividad)
  actividades_no_base <- actividades_no_base %>%
    mutate(
      peso = tiempo / total_actividades,
      tiempoPonderado = peso * sumaViajes,
    )
  
  for (i in 1:(nrow(actividades_no_base))) {
    motivo_i <- actividades_no_base$motivo[i]
    tiempo_i <- as.numeric(actividades_no_base$tiempoPonderado[i])
    
    # Buscamos si ya hay fila de esa persona y motivo
    idx <- which(promedioPorPersona$persona == actividades_no_base$persona[1] & promedioPorPersona$motivo == motivo_i)
    if (length(idx) > 0) {
      promedioPorPersona$tiempo[idx] <<- promedioPorPersona$tiempo[idx] + tiempo_i
      promedioPorPersona$n[idx] <<- promedioPorPersona$n[idx] + 1
    } else {
      promedioPorPersona <<- add_row(
        promedioPorPersona,
        persona = actividades_no_base$persona[1],
        motivo = motivo_i,
        tiempo = tiempo_i,
        n = 1
      )
    }
  }
}




print("Ordenando datos por Persona y preservando orden original")
datos <- datos %>%
  mutate(orden_original = row_number()) %>%
  arrange(Identificación, orden_original)

print("Iniciando procesamiento de journeys")

for (i in 1:(nrow(datos) - 1)) {
  fila_actual <- datos[i, ]
  fila_siguiente <- datos[i+1, ]
  cat("----------------------------------\n", file = log_conexion, append = TRUE)
  cat(paste("Fila", i, "| Persona:", fila_actual$Identificación, 
            "| Viaje a:", fila_actual$"Motivo del Viaje", 
            "| Duración:", round(fila_actual$Duracion, 1), "min\n"),
      file = log_conexion, append = TRUE)
  es_persona_diferente <- fila_actual$Identificación != fila_siguiente$Identificación
  es_viaje_a_cierre <- fila_actual$"Motivo del Viaje" %in% lugaresBase_CIERRE
  tiempoViajes_journey_actual <- c(tiempoViajes_journey_actual, fila_actual$Duracion)
  if (!es_persona_diferente) {
    if (fila_siguiente$HoraInicio < fila_actual$HoraFin) {
      tiempoAct <- as.numeric(
      difftime(fila_siguiente$HoraInicio + (24*60*60), fila_actual$HoraFin, units="mins"))
    } else {
      tiempoAct <- as.numeric(
      difftime(fila_siguiente$HoraInicio, fila_actual$HoraFin, units="mins"))
    }
    
    # Si es tipo 4 pero el tiempo de act es 0, lo tomamos como tipo 3
    if (tiempoAct > 0 && !es_viaje_a_cierre){
      actividades_journey_actual <- rbind(actividades_journey_actual, data.frame(
        motivo = fila_actual$"Motivo del Viaje", 
        tiempo = tiempoAct,
        persona = fila_actual$"Identificación"
      ))

      cat(paste("  -> Acumulada Actividad:", fila_actual$"Motivo del Viaje", 
                "por", round(tiempoAct, 1), "min\n"),
          file = log_conexion, append = TRUE)
    }
  }
  
  if (es_viaje_a_cierre || es_persona_diferente) {
    cat("\n*** FIN DE JOURNEY DETECTADO ***\n", file = log_conexion, append = TRUE)
    cat(paste("  Viajes Acumulados (min):", paste(round(tiempoViajes_journey_actual, 1), collapse = ", "), "\n"), 
        file = log_conexion, append = TRUE)
    if(nrow(actividades_journey_actual) > 0){
        cat(paste("  Actividades Acumuladas:", paste(actividades_journey_actual$motivo, collapse = ", "), "\n"), 
            file = log_conexion, append = TRUE)
    }
    # no procesar journeys vacíos
    if (length(tiempoViajes_journey_actual) == 0 || nrow(actividades_journey_actual) == 0) {
      # Reiniciar y saltar al siguiente
      tiempoViajes_journey_actual <- c()
      actividades_journey_actual <- data.frame(motivo=character(), tiempo=numeric(), persona=character())
      next # Salta a la siguiente iteración del loop
    }
    
    toco_base_intermedia <- any(actividades_journey_actual$motivo %in% lugaresBase)
    
    if (toco_base_intermedia) {
      if (nrow(actividades_journey_actual) == 1) {
        journalTipo3(tiempoViajes_journey_actual, actividades_journey_actual)
        cat("  -> CLASIFICACIÓN: TIPO 3\n\n", file = log_conexion, append = TRUE)
      } else {
        cat("  -> CLASIFICACIÓN: TIPO 4/5 (Guardado)\n\n", file = log_conexion, append = TRUE) 
        journeys4y5_pendientes[[length(journeys4y5_pendientes) + 1]] <- list(
          viajes = tiempoViajes_journey_actual,
          actividades = actividades_journey_actual,
          tipo_estimado = ifelse(nrow(actividades_journey_actual) == 2, 4, 5)
        )
      }
      
    } else {
      if (length(tiempoViajes_journey_actual) == 2) {
        journalTipo1(tiempoViajes_journey_actual, actividades_journey_actual)
        cat("  -> CLASIFICACIÓN: TIPO 1\n\n", file = log_conexion, append = TRUE)
      } else if (length(tiempoViajes_journey_actual) > 2) {
        journalTipo2(tiempoViajes_journey_actual, actividades_journey_actual)
        cat("  -> CLASIFICACIÓN: TIPO 2\n\n", file = log_conexion, append = TRUE)
      }
    }
    
    # REINICIAR ACUMULADORES
    tiempoViajes_journey_actual <- c()
    actividades_journey_actual <- data.frame(
      motivo = character(),
      tiempo = numeric(),
      persona = character()
    )
  }
}

print("Procesamiento de journeys terminado.")
print(paste(length(journeys4y5_pendientes), "journeys Tipo 4/5 guardados para el final."))
print("Iniciando procesamiento de Journeys Tipo 4 y 5")

if (length(journeys4y5_pendientes) > 0) {
  for (j in journeys4y5_pendientes) {
    # Extraemos las actividades que NO son base
    actividades_no_base <- j$actividades %>% filter(!motivo %in% lugaresBase)
    
    if (nrow(actividades_no_base) == 1) {
      # TIPO 4
      journalTipo4(j$viajes, j$actividades)
    } else if (nrow(actividades_no_base) > 1) {
      # TIPO 5
      journalTipo5(j$viajes, j$actividades)
    }
  }
}

print("Procesamiento de Journeys Tipo 4 y 5 terminado.")


print("Calculando promedios finales...")


promedioPorPersona_final <- promedioPorPersona %>%
  filter(n > 0) %>% 
  mutate(tiempo_promedio = tiempo / n) %>%
  select(persona, motivo, tiempo_promedio, tiempo_total = tiempo, n_viajes = n) %>%
  arrange(persona, motivo)

print("--- Promedio Final por Persona y Motivo (Primeras 20 filas) ---")
print(head(promedioPorPersona_final, 20))


#Promedio General (para toda la base)
promedioGeneral_final <- promedioPorPersona %>%
  group_by(motivo) %>%
  summarize(
    tiempo_total_general = sum(tiempo),
    n_total_general = sum(n)
  ) %>%
  filter(n_total_general > 0) %>%
  mutate(tiempo_promedio_general = tiempo_total_general / n_total_general)

print("Promedio General (Toda la Base)")
print(promedioGeneral_final)


cat("FIN DEL LOG\n", file = log_conexion, append = TRUE)
close(log_conexion)

print("SCRIPT COMPLETADO (y log guardado en 'log_viajes.txt')")

#Ahora guardamos los resultados en un excel:
library(writexl)

write_xlsx(
  list(
    Promedio_Por_Persona = promedioPorPersona_final,
    Promedio_General = promedioGeneral_final
  ),
  path = "resultados_viajes.xlsx"
)