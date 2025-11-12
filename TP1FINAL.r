library(jsonlite)
library(dplyr)
library(data.table)
library(ggplot2)
library(readxl)

# --- PASO 1: Carga y Limpieza de Datos ---

# Leemos la hoja 2 del excel con los datos para el practico
# Asegúrate de que esta ruta sea correcta en tu máquina
ruta_archivo <- "C:/FACULTAD/Cuarto/Optativa/datos_Tp1.xlsx"
datos <- read_excel(ruta_archivo, sheet = 2)

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

# --- APLICAMOS EL FILTRO DE 24 HORAS (1440 MINUTOS) ---
# Filtramos valores atípicos (outliers)
# Asumimos que ningún viaje individual puede durar más de 24 horas.
datos <- datos %>%
  filter(Duracion <= 1440)
# --- FIN DEL FILTRO ---


# # Opcional: Verificamos el resultado
# print("Resumen de la columna Duración (Corregida y Filtrada):")
# print(summary(datos$Duracion)) # <-- Ahora el 'max' debería ser <= 1440

# print("Datos listos para el Paso 2:")
# print(head(datos))

# # Código de diagnóstico (opcional)
# # Esto ahora debería imprimir una tabla vacía,
# # lo que prueba que el filtro funcionó.
# fila_error <- datos %>%
#   filter(Duracion > 1440)

# print("--- FILA CON ERROR DE DURACIÓN (debería estar vacío) ---")
# print(fila_error)
# print("-----------------------------------")

# --- PASO 2: Creación de Estructuras de Almacenamiento ---

# --- A. Constantes y Reglas ---
lista_motivos <- c(
  "Al Trabajo", "Belleza personal (peluquería, manicura, peinado)",
  "Buscar o dejar algo", "Comer o tomar algo", "Culto, prácticas religión",
  "De compras", "De salud", "Dejar/Buscar a Alguien", "Otra cosa",
  "Por estudio", "Por Trabajo", "Practicar deportes, correr, caminar",
  "Recreación", "Trámites", "Ver a alguien", "Volver a Casa"
)

lugaresBase <- c("Volver a Casa", "Al Trabajo", "Por Trabajo", "Por estudio")

# --- B. Acumuladores de Resultados (Globales) ---
# (Estos son los data frames FINALES que llenarán las funciones del Paso 4)
promedioPorPersona <- data.frame(
  persona = character(),
  motivo = character(),
  tiempo = numeric(),
  n = numeric()
)

# CORRECCIÓN: 'journeys4y5' debe ser una LISTA, 
# no un data.frame anidado.
journeys4y5_pendientes <- list()


# --- C. Acumuladores Temporales (para UN solo journey) ---
# (Estas son tus variables 'actividades' y 'tiempoViajes')
tiempoViajes_journey_actual <- c()
actividades_journey_actual <- data.frame(
  motivo = character(),
  tiempo = numeric(),
  persona = character()
)

#Estas funciones actualizan el promedio por motivo de viaje
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
  
  # CORRECCIÓN 2: Cálculo seguro de 'tiempoBase'
  idx_base <- which(promedioPorPersona$persona == actividades$persona[1] & promedioPorPersona$motivo == base)
  if (length(idx_base) > 0 && promedioPorPersona$n[idx_base] > 0) {
    # El promedio se calcula 'al vuelo' de lo que ya tenemos
    tiempoBase <- promedioPorPersona$tiempo[idx_base] / promedioPorPersona$n[idx_base]
  } else {
    tiempoBase <- 0 # Si no hay promedio (o n=0), no restar nada
  }
  
  sumaViajes <- max(0, sum(tiempoViajes) - tiempoBase)
  
  actividad <- actividades$motivo[which(!actividades$motivo %in% lugaresBase)]
  if (length(actividad) == 0) return() # No se encontró actividad
  actividad <- actividad[1] # Asegurarse
  
  idx_act <- which(promedioPorPersona$persona == actividades$persona[1] & promedioPorPersona$motivo == actividad)
  if (length(idx_act) == 0) {
    # No existe → agregamos una nueva fila
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
  
  # CORRECCIÓN 2: Cálculo seguro de 'tiempoBase'
  idx_base <- which(promedioPorPersona$persona == actividades$persona[1] & promedioPorPersona$motivo == base)
  if (length(idx_base) > 0 && promedioPorPersona$n[idx_base] > 0) {
    tiempoBase <- promedioPorPersona$tiempo[idx_base] / promedioPorPersona$n[idx_base]
  } else {
    tiempoBase <- 0
  }
  
  sumaViajes <- max(0, sum(tiempoViajes) - tiempoBase)
  
  # CORRECCIÓN 3: Ponderar solo sobre actividades NO-BASE
  actividades_no_base <- actividades %>% filter(!motivo %in% lugaresBase)
  if (nrow(actividades_no_base) == 0) return() # No hay nada que procesar
  
  total_actividades<-sum(actividades_no_base$tiempo)
  if (total_actividades == 0) return() # Evitar división por cero
  
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
      # Ya existe → actualizamos
      promedioPorPersona$tiempo[idx] <<- promedioPorPersona$tiempo[idx] + tiempo_i
      promedioPorPersona$n[idx] <<- promedioPorPersona$n[idx] + 1
    } else {
      # No existe → agregamos nueva fila
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


# --- PASO 3: Recorrer el Dataset y Clasificar Journeys ---

# **FIX 1: Asegurar el orden**
# Ordenar por persona y luego por hora de inicio.
# Esto es CRÍTICO para que `datos[i+1]` sea el viaje siguiente.
print("Ordenando datos por Persona y preservando orden original...")
datos <- datos %>%
  mutate(orden_original = row_number()) %>%
  arrange(Identificación, orden_original)

print("Iniciando procesamiento de journeys...")

# Iteramos hasta la PENÚLTIMA fila, porque siempre miramos `i+1`
for (i in 1:(nrow(datos) - 1)) {
  
  # Datos de la fila actual (i) y siguiente (i+1)
  fila_actual <- datos[i, ]
  fila_siguiente <- datos[i+1, ]
  
  # --- Lógica de Control ---
  # 1. ¿El viaje siguiente es de otra persona? (FIX 2)
  es_persona_diferente <- fila_actual$Identificación != fila_siguiente$Identificación
  
  # 2. ¿El viaje actual es a un lugar base?
  es_viaje_a_base <- fila_actual$"Motivo del Viaje" %in% lugaresBase
  
  
  # --- ACUMULAR DATOS DEL VIAJE Y ACTIVIDAD ---
  
  # Siempre acumulamos el viaje actual
  tiempoViajes_journey_actual <- c(tiempoViajes_journey_actual, fila_actual$Duracion)
  
  # Si la persona es la misma, calculamos el tiempo de actividad
  if (!es_persona_diferente) {
    # Calculo el tiempo de actividad
    if (fila_siguiente$HoraInicio < fila_actual$HoraFin) {
      # sumamos 24 horas en segundos
      tiempoAct <- as.numeric(
      difftime(fila_siguiente$HoraInicio + (24*60*60), fila_actual$HoraFin, units="mins"))
    } else {
      tiempoAct <- as.numeric(
      difftime(fila_siguiente$HoraInicio, fila_actual$HoraFin, units="mins"))
    }
    
    # Si es tipo 4 pero el tiempo de act es 0, lo tomamos como tipo 3
    if (tiempoAct > 0){
      actividades_journey_actual <- rbind(actividades_journey_actual, data.frame(
        motivo = fila_actual$"Motivo del Viaje", 
        tiempo = tiempoAct,
        persona = fila_actual$"Identificación"
      ))
    }
  }

  
  # --- LÓGICA DE FIN DE JOURNEY ---
  # Un journey termina si:
  # 1. El viaje actual era 'Volver a Casa' (o a otro 'lugarBase')
  # 2. El siguiente viaje es de una persona diferente.
  
  if (es_viaje_a_base || es_persona_diferente) {
    
    # --- PROCESAR EL JOURNEY ACUMULADO ---
    
    # ¿El journey tocó una base intermedia?
    # (Usamos 'nrow' en lugar de 'length' - FIX 4)
    toco_base_intermedia <- any(actividades_journey_actual$motivo %in% lugaresBase)
    
    if (toco_base_intermedia) {
      # --- Tipos 3, 4, 5 (Home-Base-...) ---
      
      if (nrow(actividades_journey_actual) == 1) {
        # TIPO 3: Home-Base-Home (1 sola actividad = en la base)
        journalTipo3(tiempoViajes_journey_actual, actividades_journey_actual)
        
      } else {
        # TIPO 4 o 5: Home-Base-Activity...-Home
        # (Tu lógica interna del 'for (j in...' era para esto)
        # Es más simple guardarlos para el final.
        
        # Guardamos en la lista (reemplaza tu 'rbind' a 'journeys4y5')
        journeys4y5_pendientes[[length(journeys4y5_pendientes) + 1]] <- list(
          viajes = tiempoViajes_journey_actual,
          actividades = actividades_journey_actual,
          tipo_estimado = ifelse(nrow(actividades_journey_actual) == 2, 4, 5) # Estimación
        )
      }
      
    } else {
      # --- Tipos 1, 2 (Home-Activity-...) ---
      
      # (Tu lógica para Tipos 1 y 2 estaba perfecta)
      if (length(tiempoViajes_journey_actual) == 2) {
        # TIPO 1: Home-Activity-Home (2 viajes, 1 actividad)
        journalTipo1(tiempoViajes_journey_actual, actividades_journey_actual)
        
      } else if (length(tiempoViajes_journey_actual) > 2) {
        # TIPO 2: Home-Activity1-Activity2...-Home
        journalTipo2(tiempoViajes_journey_actual, actividades_journey_actual)
      }
      # Si length < 2 (solo 1 viaje), es un journey incompleto. Lo ignoramos.
    }
    
    # --- REINICIAR ACUMULADORES ---
    # (Este es el FIX 3 - Reseteo para TODOS los tipos)
    tiempoViajes_journey_actual <- c()
    actividades_journey_actual <- data.frame(
      motivo = character(),
      tiempo = numeric(),
      persona = character()
    )
  }
} # --- FIN DEL LOOP FOR ---

print("Procesamiento de journeys terminado.")
print(paste(length(journeys4y5_pendientes), "journeys Tipo 4/5 guardados para el final."))

# NOTA: Este loop no procesa el ÚLTIMO viaje de la ÚLTIMA persona.
# Generalmente no es un problema si los datos terminan limpiamente,
# pero es algo a tener en cuenta.

# --- PASO 5: Procesar Journeys Tipo 4 y 5 ---
# (Este paso te faltaba. Es necesario ejecutarlo después del loop)

print("Iniciando procesamiento de Journeys Tipo 4 y 5...")

if (length(journeys4y5_pendientes) > 0) {
  for (j in journeys4y5_pendientes) {
    # Extraemos las actividades que NO son base
    actividades_no_base <- j$actividades %>% filter(!motivo %in% lugaresBase)
    
    if (nrow(actividades_no_base) == 1) {
      # TIPO 4: Home-Base-Act1-Home (o H-A1-B-H)
      journalTipo4(j$viajes, j$actividades)
    } else if (nrow(actividades_no_base) > 1) {
      # TIPO 5: Home-Base-Act1-Act2...-Home
      journalTipo5(j$viajes, j$actividades)
    }
    # Si nrow == 0, es un H-B1-B2-H, lo ignoramos.
  }
}

print("Procesamiento de Journeys Tipo 4 y 5 terminado.")

# --- PASO 6: Calcular Resultados Finales ---
# (Este paso te faltaba. El 'promedioPorPersona' actual
# tiene la SUMA, no el promedio)

print("Calculando promedios finales...")

# 1. Promedio por Persona (el que te interesa)
promedioPorPersona_final <- promedioPorPersona %>%
  filter(n > 0) %>% # Asegurarnos de no dividir por cero
  mutate(tiempo_promedio = tiempo / n) %>%
  select(persona, motivo, tiempo_promedio, tiempo_total = tiempo, n_viajes = n) %>%
  arrange(persona, motivo)

print("--- Promedio Final por Persona y Motivo (Primeras 20 filas) ---")
print(head(promedioPorPersona_final, 20))


# 2. Promedio General (para toda la base)
promedioGeneral_final <- promedioPorPersona %>%
  group_by(motivo) %>%
  summarize(
    tiempo_total_general = sum(tiempo),
    n_total_general = sum(n)
  ) %>%
  filter(n_total_general > 0) %>%
  mutate(tiempo_promedio_general = tiempo_total_general / n_total_general)

print("--- Promedio General (Toda la Base) ---")
print(promedioGeneral_final)

print("--- SCRIPT COMPLETADO ---")


#Ahora guardamos los resultados en un excel:
library(writexl)

write_xlsx(
  list(
    Promedio_Por_Persona = promedioPorPersona_final,
    Promedio_General = promedioGeneral_final
  ),
  path = "resultados_viajes.xlsx"
)