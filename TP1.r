library(jsonlite)
library(dplyr)
library(data.table)
library(ggplot2)
library(readxl)


#Leemos la hoja 2 del excel con los datos para el practico
datos <- read_excel("C:/FACULTAD/Cuarto/Optativa/datos_Tp1.xlsx", sheet = 2)

#Agregamos una columna con la duracion del viaje en minutos
datos <- datos %>% mutate(
  Duracion = as.numeric(difftime(HoraFin, HoraInicio, units = "mins"))
)

#Eliminamos las filas donde la duracion es NA
datos <- datos[!is.na(datos$Duracion), ]

#Buscamos duraciones negativas y las recalculamos
for (i in 1:(nrow(datos) - 1)) {
  if (datos$Duracion[i] < 0) {
    # Hubo cambio de dia, sumamos 24 horas en segundos
    nuevaHoraInicio <- datos$HoraInicio[i + 1] + (24 * 60 * 60)
    datos$Duracion[i] <-
      as.numeric(difftime(nuevaHoraInicio, datos$HoraFin[i], units = "mins"))
  }
}

#Inicializamos arreglos a utilizar
promedio <- data.frame(
  motivo <- c(
    "Al Trabajo",
    "Belleza personal (peluquería, manicura, peinado)",
    "Buscar o dejar algo",
    "Comer o tomar algo",
    "Culto, prácticas religión",
    "De compras",
    "De salud",
    "Dejar/Buscar a Alguien",
    "Otra cosa",
    "Por estudio",
    "Por Trabajo",
    "Practicar deportes, correr, caminar",
    "Recreación",
    "Trámites",
    "Ver a alguien",
    "Volver a Casa"
  ),
  tiempoProm = rep(0, length(motivos)),
)

lugaresBase <- c("Volver a Casa", "Al Trabajo", "Por trabajo", "Por Estudio")

tiempoViajes <- c()

actividades <- data.frame(
  motivo = character(),
  tiempo = numeric(),
)

#Identificamos los tipos de journal
for (i in 1:(nrow(datos) - 1)) {
  if (datos$"Motivo del Viaje" != "Volver a Casa") {
    # Calculo el tiempo de actividad. Revisar si hubo cruce de medianoche
    if (datos$HoraInicio[i+1] < datos$HoraFin[i]) {
      # sumamos 24 horas en segundos
      tiempoAct <- as.numeric(
      difftime(datos$HoraInicio[i+1] + (24*60*60), datos$HoraFin[i], units="mins"))
    } else {
      tiempoAct <- as.numeric(
      difftime(datos$HoraInicio[i+1], datos$HoraFin[i], units="mins"))
    }

    #Agregar el Ti, agregar el tiempo de Acti
    actividades <- rbind(actividades, data.frame(motivo = datos$"Motivo del Viaje", tiempo = tiempoAct))
    tiempoViajes[[length(tiempoViajes) + 1]] <- datos$"Duracion"
  } else {
    #agrego el tiempo de volver a casa
    tiempoViajes[[length(tiempoViajes) + 1]] <- datos$"Duracion"

    #Identificar tipo de journal y actualizar promedio con las funciones
    if (any(actividades$motivo %in% lugaresBase)){
      #Puede ser tipo 3, 4 o 5
      if (length(tiempoViajes) == 2) {
        journalTipo3(tiempoViajes, actividades)
      } else {
        if (lenght(tiempoViajes) == 3) {
          journalTipo4(tiempoViajes, actividades)
        } else {
          journalTipo5(tiempoViajes, actividades)
        }
      }
    } else {
      #Puede ser tipo 1 o 2
      if (length(tiempoViajes) == 2) {
        journalTipo1(tiempoViajes, actividades)
      } else {
        journalTipo2(tiempoViajes, actividades)
      }
      #Reiniciar los arreglos de tiempos y de actividades
      tiempoViajes <- c()

      actividades <- data.frame(
        motivo = character(),
        tiempo = numeric(),
      )
    }
  }
}

#Estas funciones actualizan el promedio por motivo de viaje
journalTipo1 <- function(tiempoViajes, actividades) {}
journalTipo2 <- function(tiempoViajes, actividades) {}
journalTipo3 <- function(tiempoViajes, actividades) {}
journalTipo4 <- function(tiempoViajes, actividades) {}
journalTipo5 <- function(tiempoViajes, actividades) {}

View(datos)