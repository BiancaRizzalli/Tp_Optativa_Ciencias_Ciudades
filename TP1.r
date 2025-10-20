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

#Recorremos los datos, identificando journals
lugaresBase <- c("Volver a Casa", "Al Trabajo", "Por Trabajo", "Por Estudio")

View(datos)
