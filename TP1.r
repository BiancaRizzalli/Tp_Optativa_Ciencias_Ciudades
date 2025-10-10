library(jsonlite)
library(dplyr)
library(data.table)
library(ggplot2)
library(readxl)

datos <- read_excel("C:/FACULTAD/Cuarto/Optativa/datos_Tp1.xlsx")


# Pasar a date las columnas de inicio y fin
datos$HoraInicio <- as.POSIXct(datos$HoraInicio, format="%H:%M")
datos$HoraFin    <- as.POSIXct(datos$HoraFin, format="%H:%M")
View(datos)

total_act <- 0
fila_salida <- 1
total_act <- 0

for (i in 1:(nrow(datos)-1)) {  # <- importante, para no exceder i+1
    if (!(is.na(datos$HoraInicio[i+1])) && !(is.na(datos$HoraFin[i]))){
        if (datos$"Motivo del Viaje"[i] == "Volver a Casa"){
            total_act <- 0
            # fila_salida <- i+1   # opcional según tu lógica
        } else {
            # Revisar si hubo cruce de medianoche
            if (datos$HoraInicio[i+1] < datos$HoraFin[i]) {
            # sumamos 24 horas en segundos
            diff_min <- as.numeric(difftime(datos$HoraInicio[i+1] + 24*60*60, datos$HoraFin[i], units="mins"))
            } else {
            diff_min <- as.numeric(difftime(datos$HoraInicio[i+1], datos$HoraFin[i], units="mins"))
            }
            total_act <- total_act + diff_min
            cat("FILA:", i, "Total act:", total_act, "\n")
        }
    }
}
