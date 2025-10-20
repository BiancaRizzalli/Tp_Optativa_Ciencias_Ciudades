### Codigo base



total\_act <- 0

fila\_salida <- 1

total\_act <- 0



for (i in 1:(nrow(datos)-1)) {  # <- importante, para no exceder i+1

&nbsp;   if (!(is.na(datos$HoraInicio\[i+1])) \&\& !(is.na(datos$HoraFin\[i]))){

&nbsp;       if (datos$"Motivo del Viaje"\[i] == "Volver a Casa"){

&nbsp;           total\_act <- 0

&nbsp;           # fila\_salida <- i+1   # opcional según tu lógica

&nbsp;       } else {

&nbsp;           # Revisar si hubo cruce de medianoche

&nbsp;           if (datos$HoraInicio\[i+1] < datos$HoraFin\[i]) {

&nbsp;           # sumamos 24 horas en segundos

&nbsp;           diff\_min <- as.numeric(difftime(datos$HoraInicio\[i+1] + 24\*60\*60, datos$HoraFin\[i], units="mins"))

&nbsp;           } else {

&nbsp;           diff\_min <- as.numeric(difftime(datos$HoraInicio\[i+1], datos$HoraFin\[i], units="mins"))

&nbsp;           }

&nbsp;           total\_act <- total\_act + diff\_min

&nbsp;           cat("FILA:", i, "Total act:", total\_act, "\\n")

&nbsp;       }

&nbsp;   }

}



### Dos filas con el mismo viaje, tiempo consecutivo y t. actividad = 0

\# Creamos un grupo que cambia cada vez que el motivo cambia

datos <- datos %>%

&nbsp; mutate(grupo = cumsum(`Motivo del Viaje` != lag(`Motivo del Viaje`, default = first(`Motivo del Viaje`))))



\# Resumimos por grupo

resumen <- datos %>%

&nbsp; group\_by(grupo, `Motivo del Viaje`) %>%

&nbsp; summarise(

&nbsp;   HoraInicio = first(HoraInicio),

&nbsp;   HoraFin = last(HoraFin),

&nbsp;   Duracion = sum(Duracion),

&nbsp;   .groups = "drop"

&nbsp; )



\# Reemplazamos datos por el resumen

datos <- resumen %>% select(-grupo)  # eliminamos la columna grupo si no la necesitamos





