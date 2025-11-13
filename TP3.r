library(readxl)

#ruta <- readline(prompt = "Ingrese la ruta del archivo datos_Tp3.xlsx: ")
#print("Accediendo al archivo...")
datos <- read_excel("C:/FACULTAD/Cuarto/Optativa/datos_Tp3.xlsx")

promedio_emociones <- data.frame(
  persona = character(),
  actividad = character(),
  emocion = character (),
  suma_ponderacion = numeric(),
  suma_minutos = numeric()
)

#Vamos a iterar por las columnas 3, 5 - 11 que son las emociones
indices <- c(3, 5, 6, 7, 8, 9, 10, 11)

for (i in 1:(nrow(datos))) {
    print("fila")
    persona <- datos$identificacion[i]
    actividad <- datos$"Etiqueta Actividad Resumida"[i]
    tiempo_act <- datos$"Total en minutos de la actividad"[i]

    for (j in indices) {

        #Si la actividad no requiere interaccion, no calculamos la emocion "a gusto"
        calcular <- TRUE
        if(j == 3){
            if (datos$"Etiqueta Interacción"[i] == "Estaba solo"){
                calcular <- FALSE
            }
        }

        if(calcular){
            emocion <- as.numeric(datos[[i, j]])
            ponderacion <- tiempo_act * emocion
            # Buscamos si ya hay fila de esa persona y actividad
            idx <- which(promedio_emociones$persona == persona & promedio_emociones$actividad == actividad & promedio_emociones$emocion == colnames(datos)[j])
            if (length(idx) > 0) {
                promedio_emociones$suma_ponderacion[idx] <- promedio_emociones$suma_ponderacion[idx] + ponderacion
                promedio_emociones$suma_minutos[idx] <- promedio_emociones$suma_minutos[idx] + tiempo_act
            }else{
                promedio_emociones <- add_row(
                    promedio_emociones,
                    persona = persona,
                    actividad = actividad,
                    emocion = colnames(datos)[j],
                    suma_ponderacion = ponderacion,
                    suma_minutos = tiempo_act
                )
            }
        }
    }
}

print("Calculando promedios de ponderacion...")

# Promedio por Persona - act - emocion
promedio_emociones_final <- promedio_emociones %>%
  filter(suma_minutos > 0) %>% # Asegurarnos de no dividir por cero
  mutate(ponderacion_promedio = suma_ponderacion / suma_minutos ) %>%
  select(persona, actividad, emocion, ponderacion_promedio) %>%
  arrange(persona, actividad, emocion)

print("--- Promedio Final por Persona y Motivo (Primeras 20 filas) ---")
print(head(promedio_emociones_final, 20))

# Promedio general por act - emocion
library(dplyr)

promedio_emociones_general <- promedio_emociones_final %>%
  group_by(actividad, emocion) %>%
  summarise(promedio_general = mean(ponderacion_promedio, na.rm = TRUE)) %>%
  arrange(actividad, emocion)

print("--- Promedio General por Actividad y Emoción ---")
print(promedio_general)


#Ahora guardamos los resultados en un excel:
library(writexl)

write_xlsx(
  list(
    Promedio_Por_Persona = promedio_emociones_final,
    Promedio_General = promedio_emociones_general
  ),
  path = "resultados_emociones.xlsx"
)
