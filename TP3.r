library(readxl)
library(ggplot2)

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
    persona <- datos$identificacion[i]
    actividad <- datos$"Etiqueta Actividad Resumida"[i]
    tiempo_act <- datos$"Total en minutos de la actividad"[i]
    if (is.na(actividad)  | is.na(tiempo_act)){
        next  # Salta a la siguiente iteración del bucle
    }

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
            if (is.na(emocion)) {
                next  # Salta a la siguiente iteración del bucle
            }
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
print(promedio_emociones_general)


#Ahora guardamos los resultados en un excel:
library(writexl)

write_xlsx(
  list(
    Promedio_Por_Persona = promedio_emociones_final,
    Promedio_General = promedio_emociones_general
  ),
  path = "resultados_emociones.xlsx"
)

#---------------------------------------------------------------------graficamos el promedio general
#Renombro algunas actividades para mejorar visualizacion del grafico
promedio_emociones_general <- promedio_emociones_general %>%
  mutate(actividad = ifelse(actividad == "Dedicación personal (comió, bañó, descansó)",
                         "Dedicación personal",
                         actividad))

promedio_emociones_general <- promedio_emociones_general %>%
  mutate(actividad = ifelse(actividad == "Entretenimiento (radio, televisión, computadora, juego en casa)",
                         "Entretenimiento",
                         actividad))

promedio_emociones_general <- promedio_emociones_general %>%
  mutate(actividad = ifelse(actividad == "Relaciones sociales (Charlar, cuidar, actividad religiosa, otras relaciones)",
                         "Relaciones sociales",
                         actividad))

promedio_emociones_general <- promedio_emociones_general %>%
  mutate(actividad = ifelse(actividad == "Tareas domésticas (comida, cuida a los hijos, mascotas)",
                         "Tareas domesticas",
                         actividad))

promedio_emociones_general <- promedio_emociones_general %>%
  mutate(actividad = ifelse(actividad == "Viajó a algún lado (coche, caminando, bicicleta)",
                         "Viajo a algun lado",
                         actividad)) 

promedio_emociones_general <- promedio_emociones_general %>%
  mutate(emocion = ifelse(emocion == "A_gusto",
                         "A gusto",
                         emocion))                        

                         

# Definimos la paleta de colores para las barras
colores_paleta <- c(
 "#eed485ff", "#ED9E61", "#7CBAA6", "#D0E0E3", 
  "#559FCB", "#ae6e6eff", "#9b6383ff",   "#A77DC2"
)

#modifico el orden de aparicion de las emociones
orden_emociones <- c(
  "Disfrute",
  "A gusto",
  "Calma",
  "Preocupación",
  "Depresión",
  "Irritación",
  "Tensión",
  "Prisa"
)

promedio_emociones_general$emocion <- factor(
  promedio_emociones_general$emocion,
  levels = orden_emociones
)

grafico_emociones <- ggplot(promedio_emociones_general, 
                            aes(x = reorder(actividad, -promedio_general), 
                                y = promedio_general, 
                                fill = emocion)) +
  geom_col(position = "dodge", width = 0.7) +
  scale_fill_manual(values = colores_paleta) +
  labs(
    title = "Ponderación promedio general por actividad y emoción",
    x = "Tipo de actividad",
    y = "Ponderación promedio",
    fill = "Emoción"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.margin = margin(15, 20, 15, 20)
  )

ggsave("grafico_emociones.png", grafico_emociones, width = 10, height = 6)
