library(readxl)
library(readr)
library(ggplot2)
library(dplyr)

#ruta <- readline(prompt = "Ingrese la ruta del archivo resultado_viajes.xlsx: ")
print("Accediendo al archivo...")
#promedio_general <- read_excel(ruta, sheet = 2)
#promedio_por_persona <- read_excel(ruta, sheet = 1)

#ruta <- readline(prompt = "Ingrese la ruta del archivo datos_Tp2.csv: ")
print("Accediendo al archivo...")
#datos_tp2 <- read_csv(ruta, show_col_types = FALSE)

#----------------------------------------------------------despues borrar y dejar lo de arriba
promedio_general <- read_excel("C:/FACULTAD/Cuarto/Optativa/resultados_viajes.xlsx", sheet = 2)
promedio_por_persona <- read_excel("C:/FACULTAD/Cuarto/Optativa/resultados_viajes.xlsx", sheet = 1)
datos_tp2 <- read_csv("C:/FACULTAD/Cuarto/Optativa/datos_Tp2.csv", show_col_types = FALSE)


#--------------------------------------------------------------------Renombro algunos motivos para mejores graficos
promedio_general <- promedio_general %>%
  mutate(motivo = ifelse(motivo == "Belleza personal (peluquería, manicura, pedicura, etc.)",
                         "Belleza personal",
                         motivo))
promedio_general <- promedio_general %>%
  mutate(motivo = ifelse(motivo == "Practicar deportes, correr, caminar",
                         "Actividad física, deportes",
                         motivo))
promedio_por_persona <- promedio_por_persona %>%
  mutate(motivo = ifelse(motivo == "Belleza personal (peluquería, manicura, pedicura, etc.)",
                         "Belleza personal",
                         motivo))
promedio_por_persona <- promedio_por_persona %>%
  mutate(motivo = ifelse(motivo == "Practicar deportes, correr, caminar",
                         "Actividad física, deportes",
                         motivo))

print("Generando graficos...")


grafico_barras_general <- ggplot(promedio_general, aes(x =reorder(motivo, -tiempo_promedio_general), y = tiempo_promedio_general)) +
    geom_bar(stat = "identity", fill = "#559FCB", width = 0.6) +
    labs(
      title = "Tiempo promedio por motivo",
      x = "Motivo del viaje",
      y = "Tiempo promedio (minutos)"
    ) +
    theme_minimal(base_size = 13) +
    theme(
        axis.text.x = element_text(angle = 45, hjust = 1),  # gira los motivos a 45 grados
        plot.title = element_text(hjust = 0.5, face = "bold"),  # centra el título
        plot.margin = margin(15, 20, 15, 20)  # agrega márgenes externos
    )
ggsave("grafico_promedio_general.png", grafico_barras_general, width = 10, height = 6)  # más ancho que alto

#----------------------------------------------------------------------------------Graficar dividiendo en genero

# Unimos los dos dataframes
datos_combinados <- promedio_por_persona %>%
  left_join(datos_tp2 %>% select(Identificación, Género),
            by = c("persona" = "Identificación"))

#promedio por motivo y género
promedio_motivo_genero <- datos_combinados %>%
  filter(!is.na(Género)) %>%   #eliminar sin género
  group_by(motivo, Género) %>%
  summarise(
    tiempo_promedio = mean(tiempo_promedio, na.rm = TRUE),
    n_personas = n(),
    .groups = "drop"
  )
#Creamos la manera de ordenar de mayor a menor
orden_motivos <- promedio_motivo_genero %>%
  group_by(motivo) %>%
  summarise(promedio_total = mean(tiempo_promedio, na.rm = TRUE)) %>%
  arrange(desc(promedio_total)) %>%
  pull(motivo) #Agarra la columna motivo como un vector


#Realizamos grafico de barra por motivo clasificando el genero
grafico_por_genero <- ggplot(promedio_motivo_genero, aes(x =factor( motivo, levels = orden_motivos), y = tiempo_promedio, fill = Género)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.6) +
  labs(
    title = "Tiempo promedio por motivo y género",
    x = "Motivo del viaje",
    y = "Tiempo promedio (minutos)",
    fill = "Género"
  ) +
    scale_fill_manual(
    values = c("Femenino" = "#ED9E61", "Masculino" = "#7CBAA6")
  ) +
  theme_minimal(base_size = 13) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.margin = margin(15, 20, 15, 20)
)


ggsave("grafico_por_genero.png", grafico_por_genero, width = 10, height = 6)  # más ancho que alto

#----------------------------------------------------------------------------------Graficar dividiendo en genero

# Unimos los datasets
datos_estudio <- promedio_por_persona %>%
  filter(motivo == "Por estudio") %>%
  left_join(select(datos_tp2, Identificación, Educación),
            by = c("persona" = "Identificación"))

promedio_educacion <- datos_estudio %>%
  group_by(Educación) %>%
  summarise(promedio_viaje = mean(tiempo_promedio, na.rm = TRUE)) %>%
  arrange(desc(promedio_viaje))

#ordeno por nivel educativo
promedio_educacion$Educación <- factor(
  promedio_educacion$Educación,
  levels = c(
    "Primario Incompleto",
    "Primario Completo",
    "Secundario Incompleto",
    "Secundario Completo",
    "Terciario Incompleto",
    "Terciario Completo",
    "Universitario Incompleto",
    "Universitario Completo y más"
  )
)

# Definimos la paleta de colores para las barras
colores_paleta <- c(
  "#7CBAA6", "#559FCB", "#b1cfd4", "#E3C567",
  "#ED9E61","#bf6c6c", "#C97BA7", "#9F86C0"
)

grafico_educacion <- ggplot(promedio_educacion, aes(x = Educación, y = promedio_viaje, fill = Educación)) +
  geom_col(width = 0.6) +
  scale_fill_manual(values = colores_paleta) +
  labs(
    title = "Promedio de tiempo de viaje por estudio según nivel educativo",
    x = "Nivel educativo",
    y = "Tiempo promedio (minutos)"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

ggsave("grafico_educacion.png", grafico_educacion, width = 10, height = 6)  # más ancho que alto
