library(readxl)
library(readr)
library(ggplot2)
library(dplyr)

ruta <- readline(prompt = "Ingrese la ruta del archivo resultado_viajes.xlsx: ")
print("Accediendo al archivo...")
promedio_general <- read_excel(ruta, sheet = 2)
promedio_por_persona <- read_excel(ruta, sheet = 1)

ruta <- readline(prompt = "Ingrese la ruta del archivo datos_Tp2.csv: ")
print("Accediendo al archivo...")
datos_tp2 <- read_csv(ruta, show_col_types = FALSE)


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

# Agrupar todos los "Otra cosa: ..." en un solo motivo "Otra cosa"
promedio_general <- promedio_general %>%
  mutate(
    motivo_plot = ifelse(startsWith(motivo, "Otra cosa"), "Otra cosa", motivo)
  )

promedio_por_persona <- promedio_por_persona %>%
  mutate(
    motivo_plot = ifelse(startsWith(motivo, "Otra cosa"), "Otra cosa", motivo)
  )

 promedio_general_plot <- promedio_general %>%
  group_by(motivo_plot) %>%
  summarise(
    tiempo_promedio_general = mean(tiempo_promedio_general, na.rm = TRUE),
    .groups = "drop"
  )


 

print("Generando graficos...")

#----------------------------------------------------------------------------------Graficar promedio general

grafico_barras_general <- ggplot(promedio_general_plot, aes(x =reorder(motivo_plot, -tiempo_promedio_general), y = tiempo_promedio_general)) +
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
  group_by(motivo_plot, Género) %>%
  summarise(
    tiempo_promedio = mean(tiempo_promedio, na.rm = TRUE),
    n_personas = n(),
    .groups = "drop"
  )
#Creamos la manera de ordenar de mayor a menor
orden_motivos <- promedio_motivo_genero %>%
  group_by(motivo_plot) %>%
  summarise(promedio_total = mean(tiempo_promedio, na.rm = TRUE)) %>%
  arrange(desc(promedio_total)) %>%
  pull(motivo_plot) #Agarra la columna motivo como un vector


#Realizamos grafico de barra por motivo clasificando el genero
grafico_por_genero <- ggplot(promedio_motivo_genero, aes(x =factor( motivo_plot, levels = orden_motivos), y = tiempo_promedio, fill = Género)) +
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

#----------------------------------------------------------------------------------Graficar por educacion

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
  "#eed485ff", "#ED9E61", "#D0E0E3", "#7CBAA6", 
  "#559FCB", "#ae6e6eff", "#9b6383ff",  "#A77DC2"
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

#----------------------------------------------------------------------------------Graficar por trabajo

datos_trabajo <- promedio_por_persona %>%
  filter(motivo %in% c("Por Trabajo", "Al Trabajo")) %>%
  left_join(select(datos_tp2, Identificación, Ocupación),
            by = c("persona" = "Identificación"))

promedio_trabajo <- datos_trabajo %>%
  group_by(Ocupación) %>%
  summarise(promedio_viaje = mean(tiempo_promedio, na.rm = TRUE)) %>%
  arrange(desc(promedio_viaje))

grafico_trabajo <- ggplot(promedio_trabajo, aes(x = Ocupación, y = promedio_viaje, fill = Ocupación)) +
  geom_col(width = 0.6) +
  scale_fill_manual(values = colores_paleta) +
  labs(
    title = "Promedio de tiempo de viaje por trabajo según ocupación",
    x = "Ocupación",
    y = "Tiempo promedio (minutos)"
  ) +
  theme_minimal(base_size = 10) +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

ggsave("grafico_trabajo.png", grafico_trabajo, width = 10, height = 8)  # más ancho que alto

#---------------------------------------------------------------------------------------Graficar por edad

#Agrego la clasificacion de los promedios
promedio_por_persona <- promedio_por_persona %>%
  mutate(
    rango_tiempo = case_when(
      tiempo_promedio < 15 ~ "Menos de 15 min",
      tiempo_promedio < 30 ~ "15 a 29 min",
      tiempo_promedio < 60 ~ "30 a 59 min",
      TRUE ~ "60 min o más"
    )
  )

datos_viajes <- promedio_por_persona %>%
  left_join(select(datos_tp2, Identificación, Edad),
            by = c("persona" = "Identificación"))

#Calculo que porcentaje de personas pertenece a cada grupo etario.
distribucion_rangos <- datos_viajes %>%
  group_by(rango_tiempo, Edad) %>%
  summarise(cantidad = n(), .groups = "drop_last") %>%
  mutate(porcentaje = cantidad / sum(cantidad) * 100)

#Creamos el grafico de torta para cada rango de promedio
grafico_torta <- ggplot(distribucion_rangos, aes(x = "", y = porcentaje, fill = Edad)) +
  geom_col(width = 1, color = "white") +
  coord_polar(theta = "y") +
  facet_wrap(~ rango_tiempo) +
  scale_fill_manual(values = c("#ED9E61", "#D0E0E3", "#7CBAA6", "#559FCB", "#A77DC2")) +
  labs(
    title = "Distribución etaria por rango de tiempo de viaje",
    x = NULL,
    y = NULL,
    fill = "Grupo etario"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    plot.title = element_text(hjust = 0.5)
  )

ggsave("grafico_torta.png", grafico_torta, width = 10, height = 10)  # más ancho que alto