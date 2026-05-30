# ------------------------------------------------------------
#Bioestadistica en R del 29/5/2026
# Laboratorio 04
#RESPIRACION DEL SUELO EN BOSQUES
#Beatriz García Valenciano
# ------------------------------------------------------------

# ------------------------------------------------------------
# 1. PAQUETES
# ------------------------------------------------------------

library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)

# ------------------------------------------------------------
# 2. IMPORTAR DATOS
# ------------------------------------------------------------

datos <- read_excel(
  "Lab04/04_respiracion_suelo_bosques.xlsx"
)

# ------------------------------------------------------------
# 3. RENOMBRAR VARIABLES
# ------------------------------------------------------------

names(datos) <- c(
  "plot_id",
  "site",
  "block",
  "land_use",
  "successional_age",
  "soil_respiration",
  "soil_temp",
  "soil_moisture",
  "ph",
  "organic_matter",
  "soil_c",
  "soil_n",
  "cn_ratio",
  "bulk_density",
  "canopy_cover",
  "litter_depth",
  "litter_mass",
  "root_biomass",
  "microbial_biomass",
  "enzyme_activity",
  "decomposition",
  "basal_area",
  "tree_density",
  "species_richness",
  "shannon",
  "soil_fauna",
  "high_respiration"
)

# ------------------------------------------------------------
# 4. LIMPIEZA GENERAL
# ------------------------------------------------------------

datos <- datos %>%
  mutate(
    soil_respiration = as.numeric(soil_respiration),
    soil_moisture = as.numeric(soil_moisture),
    ph = as.numeric(ph),
    
    plot_id = factor(plot_id),
    site = factor(site),
    block = factor(block)
  )

# ------------------------------------------------------------
# 5. CORREGIR VALORES ERRÓNEOS
# ------------------------------------------------------------

datos$soil_moisture[
  datos$soil_moisture == -999
] <- NA

# ------------------------------------------------------------
# 6. ESTANDARIZAR COBERTURAS
# ------------------------------------------------------------

datos$land_use <- trimws(
  tolower(
    as.character(datos$land_use)
  )
)

datos$land_use[
  grepl("degrad", datos$land_use)
] <- "Degradado"

datos$land_use[
  grepl("second", datos$land_use) |
    grepl("secund", datos$land_use)
] <- "Bosque secundario"

datos$land_use[
  grepl("primary", datos$land_use) |
    grepl("primry", datos$land_use)
] <- "Bosque primario"

# intentar clasificar registros faltantes usando edad sucesional

datos$land_use[
  is.na(datos$land_use) &
    datos$successional_age < 10
] <- "Degradado"

datos$land_use[
  is.na(datos$land_use) &
    datos$successional_age >= 10 &
    datos$successional_age < 60
] <- "Bosque secundario"

datos$land_use[
  is.na(datos$land_use) &
    datos$successional_age >= 60
] <- "Bosque primario"

datos$land_use <- factor(
  datos$land_use,
  levels = c(
    "Degradado",
    "Bosque secundario",
    "Bosque primario"
  )
)

# ------------------------------------------------------------
# 7. VARIABLE BINARIA
# ------------------------------------------------------------

datos$high_respiration <- ifelse(
  toupper(datos$high_respiration) == "YES",
  "Yes",
  "No"
)

datos$high_respiration <- factor(
  datos$high_respiration
)

# ------------------------------------------------------------
# 8. REVISIÓN
# ------------------------------------------------------------

cat("\nVALORES FALTANTES\n")

colSums(is.na(datos))

summary(datos)

# ------------------------------------------------------------
# 9. BASE LIMPIA PARA ANÁLISIS
# ------------------------------------------------------------

datos_limpios <- datos %>%
  filter(
    !is.na(soil_respiration),
    !is.na(soil_moisture),
    !is.na(ph),
    !is.na(land_use)
  )

# ------------------------------------------------------------
# 10. ESTADÍSTICA DESCRIPTIVA
# ------------------------------------------------------------

resumen <- datos_limpios %>%
  group_by(land_use) %>%
  summarise(
    n = n(),
    media = mean(soil_respiration),
    sd = sd(soil_respiration),
    minimo = min(soil_respiration),
    maximo = max(soil_respiration)
  )

print(resumen)

# ------------------------------------------------------------
# 11. HISTOGRAMA
# ------------------------------------------------------------

ggplot(
  datos_limpios,
  aes(x = soil_respiration)
) +
  geom_histogram(
    bins = 12,
    fill = "steelblue",
    color = "white"
  ) +
  theme_minimal()

# ------------------------------------------------------------
# 12. BOXPLOT
# ------------------------------------------------------------

ggplot(
  datos_limpios,
  aes(
    land_use,
    soil_respiration,
    fill = land_use
  )
) +
  geom_boxplot() +
  theme_minimal()

# ------------------------------------------------------------
# 13. ANOVA
# ------------------------------------------------------------

anova_resp <- aov(
  soil_respiration ~ land_use,
  data = datos_limpios
)

summary(anova_resp)

TukeyHSD(anova_resp)

# ------------------------------------------------------------
# 14. MATRIZ DE CORRELACIÓN
# ------------------------------------------------------------

numericas <- datos_limpios %>%
  select(
    successional_age,
    soil_respiration,
    soil_temp,
    soil_moisture,
    ph,
    organic_matter,
    soil_c,
    soil_n,
    cn_ratio,
    bulk_density,
    canopy_cover,
    litter_depth,
    litter_mass,
    root_biomass,
    microbial_biomass,
    enzyme_activity,
    decomposition,
    basal_area,
    tree_density,
    species_richness,
    shannon,
    soil_fauna
  )

correlacion <- cor(
  numericas,
  use = "complete.obs"
)

round(correlacion,2)

# ------------------------------------------------------------
# 15. RELACIONES BIVARIADAS
# ------------------------------------------------------------

ggplot(
  datos_limpios,
  aes(
    soil_temp,
    soil_respiration
  )
) +
  geom_point() +
  geom_smooth(
    method = "lm",
    se = FALSE
  ) +
  theme_minimal()

ggplot(
  datos_limpios,
  aes(
    soil_moisture,
    soil_respiration
  )
) +
  geom_point() +
  geom_smooth(
    method = "lm",
    se = FALSE
  ) +
  theme_minimal()

ggplot(
  datos_limpios,
  aes(
    microbial_biomass,
    soil_respiration
  )
) +
  geom_point() +
  geom_smooth(
    method = "lm",
    se = FALSE
  ) +
  theme_minimal()

# ------------------------------------------------------------
# 16. ACP
# ------------------------------------------------------------

pca_base <- datos_limpios %>%
  select(
    land_use,
    successional_age,
    soil_respiration,
    soil_temp,
    soil_moisture,
    ph,
    organic_matter,
    soil_c,
    soil_n,
    cn_ratio,
    bulk_density,
    canopy_cover,
    litter_depth,
    litter_mass,
    root_biomass,
    microbial_biomass,
    enzyme_activity,
    decomposition,
    basal_area,
    tree_density,
    species_richness,
    shannon,
    soil_fauna
  )

grupo_pca <- pca_base$land_use

pca_base <- pca_base %>%
  select(-land_use)

acp <- prcomp(
  pca_base,
  center = TRUE,
  scale. = TRUE
)

summary(acp)

# ------------------------------------------------------------
# 17. SCREE PLOT
# ------------------------------------------------------------

plot(
  acp,
  type = "lines"
)

# ------------------------------------------------------------
# 18. BIPLOT
# ------------------------------------------------------------

biplot(
  acp,
  cex = 0.7
)

# ------------------------------------------------------------
# 19. PC1 VS PC2
# ------------------------------------------------------------

scores <- as.data.frame(
  acp$x
)

scores$land_use <- grupo_pca

ggplot(
  scores,
  aes(
    PC1,
    PC2,
    color = land_use
  )
) +
  geom_point(size = 3) +
  theme_minimal()

# ------------------------------------------------------------
# 20. CLUSTER JERÁRQUICO
# ------------------------------------------------------------

distancias <- dist(
  scale(pca_base)
)

cluster <- hclust(
  distancias,
  method = "ward.D2"
)

plot(cluster)

rect.hclust(
  cluster,
  k = 3,
  border = "red"
)

# ------------------------------------------------------------
# 21. K-MEANS
# ------------------------------------------------------------

set.seed(123)

k3 <- kmeans(
  scale(pca_base),
  centers = 3,
  nstart = 50
)

table(
  k3$cluster,
  grupo_pca
)

# ------------------------------------------------------------
# 22. VISUALIZACIÓN CLUSTERS
# ------------------------------------------------------------

scores$cluster <- factor(
  k3$cluster
)

ggplot(
  scores,
  aes(
    PC1,
    PC2,
    color = cluster
  )
) +
  geom_point(size = 3) +
  theme_minimal()

# ------------------------------------------------------------
# 23. REGRESIÓN MÚLTIPLE
# ------------------------------------------------------------

modelo_completo <- lm(
  soil_respiration ~
    soil_temp +
    soil_moisture +
    soil_c +
    soil_n +
    canopy_cover +
    litter_mass +
    root_biomass +
    microbial_biomass +
    enzyme_activity +
    decomposition,
  data = datos_limpios
)

summary(modelo_completo)

# ------------------------------------------------------------
# 24. DIAGNÓSTICOS
# ------------------------------------------------------------

par(mfrow = c(2,2))
plot(modelo_completo)
par(mfrow = c(1,1))


# ------------------------------------------------------------
# FIN
# ------------------------------------------------------------