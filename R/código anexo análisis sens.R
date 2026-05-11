# Paquetes
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(janitor)
library(naniar)
library(stringr)
library(knitr)
library(kableExtra)
library(purrr)
library(FactoMineR)
library(factoextra)
library(cluster)
library(ggrepel)

# install.packages("here")  
library(here)

path <- here("data", "BASE DE DATOS SIN IMPUTAR.xlsx")
df_raw_anex <- read_excel(path, sheet = "BASE")

df_anex <- df_raw_anex %>%
  janitor::clean_names()
names(df_anex)

df_anex <- df_anex %>%
  rename(
    pais                 = pais,  
    gdp_pc_ppp       = pib_per_capita_fmi_2023,
    income_group     = nivel_de_ingreso_2023,
    region                = region,
    unemp_rate       = desempleo_percent_total_2023,
    elec_access     = acceso_a_electricidad_percent_total_2023,
    elec_kwh_pc     = consumo_de_energia_electrica_k_wh_per_capita_2023,
    rural_pct     = poblacion_rural_percent_de_la_poblacion_total_2023,
    ghg_pc         = emisiones_totales_gases_efecto_invernadero_per_capita_c02e_capita_2023,
    lifeexp         = esperanza_de_vida_al_nacer_anos_media_entre_h_y_m_2023,
    f_m_tertiary     = mujeres_vs_hombres_en_eduacion_terciaria_2023,
    secure_servers = servidores_de_internet_seguros_por_cada_millon_de_personas_2023,
    infra_lpi = calidad_de_la_infraestructura_relacionada_con_el_comercio_y_el_transporte_2022,
    mat_mort         = tasa_de_mortalidad_materna_por_cada_100000_nacidos_vivos_2023,
    physicians = medicos_por_cada_1000_personas_2023,
    water_access = acceso_a_agua_potable_percent_poblacion_2023,
    hdi    = nivel_de_desarrollo_humano_segun_la_onu_2023
  )
names(df_anex)

# PRIMERO: para imputación con la media.
df_mean_anex <- df_anex

for (col in names(df_mean_anex)) {
  if (is.numeric(df_mean_anex[[col]])) {
    df_mean_anex[[col]][is.na(df_mean_anex[[col]])] <- mean(df_mean_anex[[col]], na.rm = TRUE)
  }
}

#comprobación de que se han imputado todos
na_por_variable_anex <- df_mean_anex %>%
  summarise(across(-any_of("pais"), ~ sum(is.na(.)))) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "na_count") %>%
  mutate(na_pct = round(100 * na_count / nrow(df_mean_anex), 2)) %>%
  arrange(desc(na_count))

#transformar y estandarizar
vars_log_claras_anex <- c(
  "gdp_pc_ppp",
  "secure_servers",
  "elec_kwh_pc",
  "ghg_pc",
  "mat_mort"
)

df_mean_anex <- df_mean_anex %>%
  mutate(
    across(
      all_of(c(vars_log_claras_anex)),
      ~ log1p(.x),
      .names = "{.col}_trans"
    )
  )
names(df_mean_anex)[grepl("_trans$", names(df_mean_anex))]

vars_anex <- c(
  "elec_access",
  "water_access",
  "unemp_rate",
  "physicians",
  "f_m_tertiary",
  "hdi",
  "infra_lpi",
  "lifeexp",
  "rural_pct",
  "gdp_pc_ppp_trans",
  "secure_servers_trans",
  "elec_kwh_pc_trans",
  "ghg_pc_trans",
  "mat_mort_trans"
)

df_mean_anex <- df_mean_anex %>%
  mutate(
    across(
      all_of(vars_anex),
      ~ as.numeric(scale(.x)),
      .names = "{.col}_std"
    )
  )

#pca
vars_pca_anex <- c(
  "gdp_pc_ppp_trans_std",
  "elec_kwh_pc_trans_std",
  "secure_servers_trans_std",
  "ghg_pc_trans_std",
  "mat_mort_trans_std",
  "elec_access_std",
  "water_access_std",
  "unemp_rate_std",
  "physicians_std",
  "f_m_tertiary_std",
  "infra_lpi_std",
  "lifeexp_std",
  "rural_pct_std"
)

res_pca <- PCA(
  df_mean_anex %>% select(all_of(vars_pca_anex)),
  scale.unit = FALSE,
  graph = FALSE
)

#devuelve los autovalores y los porcentajes de varianza explicada
eig_val <- get_eigenvalue(res_pca)
eig_val

#scree plot
max_var <- max(res_pca$eig[,2])

fviz_eig(
  res_pca,
  addlabels = TRUE,
  ylim = c(0, max_var + 5),
  barfill = "purple",
  barcolor = "purple"
)

#círculo de correlaciones
# Gráfico base sin etiquetas
p <- fviz_pca_var(
  res_pca,
  col.var = "contrib",
  gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
  label = "none"
)

# Extraer info
var <- get_pca_var(res_pca)

# Top 8 variables (según contribución total Dim1+Dim2)
contrib_total <- var$contrib[,1] + var$contrib[,2]
top_vars <- names(sort(contrib_total, decreasing = TRUE))[1:8]

# Crear data frame SOLO con esas variables
df_labels <- as.data.frame(var$coord[top_vars, ])
df_labels$varname <- rownames(df_labels)

# Añadir etiquetas
p + geom_text_repel(
  data = df_labels,
  aes(x = Dim.1, y = Dim.2, label = varname),
  size = 4
) +
  theme(
    legend.position = "bottom"
  )


#cluster
coord_paises <- as.data.frame(res_pca$ind$coord) %>%
  mutate(
    pais = df_mean_anex$pais,
    region = df_mean_anex$region,
    income_group = df_mean_anex$income_group
  )

# Nos quedamos con las dos primeras componentes
X_clust_anex <- coord_paises %>%
  select(Dim.1, Dim.2)

#método del codo
fviz_nbclust(X_clust_anex, kmeans, method = "wss") +
  labs(title = "Método del codo para k-means")

#silueta
fviz_nbclust(X_clust_anex, kmeans, method = "silhouette") +
  labs(title = "Índice silhouette para k-means")

#k = 2 y k = 3 automatizado
set.seed(123)
kmeans_res_2 <- kmeans(X_clust_anex, centers = 2, nstart = 25)
kmeans_res_3 <- kmeans(X_clust_anex, centers = 3, nstart = 25)

#visualizar con factoextra k = 2
fviz_cluster(
  kmeans_res_2,
  data = X_clust_anex,
  geom = "point",
  ellipse.type = "convex",
  ggtheme = theme_minimal()
)

#visualizar con factoextra k = 3
fviz_cluster(
  kmeans_res_3,
  data = X_clust_anex,
  geom = "point",
  ellipse.type = "convex",
  ggtheme = theme_minimal()
)

#SEGUNDO: eliminación de países con nulos.

df_complete_anex <- na.omit(df_anex)

#transformar y estandarizar
vars_log_claras_anex <- c(
  "gdp_pc_ppp",
  "secure_servers",
  "elec_kwh_pc",
  "ghg_pc",
  "mat_mort"
)

df_complete_anex <- df_complete_anex %>%
  mutate(
    across(
      all_of(c(vars_log_claras_anex)),
      ~ log1p(.x),
      .names = "{.col}_trans"
    )
  )
names(df_complete_anex)[grepl("_trans$", names(df_complete_anex))]

vars_anex <- c(
  "elec_access",
  "water_access",
  "unemp_rate",
  "physicians",
  "f_m_tertiary",
  "hdi",
  "infra_lpi",
  "lifeexp",
  "rural_pct",
  "gdp_pc_ppp_trans",
  "secure_servers_trans",
  "elec_kwh_pc_trans",
  "ghg_pc_trans",
  "mat_mort_trans"
)

df_complete_anex <- df_complete_anex %>%
  mutate(
    across(
      all_of(vars_anex),
      ~ as.numeric(scale(.x)),
      .names = "{.col}_std"
    )
  )

#pca
vars_pca_anex <-c(
  "gdp_pc_ppp_trans_std",
  "elec_kwh_pc_trans_std",
  "secure_servers_trans_std",
  "ghg_pc_trans_std",
  "mat_mort_trans_std",
  "elec_access_std",
  "water_access_std",
  "unemp_rate_std",
  "physicians_std",
  "f_m_tertiary_std",
  "infra_lpi_std",
  "lifeexp_std",
  "rural_pct_std"
)

res_pca <- PCA(
  df_complete_anex %>% select(all_of(vars_pca_anex)),
  scale.unit = FALSE,
  graph = FALSE
)

#devuelve los autovalores y los porcentajes de varianza explicada
eig_val <- get_eigenvalue(res_pca)
eig_val

#scree plot
max_var <- max(res_pca$eig[,2])

fviz_eig(
  res_pca,
  addlabels = TRUE,
  ylim = c(0, max_var + 5),
  barfill = "lightblue",
  barcolor = "lightblue"
)

#círculo de correlaciones
# Gráfico base sin etiquetas
p <- fviz_pca_var(
  res_pca,
  col.var = "contrib",
  gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
  label = "none"
)

# Extraer info
var <- get_pca_var(res_pca)

# Top 8 variables (según contribución total Dim1+Dim2)
contrib_total <- var$contrib[,1] + var$contrib[,2]
top_vars <- names(sort(contrib_total, decreasing = TRUE))[1:8]

# Crear data frame SOLO con esas variables
df_labels <- as.data.frame(var$coord[top_vars, ])
df_labels$varname <- rownames(df_labels)

# Añadir etiquetas
p + geom_text_repel(
  data = df_labels,
  aes(x = Dim.1, y = Dim.2, label = varname),
  size = 4
) +
  theme(
    legend.position = "bottom"
  )


#cluster
coord_paises <- as.data.frame(res_pca$ind$coord) %>%
  mutate(
    pais = df_complete_anex$pais,
    region = df_complete_anex$region,
    income_group = df_complete_anex$income_group
  )

# Nos quedamos con las dos primeras componentes
X_clust_anex <- coord_paises %>%
  select(Dim.1, Dim.2)

#método del codo
fviz_nbclust(X_clust_anex, kmeans, method = "wss") +
  labs(title = "Método del codo para k-means")

#silueta
fviz_nbclust(X_clust_anex, kmeans, method = "silhouette") +
  labs(title = "Índice silhouette para k-means")

#k = 2 y k = 3 automatizado
set.seed(123)
kmeans_res_2 <- kmeans(X_clust_anex, centers = 2, nstart = 25)
kmeans_res_3 <- kmeans(X_clust_anex, centers = 3, nstart = 25)

#visualizar con factoextra k = 2
fviz_cluster(
  kmeans_res_2,
  data = X_clust_anex,
  geom = "point",
  ellipse.type = "convex",
  ggtheme = theme_minimal()
)

#visualizar con factoextra k = 3
fviz_cluster(
  kmeans_res_3,
  data = X_clust_anex,
  geom = "point",
  ellipse.type = "convex",
  ggtheme = theme_minimal()
)
