#Paquetes
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

df_raw <- read_excel(path, sheet = "BASE")
df_imputed <- read_excel(path, sheet = "B. IMPUTADOS")

#df <- df_raw %>%
df <- df_imputed %>% 
  janitor::clean_names()
names(df)

df <- df %>%
  rename(
    pais                 = pais,  
    gdp_pc_ppp_2023       = pib_per_capita_fmi_2023,
    income_group_2023     = nivel_de_ingreso_2023,
    region                = region,
    unemp_rate_2023       = desempleo_percent_total_2023,
    elec_access_2023      = acceso_a_electricidad_percent_total_2023,
    elec_kwh_pc_2023      = consumo_de_energia_electrica_k_wh_per_capita_2023,
    rural_pct_2023        = poblacion_rural_percent_de_la_poblacion_total_2023,
    ghg_pc_2023           = emisiones_totales_gases_efecto_invernadero_per_capita_c02e_capita_2023,
    lifeexp_2023          = esperanza_de_vida_al_nacer_anos_media_entre_h_y_m_2023,
    f_m_tertiary_2023     = mujeres_vs_hombres_en_eduacion_terciaria_2023,
    secure_servers_pm_2023 = servidores_de_internet_seguros_por_cada_millon_de_personas_2023,
    infra_lpi_2022        = calidad_de_la_infraestructura_relacionada_con_el_comercio_y_el_transporte_2022,
    mat_mort_2023         = tasa_de_mortalidad_materna_por_cada_100000_nacidos_vivos_2023,
    physicians_per1k_2023 = medicos_por_cada_1000_personas_2023,
    water_access_2023     = acceso_a_agua_potable_percent_poblacion_2023,
    hdi_2023              = nivel_de_desarrollo_humano_segun_la_onu_2023
  )
names(df)

#EDA 

# TABLA DESCRIPTIVA EDA

# Selección variables numéricas del EDA
vars_eda <- c(
  "gdp_pc_ppp_2023",
  "unemp_rate_2023",
  "elec_access_2023",
  "elec_kwh_pc_2023",
  "rural_pct_2023",
  "ghg_pc_2023",
  "lifeexp_2023",
  "f_m_tertiary_2023",
  "secure_servers_pm_2023",
  "infra_lpi_2022",
  "mat_mort_2023",
  "physicians_per1k_2023",
  "water_access_2023",
  "hdi_2023"
)

# Funciones auxiliares para asimetría y curtosis 
skewness_manual <- function(x) {
  x <- x[!is.na(x)]
  n <- length(x)
  if (n < 3) return(NA_real_)
  m <- mean(x)
  s <- sd(x)
  if (s == 0) return(0)
  sum(((x - m) / s)^3) * n / ((n - 1) * (n - 2))
}

kurtosis_manual <- function(x) {
  x <- x[!is.na(x)]
  n <- length(x)
  if (n < 4) return(NA_real_)
  m <- mean(x)
  s <- sd(x)
  if (s == 0) return(0)
  
  term1 <- (n * (n + 1)) / ((n - 1) * (n - 2) * (n - 3)) * sum(((x - m) / s)^4)
  term2 <- (3 * (n - 1)^2) / ((n - 2) * (n - 3))
  term1 - term2   # curtosis en exceso
}

# Función para resumir una variable
summary_table_var <- function(x, var_name, n_total) {
  tibble(
    variable      = var_name,
    n_valid       = sum(!is.na(x)),
    n_missing     = sum(is.na(x)),
    pct_missing   = 100 * sum(is.na(x)) / n_total,
    media         = mean(x, na.rm = TRUE),
    sd            = sd(x, na.rm = TRUE),
    varianza      = var(x, na.rm = TRUE),
    minimo        = min(x, na.rm = TRUE),
    q1            = quantile(x, 0.25, na.rm = TRUE, names = FALSE),
    mediana       = median(x, na.rm = TRUE),
    q3            = quantile(x, 0.75, na.rm = TRUE, names = FALSE),
    maximo        = max(x, na.rm = TRUE),
    asimetria     = skewness_manual(x),
    curtosis_exc  = kurtosis_manual(x)
  )
}

# Aplicar a todas las variables
n_total <- nrow(df)   

tabla_descriptiva <- bind_rows(
  lapply(vars_eda, function(v) summary_table_var(df[[v]], v, n_total))
)

# Redondear para que quede bonita la tabla
tabla_descriptiva <- tabla_descriptiva %>%
  mutate(across(where(is.numeric), ~ round(., 3)))

tabla_descriptiva %>%
  kable(caption = "Estadísticos descriptivos de las variables numéricas (base inicial)") %>%
  kable_styling(full_width = FALSE)

#matriz de correlaciones

corr_mat_eda <- df %>%
  select(all_of(vars_eda)) %>%
  cor(use = "pairwise.complete.obs")

# Pasar a formato largo para ggplot
corr_df_eda <- as.data.frame(corr_mat_eda) %>%
  mutate(variable1 = rownames(.)) %>%
  pivot_longer(
    cols = -variable1,
    names_to = "variable2",
    values_to = "correlation"
  )

# Heatmap
#opcion dos de heatmap con numeritos dentro
ggplot(corr_df_eda, aes(x = variable1, y = variable2, fill = correlation)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(correlation, 2)), size = 3) +
  scale_fill_gradient2(
    low = "#3B4CC0",
    mid = "white",
    high = "#B40426",
    midpoint = 0,
    limits = c(-1, 1)
  ) +
  theme_minimal() +
  labs(
    title = "Matriz de correlaciones",
    x = NULL,
    y = NULL,
    fill = "r"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid = element_blank()
  )


#análisis de valores faltantes
na_por_variable <- df %>%
  summarise(across(-any_of("pais"), ~ sum(is.na(.)))) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "na_count") %>%
  mutate(na_pct = round(100 * na_count / nrow(df), 2)) %>%
  arrange(desc(na_count))
na_por_variable
#write.csv(na_por_variable, "tabla_na_por_variable.csv", row.names = FALSE)


na_por_pais <- df %>%
  mutate(na_count = rowSums(is.na(.))) %>%
  select(pais, na_count) %>%  
  arrange(desc(na_count))
na_por_pais 
#write.csv(na_por_pais, "tabla_na_por_pais.csv", row.names = FALSE)

#seleccionar variables numericas
vars_numericas <- df %>%
  select(where(is.numeric)) %>%
  names()
vars_numericas

#panel de histogramas
df %>%
  select(all_of(vars_numericas)) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "valor") %>%
  ggplot(aes(x = valor)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "white") +
  facet_wrap(~ variable, scales = "free") +
  labs(
    title = "Distribución de las variables numéricas",
    x = "Valor",
    y = "Frecuencia"
  ) +
  theme_minimal()
ggsave("histogramas_variables.png", width = 10, height = 8)

#para las categorticas
grafico_region <- df %>%
  count(region) %>%
  ggplot(aes(x = region, y = n, fill = region)) +
  geom_col() +
  labs(
    title = "Distribución por región",
    x = "Región",
    y = "Frecuencia"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

grafico_region
ggsave(
  filename = "dist_region.png",
  plot = grafico_region,
  width = 16,
  height = 8
)

# Gráfico de barras para income_group_2023
grafico_income <- df %>%
  count(income_group_2023) %>%
  ggplot(aes(x = income_group_2023, y = n, fill = income_group_2023)) +
  geom_col() +
  labs(
    title = "Distribución por grupo de ingresos",
    x = "Grupo de ingresos",
    y = "Frecuencia"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

grafico_income
ggsave(
  filename = "dist_income.png",
  plot = grafico_income,
  width = 7,
  height = 5
)

#boxplots
vars_porcentaje <- c(
  "unemp_rate_2023",
  "elec_access_2023",
  "rural_pct_2023",
  "water_access_2023"
)

grafico_porcentaje <- df %>%
  select(all_of(vars_porcentaje)) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "valor") %>%
  ggplot(aes(x = variable, y = valor)) +
  geom_boxplot(fill = "lightgreen") +
  coord_flip() +
  labs(
    title = "Boxplots de variables en porcentaje",
    x = "Variable",
    y = "Porcentaje"
  ) +
  theme_minimal()
grafico_porcentaje

ggsave(filename = "boxplot_variables_porcentaje.png", plot = grafico_porcentaje, width = 7, height = 5)

#ir seleccionando unas u otras en función de cual se quiera hacer el plot
vars_individuales <- c(
  #"hdi_2023",
  #"infra_lpi_2022",
  #"gdp_pc_ppp_2023",
  #"elec_kwh_pc_2023",
  #"ghg_pc_2023",
  #"f_m_tertiary_2023",
  "secure_servers_pm_2023"
  #"mat_mort_2023",
  #"physicians_per1k_2023"
)

plots_individuales <- map(vars_individuales, function(var) {
  ggplot(df, aes(y = .data[[var]])) +
    geom_boxplot(fill = "tomato") +
    labs(
      title = paste("Boxplot", var),
      y = var,
      x = NULL
    ) +
    theme_minimal()
})

walk2(plots_individuales, vars_individuales, function(p, nombre) {
  ggsave(
    filename = paste0("boxplot_", nombre, ".png"),
    plot = p,
    width = 3,
    height = 3
  )
})

## TRANSFORMACIÓN DE VARIABLES

# Variables claramente candidatas a transformación logarítmica
vars_log_claras <- c(
  "gdp_pc_ppp_2023",
  "secure_servers_pm_2023",
  "elec_kwh_pc_2023",
  "ghg_pc_2023",
  "mat_mort_2023"
)

# Variables dudosas (las exploramos primero)
vars_log_dudosas <- c(
  "physicians_per1k_2023",
  "unemp_rate_2023"
)

# Crear nuevas columnas con sufijo _trans usando log1p(x) = log(1+x)
df <- df %>%
  mutate(
    across(
      all_of(c(vars_log_claras, vars_log_dudosas)),
      ~ log1p(.x),
      .names = "{.col}_trans"
    )
  )
names(df)[grepl("_trans$", names(df))]

vars_compare <- c(
  "gdp_pc_ppp_2023",
  "secure_servers_pm_2023",
  "elec_kwh_pc_2023",
  "ghg_pc_2023",
  "mat_mort_2023",
  "physicians_per1k_2023",
  "unemp_rate_2023"
)

# Pasar a formato largo para comparar original vs transformada
df_hist <- bind_rows(
  df %>%
    select(all_of(vars_compare)) %>%
    pivot_longer(
      everything(),
      names_to = "variable",
      values_to = "valor"
    ) %>%
    mutate(version = "Original"),
  
  df %>%
    select(all_of(paste0(vars_compare, "_trans"))) %>%
    setNames(vars_compare) %>%  
    pivot_longer(
      everything(),
      names_to = "variable",
      values_to = "valor"
    ) %>%
    mutate(version = "Transformada")
)

ggplot(df_hist, aes(x = valor)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "white") +
  facet_grid(version ~ variable, scales = "free") +
  theme_minimal() +
  labs(
    title = "Comparación de histogramas: variables originales vs transformadas",
    x = "Valor",
    y = "Frecuencia"
  )

comparar_hist <- function(data, var){
  var_trans <- paste0(var, "_trans")
  
  tmp <- bind_rows(
    data %>%
      select(all_of(var)) %>%
      rename(valor = all_of(var)) %>%
      mutate(version = "Original"),
    
    data %>%
      select(all_of(var_trans)) %>%
      rename(valor = all_of(var_trans)) %>%
      mutate(version = "Transformada")
  )
  
  ggplot(tmp, aes(x = valor)) +
    geom_histogram(bins = 30, fill = "steelblue", color = "white") +
    facet_wrap(~ version, scales = "free") +
    theme_minimal() +
    labs(
      title = paste("Comparación:", var),
      x = "Valor",
      y = "Frecuencia"
    )
}
comparar_hist(df, "gdp_pc_ppp_2023")
comparar_hist(df, "secure_servers_pm_2023")
comparar_hist(df, "elec_kwh_pc_2023") 
comparar_hist(df, "ghg_pc_2023") 
comparar_hist(df, "mat_mort_2023")
comparar_hist(df, "physicians_per1k_2023") 
comparar_hist(df, "unemp_rate_2023") 


##estandarización

# Variables transformadas 
vars_trans <- c(
  "gdp_pc_ppp_2023_trans",
  "elec_kwh_pc_2023_trans",
  "secure_servers_pm_2023_trans",
  "ghg_pc_2023_trans",
  "mat_mort_2023_trans"
)

# Variables originales 
vars_orig <- c(
  "elec_access_2023",
  "water_access_2023",
  "unemp_rate_2023",
  "physicians_per1k_2023",
  "f_m_tertiary_2023",
  "hdi_2023",
  "infra_lpi_2022",
  "lifeexp_2023",
  "rural_pct_2023"
)

# Vector final de variables activas a estandarizar
vars_modelo <- c(vars_trans, vars_orig)

df <- df %>%
  mutate(
    across(
      all_of(vars_modelo),
      ~ as.numeric(scale(.x)),
      .names = "{.col}_std"
    )
  )

df_final <- df %>%
  select(
    pais,
    region,
    income_group_2023,
    ends_with("_std")
  )


#pca

vars_pca <- c(
  "gdp_pc_ppp_2023_trans_std",
  "elec_kwh_pc_2023_trans_std",
  "secure_servers_pm_2023_trans_std",
  "ghg_pc_2023_trans_std",
  "mat_mort_2023_trans_std",
  "elec_access_2023_std",
  "water_access_2023_std",
  "unemp_rate_2023_std",
  "physicians_per1k_2023_std",
  "f_m_tertiary_2023_std",
  "infra_lpi_2022_std",
  "lifeexp_2023_std",
  "rural_pct_2023_std"
)

df_pca <- df_final %>%
  select(pais, region, income_group_2023, all_of(vars_pca))


colSums(is.na(df_pca))

df_pca %>%
  mutate(na_count = rowSums(is.na(across(all_of(vars_pca))))) %>%
  count(na_count)
df_pca_clean <- df_pca %>%
  filter(rowSums(is.na(across(all_of(vars_pca)))) == 0)

#ejecutar pca
res_pca <- PCA(
  df_pca_clean %>% select(all_of(vars_pca)),
  scale.unit = FALSE,
  graph = FALSE
)

#devuelve los autovalores y los porcentajes de varianza explicada
eig_val <- get_eigenvalue(res_pca)
eig_val

#scree plot
fviz_eig(
  res_pca,
  addlabels = TRUE,
  ylim = c(0, 50),
  barfill = "violet",
  barcolor = "violet"
)

#ggsave("scree_plot_pca.png", width = 8, height = 5, dpi = 300)

#carga de las variables y coordenadas de los paises
var_coord <- get_pca_var(res_pca)
var_coord$coord      # coordenadas/cargas
var_coord$contrib    # contribución a cada componente
var_coord$cos2       # calidad de representación

cargas_pca <- as.data.frame(var_coord$coord[, 1:2])
cargas_pca$Variable <- rownames(cargas_pca)
contrib_pca <- as.data.frame(var_coord$contrib[, 1:2])
contrib_pca$Variable <- rownames(contrib_pca)

tabla_pca <- cargas_pca %>%
  rename(CP1 = Dim.1, CP2 = Dim.2) %>%
  left_join(
    contrib_pca %>%
      rename(Contrib_CP1 = Dim.1, Contrib_CP2 = Dim.2),
    by = "Variable"
  )
tabla_pca
write.csv(tabla_pca, "tabla_pca.csv", row.names = FALSE)

cargas_pca <- cargas_pca %>%
  select(Variable, Dim.1, Dim.2)
cargas_pca
write.csv(na_por_pais, "tabla_na_por_pais.csv", row.names = FALSE)


ind_coord <- get_pca_ind(res_pca)
ind_coord$coord      # coordenadas de los países
ind_coord$cos2       # calidad de representación
ind_coord$contrib    # contribución

#coordenadas + variables ilustrativas
coord_paises <- as.data.frame(ind_coord$coord) %>%
  mutate(
    pais = df_pca_clean$pais,
    region = df_pca_clean$region,
    income_group_2023 = df_pca_clean$income_group_2023
  )

head(coord_paises)

#círculo de correlaciones
fviz_pca_var(
  res_pca,
  col.var = "contrib",
  gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
  repel = TRUE
)

#ggsave("circulo_correlaciones_pca.png", width = 8, height = 6, dpi = 300)

#plano de individuos coloreado por región
fviz_pca_ind(
  res_pca,
  geom.ind = "point",
  col.ind = df_pca_clean$region,
  palette = "jco",
  addEllipses = FALSE,
  repel = TRUE
)

#plano de individuos coloreado por grupo de ingresos
fviz_pca_ind(
  res_pca,
  geom.ind = "point",
  col.ind = df_pca_clean$income_group_2023,
  palette = "jco",
  addEllipses = FALSE,
  repel = TRUE
)

#BIPLOT
fviz_pca_biplot(
  res_pca,
  repel = TRUE,
  col.var = "steelblue",
  col.ind = "pink"
)
#ggsave("biplot_pca.png", width = 9, height = 7, dpi = 300)



#cluster analysis

# Coordenadas de los individuos en el espacio PCA
coord_paises <- as.data.frame(res_pca$ind$coord) %>%
  mutate(
    pais = df_pca_clean$pais,
    region = df_pca_clean$region,
    income_group_2023 = df_pca_clean$income_group_2023
  )

# Nos quedamos con las dos primeras componentes
X_clust <- coord_paises %>%
  select(Dim.1, Dim.2)

#método del codo
fviz_nbclust(X_clust, kmeans, method = "wss") +
  labs(title = "Método del codo para k-means")

#silueta
fviz_nbclust(X_clust, kmeans, method = "silhouette") +
  labs(title = "Índice silhouette para k-means")

#k = 2 y k = 3 automatizado
set.seed(123)
kmeans_res_2 <- kmeans(X_clust, centers = 2, nstart = 25)
kmeans_res_3 <- kmeans(X_clust, centers = 3, nstart = 25)

coord_paises <- coord_paises %>%
  mutate(
    cluster_k2 = factor(kmeans_res_2$cluster),
    cluster_k3 = factor(kmeans_res_3$cluster)
  )

#visualizar con factoextra k = 2
fviz_cluster(
  kmeans_res_2,
  data = X_clust,
  geom = "point",
  ellipse.type = "convex",
  ggtheme = theme_minimal()
)

#visualizar con factoextra k = 3
fviz_cluster(
  kmeans_res_3,
  data = X_clust,
  geom = "point",
  ellipse.type = "convex",
  ggtheme = theme_minimal()
)

#comparaciones
table(coord_paises$cluster_k2)
table(coord_paises$cluster_k3)

table(coord_paises$cluster_k3, coord_paises$income_group_2023)
table(coord_paises$cluster_k3, coord_paises$region)

#cluster jerárquico (k = 3) SOBRE LAS 2 PRIMERAS CP

# Dataset para clustering: dos primeras componentes
X_clust <- coord_paises %>%
  select(Dim.1, Dim.2)

# Matriz de distancias euclídeas
dist_mat <- dist(X_clust, method = "euclidean")

# Clustering jerárquico con método de Ward
hc_res <- hclust(dist_mat, method = "ward.D2")

# Dendrograma
plot(hc_res, labels = FALSE, hang = -1,
     main = "Dendrograma - clustering jerárquico (Ward)")
rect.hclust(hc_res, k = 3, border = 2:4)

# Cortar el árbol en 3 clusters
cluster_hc3 <- cutree(hc_res, k = 3)

# Añadir clusters al dataframe de coordenadas
coord_paises <- coord_paises %>%
  mutate(cluster_hc3 = factor(cluster_hc3))

# Visualización en el plano de las dos primeras componentes
ggplot(coord_paises, aes(x = Dim.1, y = Dim.2, color = cluster_hc3)) +
  geom_point(size = 2) +
  theme_minimal() +
  labs(
    title = "Clusters obtenidos mediante clustering jerárquico (k = 3)",
    x = "Componente principal 1",
    y = "Componente principal 2",
    color = "Cluster"
  )

# Tamaño de los clusters
table(coord_paises$cluster_hc3)

#DISTRIBUCIONES DE LOS PAÍSES POR CLUSTER, COMPARANDO AMBOS MÉTODOS
# Distribución por grupo de ingresos
table(coord_paises$cluster_hc3, coord_paises$income_group_2023)
table(coord_paises$cluster_k3, coord_paises$income_group_2023)

# Distribución por región
table(coord_paises$cluster_hc3, coord_paises$region)
table(coord_paises$cluster_k3, coord_paises$region)

# Comparación con k-means
table(coord_paises$cluster_k3, coord_paises$cluster_hc3)


#medias de variables por cluster

# Crear tabla de clusters (solución principal k = 3)
clusters_k3 <- coord_paises %>%
  select(pais, cluster_k3)

# Unir clusters al dataframe original
df_clusters <- df %>%
  inner_join(clusters_k3, by = "pais")

# Calcular medias por cluster en variables originales
tabla_medias_cluster <- df_clusters %>%
  group_by(cluster_k3) %>%
  summarise(
    pib_pc = mean(gdp_pc_ppp_2023, na.rm = TRUE),
    esperanza_vida = mean(lifeexp_2023, na.rm = TRUE),
    mortalidad_materna = mean(mat_mort_2023, na.rm = TRUE),
    agua = mean(water_access_2023, na.rm = TRUE),
    electricidad = mean(elec_access_2023, na.rm = TRUE),
    infraestructura = mean(infra_lpi_2022, na.rm = TRUE),
    poblacion_rural = mean(rural_pct_2023, na.rm = TRUE)
  ) %>%
  mutate(across(where(is.numeric), ~ round(.x, 2)))

tabla_medias_cluster

#MEDIA IDH POR CLUSTER

# Unir cluster principal (k = 3) al dataframe original
df_idh_cluster <- df %>%
  inner_join(
    coord_paises %>% select(pais, cluster_k3),
    by = "pais"
  )

# Media de IDH por cluster
tabla_idh_cluster <- df_idh_cluster %>%
  group_by(cluster_k3) %>%
  summarise(
    n_paises = n(),
    idh_medio = mean(hdi_2023, na.rm = TRUE),
    idh_sd = sd(hdi_2023, na.rm = TRUE),
    idh_min = min(hdi_2023, na.rm = TRUE),
    idh_max = max(hdi_2023, na.rm = TRUE)
  ) %>%
  mutate(across(where(is.numeric), ~ round(.x, 3)))

tabla_idh_cluster

library(ggplot2)

ggplot(df_idh_cluster, aes(x = cluster_k3, y = hdi_2023, fill = cluster_k3)) +
  geom_boxplot() +
  theme_minimal() +
  labs(
    title = "Distribución del IDH por cluster",
    x = "Cluster",
    y = "IDH"
  )

#gráfico pca con paises etiquetados

extremos_pca <- coord_paises %>%
  mutate(
    rank_dim1_pos = rank(-Dim.1, ties.method = "first"),
    rank_dim1_neg = rank(Dim.1, ties.method = "first"),
    rank_dim2_pos = rank(-Dim.2, ties.method = "first"),
    rank_dim2_neg = rank(Dim.2, ties.method = "first")
  ) %>%
  filter(
    rank_dim1_pos <= 5 |
      rank_dim1_neg <= 5 |
      rank_dim2_pos <= 5 |
      rank_dim2_neg <= 5
  ) %>%
  distinct(pais, .keep_all = TRUE)

extremos_pca %>% select(pais, Dim.1, Dim.2)

ggplot(coord_paises, aes(x = Dim.1, y = Dim.2)) +
  geom_point(aes(color = cluster_k3), alpha = 0.7) +
  geom_text_repel(
    data = extremos_pca,
    aes(label = pais),
    size = 3,
    max.overlaps = Inf
  ) +
  theme_minimal() +
  labs(
    title = "Países extremos en el plano PCA",
    x = "Componente principal 1",
    y = "Componente principal 2",
    color = "Cluster"
  )
