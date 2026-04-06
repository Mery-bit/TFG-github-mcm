# TFG - Código y datos

Este repositorio contiene el código usado en el TFG y un análisis de sensibilidad (anexo).

## Estructura
- `código proyecto principal.R`: análisis principal del trabajo.
- `código anexo sens.R`: análisis de sensibilidad (anexo E).
- `data/BASE DE DATOS SIN IMPUTAR.xlsx`: base de datos (hojas `BASE` y `B. IMPUTADOS`).

## Requisitos
Paquetes usados (instalar si falta):
- readxl
- dplyr
- tidyr
- ggplot2
- ggrepel
-janitor
- naniar
- stringr
- knitr
- kableExtra
- purrr
- FactoMineR
- factoextra
- cluster
- here

## Cómo ejecutar
1. Descargar/clonar el repositorio.
2. Abrir R/RStudio y situarse en la carpeta raíz del repositorio.
3. Ejecutar:
   - `código proyecto principal.R`
   - `código anexo análisis sens.R`

## Notas sobre rutas
Los scripts leen el Excel desde `data/BASE DE DATOS SIN IMPUTAR.xlsx` usando una ruta relativa.