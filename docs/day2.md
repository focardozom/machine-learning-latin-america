---
title: "Técnicas de Regresión y Clasificación para Ciencias Sociales y de la Salud (Machine Learning) Día 2"
author: "Francisco Cardozo"
format: revealjs
editor_options: 
  chunk_output_type: console
---

# Datos ICFES

```{r}
library(tidyverse)
library(tidymodels)
tidymodels_prefer()

data_icfes <- readRDS("DATA/data_icfes.rds")
data_icfes |> glimpse()
```

# Descripción de los datos

```{r}
data_icfes |> skimr::skim()
```

```{r}
mini_datos <- data_icfes |>
  select(contains("fami"), starts_with("estu"), "punt_matematicas_11") |>
  select(contains("9"), "punt_matematicas_11") |>
  select(
    -"fami_trabajolaborpadre_9", -"fami_trabajolabormadre_9",
    -"estu_cod_depto_presentacion_9",
    -"estu_estudiante_9", -"estu_grado_9"
  ) |>
  tidyREDCap::drop_labels()

table1::table1(~., data = mini_datos)

mini_datos <- mini_datos |>
  mutate(across(everything(), ~ if_else(.x == "NA", NA, .x))) |>
  mutate(across(everything(), ~ if_else(.x == "", NA, .x))) |>
  mutate(estu_edad_9 = as.numeric(str_extract_all(estu_edad_9, "\\d+"))) |>
  mutate(estu_edad_9 = na_if(estu_edad_9, 0)) |>
  mutate(estu_mcpio_presentacion_9 = fct_lump_min(estu_mcpio_presentacion_9,
    min = 50,
    other_level = "Otro"
  ))

table(mini_datos$estu_edad_9)
table1::table1(~., data = mini_datos)

mini_datos <- mini_datos |> drop_na()
```

# Regresión lineal

```{r}
lm_model <-
  linear_reg() |>
  set_engine("lm")

lm_form_fit <- workflow() |>
  add_model(lm_model) |>
  add_formula(punt_matematicas_11 ~ .)

lm_results <- lm_form_fit |>
  fit(data = mini_datos)

lm_results |>
  tidy() |>
  view()

lm_results |> glance()
```

# Split data

```{r}
set.seed(2231)

datos_divididos <- initial_split(mini_datos, prop = 0.8)

datos_entrenamiento <- training(datos_divididos)
datos_prueba <- testing(datos_divididos)

mean(datos_entrenamiento$punt_matematicas_11, na.rm = T)
mean(datos_prueba$punt_matematicas_11, na.rm = T)
```

Estimar el modelo utilizando la base de entrenamiento 

```{r estimar-entrenamiento}
fit_entrenamiento <- lm_form_fit |>
  fit(data = datos_entrenamiento)

fit_entrenamiento |> glance()
```

```{r estimar-testing-error}
results <- lm_form_fit |>
  last_fit(datos_divididos)

results |> collect_metrics()

results |>
  extract_fit_parsnip() |>
  tidy() |>
  mutate(p.value = scales::pvalue(p.value)) |>
  view()
```

# ¿De dónde proviene el error?

![](img/error-fuente.jpeg)

## Sesgo y varianza 

Estos dos conceptos son cruciales para entender el equilibrio entre la complejidad del modelo y su capacidad para generalizar a nuevos datos.

**Sesgo (Bias):**
a. Definición: El sesgo es la diferencia entre la predicción promedio de nuestro modelo y el valor verdadero que intentamos predecir.El sesgo, en términos estadísticos, se refiere a la diferencia sistemática entre la esperanza (o promedio) de las estimaciones que produce un estimador y el valor real del parámetro que se desea estimar. Un modelo con alta varianza es muy sensible a pequeñas variaciones en los datos de entrenamiento, lo que puede resultar en un sobreajuste. Es decir, el modelo se ajusta muy bien a los datos de entrenamiento, pero tiene un rendimiento deficiente en datos no vistos o de prueba.

b. Ejemplo: Un modelo de regresión lineal simple podría tener un alto sesgo si los datos reales tienen una relación no lineal.
c. Implicaciones: Un modelo con alto sesgo es demasiado simple y no captura la estructura subyacente de los datos. Esto conduce a un bajo rendimiento en el conjunto de entrenamiento y prueba.

**Varianza (Variance):**
a. Definición: La varianza es la cantidad de variabilidad en las predicciones del modelo para un punto de datos dado.
b. Ejemplo: Un modelo de árbol de decisión muy profundo podría tener alta varianza, ya que es muy sensible a pequeñas variaciones en los datos de entrenamiento.
c. Implicaciones: Un modelo con alta varianza tiende a sobreajustarse a los datos de entrenamiento, lo que resulta en un buen rendimiento en el conjunto de entrenamiento pero un bajo rendimiento en el conjunto de prueba.

*El objetivo en Machine Learning es equilibrar el sesgo y la varianza para minimizar el error de predicción general en el modelo*

a. Objetivo: Encontrar un equilibrio entre sesgo y varianza que minimice el error total de predicción.
b. Estrategias: Seleccionar un modelo con la complejidad adecuada, usar técnicas de regularización, y validar el modelo con conjuntos de datos de entrenamiento y prueba separados.


### Formas de disminur el sesgo

Aumentar la complejidad del modelo: Un modelo más complejo puede capturar mejor la estructura subyacente de los datos. Por ejemplo, en lugar de utilizar una regresión lineal simple, podrías probar una regresión polinomial o un modelo de árbol de decisión.

Añadir más variables: A veces, el sesgo puede ser el resultado de no tener en cuenta variables importantes que influyen en la variable objetivo. Añadir más variables relevantes puede ayudar a reducir el sesgo del modelo.

Utilizar técnicas de "ingeniería de predictores": Transformar o combinar las variables existentes para crear nuevas características puede ayudar a capturar mejor la relación entre las variables de entrada y salida. Por ejemplo, si estás trabajando en un problema de predicción de precios de viviendas, podrías crear una nueva característica que represente la relación entre el tamaño de la casa y el número de habitaciones.

Aumentar el tamaño del conjunto de datos: Si tu conjunto de datos es pequeño o no es representativo de la población general, es posible que el modelo tenga un sesgo alto. Aumentar el tamaño del conjunto de datos y asegurarte de que es representativo puede ayudar a reducir el sesgo.

Utilizar ensembles de modelos: Combinar varios modelos en un ensemble puede ayudar a reducir el sesgo, ya que cada modelo puede capturar diferentes aspectos de la relación entre las variables de entrada y salida. Por ejemplo, puedes utilizar métodos de ensemble como Bagging, Boosting o Stacking.

## Técnicas para reducir la varianza

Reducir la complejidad del modelo: Un modelo más simple tiende a tener una menor varianza y es menos propenso al sobreajuste. Por ejemplo, podrías limitar la profundidad de un árbol de decisión.

Utilizar regularización: La regularización es una técnica que añade una penalización a los coeficientes del modelo para evitar que se ajusten demasiado a los datos de entrenamiento.Por ejemplo, regularización L1 (Lasso) y L2 (Ridge).

Aumentar el tamaño del conjunto de datos: Si dispones de más datos, el modelo será menos sensible a pequeñas variaciones en los datos de entrenamiento y tendrá una menor varianza.

Eliminar características ruidosas: Si tu modelo incluye características que no están relacionadas con la variable objetivo o que contienen mucho ruido, estas pueden aumentar la varianza del modelo. Realizar un análisis de importancia de características y eliminar las características poco importantes puede ayudar a reducir la varianza.

![](img/impacto-predictores.png)

Validación cruzada (cross-validation): Utilizar la validación cruzada, como k-fold cross-validation, te permite evaluar el rendimiento del modelo en diferentes subconjuntos del conjunto de datos de entrenamiento. Esto puede ayudarte a identificar si el modelo está sobreajustando los datos y ajustar la complejidad del modelo en consecuencia.

Utilizar diferentes modelos: Combinar varios modelos en un ensemble puede ayudar a reducir la varianza, ya que la variabilidad de cada modelo individual se promedia. Por ejemplo, puedes utilizar métodos de ensemble como Bagging (Bootstrap Aggregating) o Random Forest, que promedian las predicciones de múltiples árboles de decisión entrenados en subconjuntos aleatorios de los datos.

# Regularización (lasso-ridge) 

El primer paso es  especificar el modelo 

```{r}
spec_lasso <-
  linear_reg(penalty = 0.5, mixture = 1) |>
  # En glmnet, mixture = 1 es un modelo lasso. Mixture = 0 es ridge regression.
  set_engine("glmnet") |>
  set_mode("regression")

spec_lasso |> translate()
```

El segundo paso es crear un workflow. A ese workflow le vamos añadir diferentes pasos. Agregamos el modelo. 

```{r}
wf <-
  workflow() |>
  add_model(spec_lasso)
```

Después agregarmos las transformaciones que debemos realizar a las variables. 

* Dummy
* Centrar
* Escalar

Centrar los datos significa restar la media de una variable de los datos. Escalar los datos significa dividir sobre la desviación estándar.

# Receta 

```{r receta-lasso}
receta_lasso <- recipe(punt_matematicas_11 ~ ., data = mini_datos) |>
  step_normalize(all_double_predictors()) |>
  step_dummy(all_factor_predictors())

prep(receta_lasso) |>
  bake(new_data = mini_datos) |>
  view()
```

Ahora podemos agregar la receta al workflow, para que se aplique a los datos antes de estimar el modelo

```{r}
wf <- wf |>
  add_recipe(receta_lasso)
```

Estimar el error en el training

```{r}
wf %>%
  fit(mini_datos) |>
  extract_fit_parsnip()
```


```{r}
lasso_fit <- wf %>%
  last_fit(datos_divididos)

collect_metrics(lasso_fit)
```

Pero, ¿por qué elegimos esa penalidad? Podemos usar cross-validation para tener algo de información sobre los hiperparámetros que nos dan mejor rendimiento en los datos de prueba. 

```{r}
tuned_spec <-
  linear_reg(penalty = tune(), mixture = 1) |>
  set_mode("regression") |>
  set_engine("glmnet")

tune_wf <-
  workflow() |>
  add_model(tuned_spec) |>
  add_recipe(receta_lasso)

lambda_grid <- grid_regular(penalty(), levels = 10)
```

```{r}
set.seed(1234)
folds <-
  vfold_cv(datos_entrenamiento,
    v = 5
  )
```

```{r}
doParallel::registerDoParallel()

set.seed(2023)

lasso_grid <- tune_grid(
  tune_wf,
  resamples = folds,
  grid = lambda_grid
)

doParallel::stopImplicitCluster()
```

```{r}
show_best(lasso_grid, metric = "rsq")

best <- select_best(lasso_grid, metric = "rsq")
```

```{r}
final_lasso <- finalize_workflow(
  tune_wf, best
)
```

```{r}
last_fit(final_lasso, split = datos_divididos) |> collect_metrics()
```

```{r vip plot}
library(vip)

final_lasso %>%
  fit(datos_entrenamiento) %>%
  extract_fit_parsnip() %>%
  vip::vi(lambda = best$penalty) %>%
  mutate(
    Importance = abs(Importance),
    Variable = fct_reorder(Variable, Importance)
  ) %>%
  head(20) |>
  ggplot(aes(x = Importance, y = Variable, fill = Sign)) +
  geom_col() +
  scale_x_continuous(expand = c(0, 0)) +
  labs(y = NULL) +
  facet_wrap(~Sign, scales = "free_y") +
  theme(legend.position = "none")
```

## Usar recetas

Construir la receta:

`recipe(punt_matematicas_11 ~ ., data = datos_entrenamiento)` crea un objeto recipe que especifica que se va a predecir la variable punt_matematicas_11 usando todas las otras variables (.) en los datos de entrenamiento datos_entrenamiento.

`step_mutate(across(everything(), ~ if_else(.x == "NA", NA, .x)))` reemplaza todos los valores de "NA" en todas las columnas con el valor NA. Esto se hace para asegurarse de que los datos faltantes estén representados correctamente en el conjunto de datos.

`step_mutate(estu_cod_mcpio_presentacion_9 = as_factor(estu_cod_mcpio_presentacion_9))` convierte la columna estu_cod_mcpio_presentacion_9 a un factor. Esto es necesario porque esta columna representa códigos de municipios y no debería tratarse como una variable numérica continua.

`step_mutate(estu_cod_depto_presentacion_9 = as_factor(estu_cod_depto_presentacion_9))` convierte la columna estu_cod_depto_presentacion_9 a un factor. Esto se hace por la misma razón que en el paso anterior.

`step_mutate(estu_edad_9 = as.numeric(str_extract_all(estu_edad_9, "\\d+")))` extrae los números de la columna estu_edad_9 y los convierte a valores numéricos. Esto se hace para asegurarse de que la variable se trate como numérica en lugar de como un factor.

`step_mutate(fami_cuartoshogar_9 = if_else(fami_cuartoshogar_9 == "NA", NA, fami_cuartoshogar_9))` reemplaza los valores "NA" en la columna fami_cuartoshogar_9 con el valor NA. Esto es necesario para asegurarse de que los datos faltantes estén representados correctamente.

`step_mutate(fami_cuartoshogar_9 = as.numeric(fami_cuartoshogar_9))` convierte la columna fami_cuartoshogar_9 a valores numéricos. Esto se hace porque esta variable representa el número de habitaciones en el hogar y se espera que sea una variable numérica.

`step_impute_mode(all_nominal_predictors())` imputa los valores faltantes en todas las variables nominales (factores) con el modo (valor más común) de cada columna.

`step_impute_mean(all_double())` imputa los valores faltantes en todas las variables continuas con la media de cada columna.

`step_zv(all_predictors())` elimina las variables que tienen una varianza cero. Esto se hace para asegurarse de que las variables que no cambian en todo el conjunto de datos no estén incluidas en el modelo.

`step_normalize(all_double_predictors())` normaliza todas las variables continuas para que tengan media cero y varianza unitaria. Esto se hace para asegurarse de que las variables estén en la misma métrica

```{r}
set.seed(2231)

mini_datos <- data_icfes |>
  select(contains("fami"), starts_with("estu"), "punt_matematicas_11") |>
  select(contains("9"), "punt_matematicas_11") |> tidyREDCap::drop_labels()

datos_divididos <- initial_split(mini_datos, prop = 0.8)

datos_entrenamiento <- training(datos_divididos)
datos_prueba <- testing(datos_divididos)

mean(datos_entrenamiento$punt_matematicas_11, na.rm = T)
mean(datos_prueba$punt_matematicas_11, na.rm = T)
```

```{r}
receta_lasso_2 <- 
  recipe(punt_matematicas_11 ~ ., data = datos_entrenamiento) |>
  step_mutate(across(everything(), ~ if_else(.x == "NA", NA, .x))) |>
  step_mutate(estu_cod_mcpio_presentacion_9 = as_factor(estu_cod_mcpio_presentacion_9)) |>
  step_mutate(estu_cod_depto_presentacion_9 = as_factor(estu_cod_depto_presentacion_9)) |>
  step_mutate(estu_edad_9 = as.numeric(str_extract_all(estu_edad_9, "\\d+"))) |>
  step_mutate(fami_cuartoshogar_9 = if_else(fami_cuartoshogar_9 == "NA", NA, 
                                            fami_cuartoshogar_9)) |>
  step_mutate(fami_cuartoshogar_9 = as.numeric(fami_cuartoshogar_9)) |>
  step_impute_mode(all_nominal_predictors()) |>
  step_impute_mean(all_double()) |>
  step_zv(all_predictors()) |>
  step_normalize(all_double_predictors()) |>
  step_dummy(all_factor_predictors())
```

Aplicar la receta y obtener los datos transformados. 

```{r}
despues_receta <- prep(receta_lasso) |> bake(new_data = datos_entrenamiento)
```

```{r}
spec_lasso <-
  linear_reg(penalty = 0.5, mixture = 1) |>
  # En glmnet, mixture = 1 es un modelo lasso. Mixture = 0 es ridge regression.
  set_engine("glmnet") |>
  set_mode("regression")

```

El segundo paso es crear un workflow. A ese workflow le vamos añadir diferentes pasos. Agregamos el modelo. 

```{r}
wf2 <-
  workflow() |>
  add_model(spec_lasso)
```

Después agregarmos las transformaciones que debemos realizar a las variables. 

* Dummy
* Centrar
* Escalar

Centrar los datos significa restar la media de una variable de los datos. Escalar los datos significa dividir sobre la desviación estándar.

Ahora podemos agregar la receta al workflow, para que se aplique a los datos antes de estimar el modelo

```{r}
wf2 <- wf2 |>
  add_recipe(receta_lasso_2)
```

Estimar el error en el training

```{r}

lasso_fit <- wf2 |> 
  last_fit(datos_divididos)

collect_metrics(lasso_fit)

```

```{r}
wf2 %>%
  fit(datos_entrenamiento) %>%
  extract_fit_parsnip() %>%
  vip::vi() %>%
  mutate(
    Importance = abs(Importance),
    Variable = fct_reorder(Variable, Importance)
  ) %>%
  head(20) |>
  ggplot(aes(x = Importance, y = Variable, fill = Sign)) +
  geom_col() +
  scale_x_continuous(expand = c(0, 0)) +
  labs(y = NULL) +
  facet_wrap(~Sign, scales = "free_y") +
  theme(legend.position = "none")
```



El paquete `usemodels` propone una manera de hacer el recipe, el engine y el workflow

```{r}

usemodels::use_ranger(punt_matematicas_11 ~ ., data = datos_entrenamiento)

```