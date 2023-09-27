

# Data set 1 Nuevos Rumbos

## Nuevos Rumbos

Nuevos Rumbos es una corporación dedicada a la prevención y a la investigación del consumo de sustancias. 

https://nuevosrumbos.org/index

## Base de datos

- Factores de riesgo y de protección
- Consumo de alcohol y otras drogas
- Características demográficas

## Importar la base de datos

```{r base-de-datos}
library(tidyverse)
los_datos <- readRDS("DATA/base_NR.rds") |> as_tibble()
```

## Explorar los datos

```{r explorar-los-datos}
los_datos %>% 
  glimpse()
```


## `select()`

Este verbo selecciona solo la columnas que yo quiero de la base de datos. 

En este ejemplo solo quiero una base de datos que tenga la variable 
"Hay gente en mi barrio que se siente orgullosa de mí cuando hago algo bien"

```{r}
los_datos |> 
  select(NHPROUD) |>
  distinct()
```

En este ejemplo solo voy a seleccionar la variable de Sexo y las preguntas relacionadas con: 
"Qué tan mal ven la mayoría de los adultos  de tu barrio (aquellos más cercanos a ti) el que los jóvenes de tu edad…"

```{r}
los_datos |> 
  select(GENDER, AWRMAR, AWRALC, AWRCIG)
```

## `mutate()` y  `across()`

Este verbo es capaz de crear una nueva variable o tranformar una variable que esté presente en la base de datos. 
Mi idea es tranformar las tres variables del ejemplo anterior para tener un puntaje de las percepciones de los estudiantes sobre las creencias de los adultos frente al consumo de las sustancias. 

Entonces quiero
1. Transformar el texto en 1, 2, 3 y 4
2. Crear una nueva variable que calcule la media de los puntajes. 

```{r}

los_datos |> 
  select(AWRMAR, AWRALC, AWRCIG) |> 
  mutate(AWRMAR = case_when(
    AWRMAR == "Muy mal" ~ 1,
    AWRMAR == "Mal" ~ 2,
    AWRMAR == "Notan mal" ~ 3,
    AWRMAR == "Para nada mal" ~ 4,
   TRUE ~ NA
  ))

transformar_respuesta <- function(x) {
  case_when(
    x == "Muy mal" ~ 1,
    x == "Mal" ~ 2,
    x == "Notan mal" ~ 3,
    x == "Para nada mal" ~ 4,
    TRUE ~ NA
  )
}

los_datos |> 
  select(AWRMAR, AWRALC, AWRCIG) |> 
  mutate(across(everything(), transformar_respuesta)) |> 
   mutate(TOTAL = (AWRMAR + AWRALC + AWRCIG)/3)

```


## `transmute()`

```{r}

los_datos |> 
  select(AWRMAR, AWRALC, AWRCIG) |> 
   mutate(across(everything(), transformar_respuesta)) |> 
  transmute(TOTAL = (AWRMAR + AWRALC + AWRCIG)/3)

```


## `filter()`

Siguiendo con el ejemplo de qué tan mal los adultos del barrio ven el consumo de ciertas sustancias, 
voy a utilizar un filtro para seleccionar solamente esas filas en las que los adultos respondieron "No tan mal" para el consumo de marihuana (AWRMAR)

```{r}
los_datos |> 
  select(GENDER, AWRMAR, AWRALC, AWRCIG) |> 
  filter(AWRMAR == "No tan mal")
```

Con este filtro puedo facilmente ver en los primeros 10 casos que cuando un adulto juzga que no está tan mal fumar marihuana, el juicio de consumo de alcohol y cigarrillo parece seguir el mismo patrón. 

Ahora miremos qué pasa si filtro por la opción de "Muy mal"

```{r}
los_datos |> 
  select(GENDER, AWRMAR, AWRALC, AWRCIG) |> 
  filter(AWRMAR == "Muy mal")
```

En los primeros 10 casos puedo ver que es diferente al primer ejemplo, juzgar el consumo de marihuana como Muy mal parece también coincidir con el consumo de alcohol y cigarrillo. 

## `summarise()` y `group_by()`

Estos dos verbos permiten sacar medidas de tendencia central de la base de datos

```{r}

# Para marihuana

los_datos |> 
  select(GENDER, AWRMAR, AWRALC, AWRCIG) |> 
  mutate(across(starts_with("A"),transformar_respuesta)) |> 
  group_by(GENDER) |> 
  summarise(mean_AWRMAR = mean(AWRMAR, na.rm = TRUE),
            sd_AWRMAR = sd(AWRMAR, na.rm = TRUE), 
            max_AWRMAR = max(AWRMAR, na.rm = TRUE), 
            min_AWRMAR = min(AWRMAR, na.rm = TRUE))

# Para alcohol
los_datos |> 
  select(GENDER, AWRMAR, AWRALC, AWRCIG) |> 
  mutate(across(starts_with("A"),transformar_respuesta)) |> 
  group_by(GENDER) |> 
  summarise(mean_AWRALC = mean(AWRALC, na.rm = TRUE),
            sd_AWRALC = sd(AWRALC, na.rm = TRUE), 
            max_AWRALC = max(AWRALC, na.rm = TRUE), 
            min_AWRALC = min(AWRALC, na.rm = TRUE))

```


## Explorar los datos

```{r variables-interes}
factores_de_riesgo <- c("CRPAD", "CRLNFD", "FRPFD", "FRPFM", "SRLCS", "PRFAD", 
                        "PRATA", "PRFUD", "PRIAP", "FPOPI", "FPRPI", "SPRPI")
demograficas <- c("YEAR", "GRADE", "GENDER", "AGE")
consumo_alcohol <- c("PYALC")
```

## Explorar los datos

```{r explorar-subset}

los_datos %>% 
  select(all_of(factores_de_riesgo), 
         all_of(demograficas), 
         all_of(consumo_alcohol)) %>% 
  glimpse()

```

## Explorar los datos

```{r crear-subset}

mini_datos <- los_datos %>% 
  select(all_of(factores_de_riesgo), 
         all_of(demograficas), 
         all_of(consumo_alcohol)) 

```

## Explorar los datos

```{r descriptivos}

mini_datos |> 
  skimr::skim()

```

## Estimar un modelo

- lm(formula, data, ...)
- stan_glm(formula, data, family= "gaussian",...)
- glmnet(x=matrix, y=vector, family="gaussian",...)

## Especificar el modelo en `tidymodels`
https://www.tidymodels.org/

- Especificar el tipo de modelo
- Declarar el tipo de outcome
    - Regresión: Continua
    - Clasificación: multinomial, ordinal, binaria

```{r modelos-tidymodels}

library(tidymodels)
tidymodels_prefer()

linear_reg() |> 
  set_engine("lm") 

linear_reg() |> 
    set_engine("glmnet") 
    
linear_reg() |>
    set_engine("stan") 
```

## Ejemplo 1

Estimar un modelo de regresión logistica para evaluar asociación entre el consumo de alcohol y los factores de riesgo

```{r}
mini_datos |>
    select(PYALC,factores_de_riesgo) |>
    table1::table1( ~ ., data=_)
```


```{r}
mini_datos |>
    select(PYALC,factores_de_riesgo) |>
  filter(!is.na(PYALC)) |> 
    table1::table1( ~ . | PYALC, data=_)
```

Explorar los datos gráficamente

```{r}

mini_datos |> 
  group_by(PYALC) |> 
  summarise(across("CRPAD":"SPRPI", \(x) mean(x, na.rm = TRUE))) |> 
  pivot_longer(-PYALC) |> 
  filter(!is.na(PYALC)) |> 
  ggplot(aes(value, fct_reorder(name, value), fill = PYALC)) +
  geom_col(alpha = 0.8, position = "dodge") +
  scale_x_continuous() +
  labs(x = "Promedio", y = NULL, fill = NULL)

```

```{r}
library(tidymodels)
tidymodels_prefer()

lm_model <-
    logistic_reg() %>% 
    set_engine("glm", family="binomial") |>
    set_mode("classification")
```

```{r}
lm_results <- lm_model |>
    fit(PYALC ~ ., data = mini_datos)

lm_results
```

```{r }
lm_results |> tidy() 

lm_results |> glance()
```

```{r}
lm_results |> tidy(exp=TRUE, conf.int=TRUE)
```

# Actividad

1. Con select, creen una nueva base de datos con los predictores que ustedes consideren relevantes. 
2. Estimen un modelo con esta nueva base de datos
3. Calculen los odds ratio (glance())

## Para qué necesitamos los datos?

Necesitamos:

    - Estimar parametros
    - Seleccionar modelos
    - Sintonizar los modelos
    - Evaluar los modelos

Cómo gastarnos los datos de una forma que sea eficiente para todos estos pasos? (validación empírica)

# Primera idea útil: dividir los datos en dos muestras.

## Dividir los datos

- Dividir los datos en dos conjuntos
    - Entrenamiento
        - La mayoría de los datos
    - Prueba
        - Un pequeño conjunto de datos
        - Aquí se evaluará el modelo

Los datos de prueba se utilizan una sola vez, si se utilizan más de una vez, se convierten en parte del proceso de entrenamiento.

la división de los datos se hace al nivel de unidad independiente de observación.

# Contaminación del los datos de prueba (information leakage)

## Dividir los datos

Comencemos con un ejemplo simple

- crear dos bases: 80% y 20%

```{r dividir-datos2}
set.seed(1234)

mini_datos <- mini_datos |> drop_na()

datos_divididos <- initial_split(mini_datos, prop = 0.8, strata = "PYALC")

datos_entrenamiento <- training(datos_divididos)
datos_prueba <- testing(datos_divididos)
```

La opción strata es para que los datos de entrenamiento y prueba tengan la misma proporción de la variable PYALC

A veces la selección aleatoria de la muestra es problemática, por ejemplo cuando hay una componente de tiempo en los datos. En este caso, se puede usar la función initial_time_split().


```{r modelo-regresion-logistica-descp1}
datos_entrenamiento |>
    select(PYALC,factores_de_riesgo) |>
    table1::table1( ~ ., data=_)
```

```{r modelo-regresion-logistica-descp2}

datos_entrenamiento |>
  select(PYALC, factores_de_riesgo) |>
  filter(!is.na(PYALC)) |>
  table1::table1(~ . | PYALC, data = _)
```

```{r modelo-regresion-logistica}

lm_model <-
  logistic_reg() %>%
  set_engine("glm", family = "binomial") |>
  set_mode("classification")

```

```{r}

lm_results <- lm_model |>
    fit(PYALC ~ ., data = datos_entrenamiento)

lm_results
```

```{r modelo-regresion-logistica2}
lm_results |> tidy()
```

```{r modelo-regresion-logistica3}
lm_results |> tidy(exp=TRUE, conf.int=TRUE)
```

## Evaluar el modelo

```{r modelo-regresion-logistica4}

predecir_estos_valores <- datos_entrenamiento %>% 
    select(-PYALC) %>% 
    slice(1:10)

predict(lm_results, predecir_estos_valores)
predict(lm_results, predecir_estos_valores, type="prob")

```

```{r modelo-regresion-logistica5}

entrenamiento <- datos_entrenamiento %>% 
    select(-PYALC)

prediccion <- predict(lm_results, entrenamiento)

resultados_prueba <- cbind(prediccion, datos_entrenamiento) |> 
  select(.pred_class, PYALC, everything()) |> 
  tibble()

```

# Métricas

**Accuracy (Exactitud):** Es una métrica que mide la proporción de predicciones correctas realizadas por un modelo en comparación con el total de predicciones realizadas. Es decir, la cantidad de veces que el modelo acertó sobre el total de datos que se le presentaron.

**Sensitivity (Sensibilidad):** Es la proporción de verdaderos positivos (TP) que son identificados correctamente por el modelo en relación con el total de verdaderos positivos y falsos negativos (FN). La sensibilidad mide la capacidad del modelo para detectar correctamente los casos positivos.

**Specificity (Especificidad):** Es la proporción de verdaderos negativos (TN) que son identificados correctamente por el modelo en relación con el total de verdaderos negativos y falsos positivos (FP). La especificidad mide la capacidad del modelo para detectar correctamente los casos negativos.

**Precision (Precisión):** Es la proporción de verdaderos positivos (TP) que son identificados correctamente por el modelo en relación con el total de verdaderos positivos y falsos positivos (FP). La precisión mide la capacidad del modelo para no identificar falsamente un caso como positivo.

**Recall (Recuperación):** Es la proporción de verdaderos positivos (TP) que son identificados correctamente por el modelo en relación con el total de verdaderos positivos y falsos negativos (FN). El recall mide la capacidad del modelo para identificar todos los casos positivos.

**F-measure (Puntuación F):** Es una métrica que combina la precisión y el recall en una sola puntuación. El valor de la F-measure oscila entre 0 y 1, siendo 1 el valor óptimo.

**Kappa (Coeficiente Kappa):** Es una medida de concordancia que compara la cantidad de acuerdos observados entre el modelo y las observaciones reales con la cantidad de acuerdos que se esperarían por casualidad. Un valor de kappa cercano a 1 indica una concordancia casi perfecta entre el modelo y las observaciones reales.

**Matthews Correlation Coefficient (Coeficiente de Correlación de Matthews):** Es una medida que se utiliza para evaluar la calidad de la clasificación binaria. El coeficiente de correlación de Matthews oscila entre -1 y 1, siendo 1 el valor óptimo. Un valor cercano a 1 indica una clasificación perfecta, mientras que un valor cercano a -1 indica una clasificación completamente incorrecta.

![](img/metrics.jpeg)

```{r modelo-regresion-logistica5-metricas}

conf_mat(resultados_prueba, truth = PYALC,
         estimate = .pred_class)

accuracy(resultados_prueba, truth = PYALC,
         estimate = .pred_class)

sens(resultados_prueba, truth = PYALC,
    estimate = .pred_class)

spec(resultados_prueba, truth = PYALC,
    estimate = .pred_class)

precision(resultados_prueba, truth = PYALC,
    estimate = .pred_class)

recall(resultados_prueba, truth = PYALC,
    estimate = .pred_class)

kap(resultados_prueba, truth = PYALC,
    estimate = .pred_class)

```

```{r}
custom_metrics <- metric_set(accuracy, sens, spec, precision, recall, f_meas, kap, mcc)

custom_metrics(resultados_prueba,
  truth = PYALC,
  estimate = .pred_class
)

lm_metrics <- custom_metrics(resultados_prueba,
  truth = PYALC,
  estimate = .pred_class
) |>
  mutate(model = "lm")
```

# Tabla de Métricas 

```{r tablas}
library(gt)

custom_metrics(resultados_prueba,
  truth = PYALC,
  estimate = .pred_class
) |> gt()

```

# ROC 

```{r tablas}

prediccion_auc <- predict(lm_results, entrenamiento, type = "prob")

resultados_prueba_auc <- cbind(prediccion_auc, datos_entrenamiento) |>
  tibble()

roc_auc(resultados_prueba_auc,
  truth = PYALC,
  `.pred_No ha consumido`
)

roc_curve(resultados_prueba_auc,
  truth = PYALC,
  `.pred_No ha consumido`
) |> autoplot()


```

# ¿Podemos encontrar un modelo que sea mejor que el anterior?


## Árboles de decisión

El objetivo es tomar una serie de decisiones a lo largo del árbol, basadas en las respuestas a cada pregunta, para llegar a una conclusión o predicción final. Cada nodo del árbol representa una característica de los datos que se están analizando y cada rama representa una posible respuesta a esa característica.

Los árboles de decisión son útiles porque proporcionan una forma fácil de visualizar y entender cómo se toman las decisiones en un modelo 


![](img/tree.png)

## Arbusto

```{r }
mi_primer_arbol <- los_datos |> 
  select("PYALC", "GETALC", "GENDER", "GRADE")
```

```{r }
tree_spec <- 
  decision_tree() |> 
  set_mode("classification") |> 
  set_engine("rpart")
```

```{r}
tree_results <- tree_spec |>
    fit(PYALC ~ ., data = mi_primer_arbol)
```

```{r}
cart_tree_fit <- tree_results$fit

treemisc::tree_diagram(cart_tree_fit, roundint = FALSE)
```

https://mlu-explain.github.io/decision-tree/ 

### Árboles de clasificación y regresión (CART)

```{r especificar-model-tree}
tree_spec <- 
  decision_tree() |> 
  set_mode("classification") |> 
  set_engine("rpart")
```

```{r estimar-modelo-tree}
tree_results <- tree_spec |>
  fit(PYALC ~ ., data = datos_entrenamiento)
```

```{r predecir-modelo-tree}
predict(tree_results, predecir_estos_valores)

entrenamiento <- datos_entrenamiento %>%
  select(-PYALC)

prediccion_tree <- predict(tree_results, entrenamiento)

resultados_prueba_tree <- cbind(prediccion_tree, datos_entrenamiento) |>
  tibble()

```

```{r}
custom_metrics(resultados_prueba_tree,
  truth = PYALC,
  estimate = .pred_class
) |>
  mutate(model = "tree")

tree_metrics <- custom_metrics(resultados_prueba_tree,
  truth = PYALC,
  estimate = .pred_class
) |>
  mutate(model = "tree")

rbind(tree_metrics, lm_metrics) |>
  pivot_wider(names_from = model, values_from = .estimate)

```

# CART con hiperparametros

Los hiperparámetros son valores específicos para los modelos que se pueden ajustar y que permiten controlar el proceso de entrenamiento de un modelo.


```{r}
tree_spec <- 
  decision_tree(min_n = 5, cost_complexity = 0.001) |> 
  set_mode("classification") |> 
  set_engine("rpart")
```

```{r}
tree2_results <- tree_spec |>
    fit(PYALC ~ ., data = datos_entrenamiento)
```

```{r}

tree2_predicciones <- predict(tree2_results, entrenamiento)

resultados_prueba_tree2 <- cbind(tree2_predicciones, datos_entrenamiento) |>
  tibble()

tree2_metrics <- custom_metrics(resultados_prueba_tree2,
  truth = PYALC,
  estimate = .pred_class
) |>
  mutate(model = "tree2")

rbind(tree2_metrics, tree_metrics, lm_metrics) |>
  pivot_wider(names_from = model, values_from = .estimate)
```

# ¡Pero todo esto pasa en los datos de prueba!

## ¿Por qué es malo?

```{r}

datos_prueba2_out <- 
  datos_prueba |> 
    select(PYALC) 
  
lm_predicciones <- cbind(predict(lm_results, datos_prueba), datos_prueba2_out)|> mutate(model="lm")

tree_predicciones <- cbind(predict(tree_results, datos_prueba), datos_prueba2_out)|> mutate(model="tree")

tree2_predicciones <- cbind(predict(tree2_results, datos_prueba), datos_prueba2_out) |>  mutate(model="tree2")

all_models <- 
rbind(lm_predicciones, tree_predicciones, tree2_predicciones) 

all_models
```

```{r}

all_models2 <- all_models |> 
  group_split(model) %>%
   setNames(unique(all_models$model)) %>%
  map_dfr(., ~custom_metrics(.x,
               truth = PYALC,
               estimate = .pred_class), .id = "names")

all_models2 |> 
  pivot_wider(names_from = names, values_from = .estimate)
```

# ¿Cómo vamos a elegir el mejor modelo si solo evaluamos el modelo en los datos de prueba?

## Validación Cruzada

Supongamos que estamos trabajando en un problema de clasificación binaria y disponemos de un conjunto de datos con 1000 registros. Queremos evaluar el rendimiento de un modelo de regresión logística utilizando la validación cruzada k-fold.


Paso a paso del proceso de k-fold cross-validation:

Dividir el conjunto de datos: Primero, dividimos el conjunto de datos en k subconjuntos (folds) de igual tamaño. En este ejemplo, elegimos k=10, lo que significa que dividimos el conjunto de datos en 10 subconjuntos de 100 registros cada uno.

Entrenar y evaluar el modelo: Luego, realizamos lo siguiente para cada uno de los k subconjuntos:

a. Tomamos un subconjunto como el conjunto de prueba (validación) y los k-1 subconjuntos restantes como el conjunto de entrenamiento. Por ejemplo, en la primera iteración, usamos el primer subconjunto como conjunto de prueba y los subconjuntos del 2 al 10 como conjunto de entrenamiento.

b. Entrenamos el modelo de regresión logística utilizando el conjunto de entrenamiento.

c. Evaluamos el rendimiento del modelo en el conjunto de prueba utilizando una métrica adecuada, como la precisión, la exhaustividad o el F1-score. Anotamos el resultado de la métrica para esta iteración.

Promediar los resultados: Después de completar las k iteraciones, calculamos la media de los resultados de la métrica para todas las iteraciones. Esta media nos proporciona una estimación más robusta del rendimiento del modelo, ya que el modelo ha sido evaluado en diferentes subconjuntos del conjunto de datos.

https://scikit-learn.org/stable/modules/cross_validation.html
https://www.tmwr.org/resampling.html


## CART

Hay tres variables que están más relacionadas con no consumir alcohol. 

```{r}
crossvalidation <-
  vfold_cv(datos_entrenamiento, 
           v = 5,  # número de cajas
           strata = "PYALC")
```

```{r}
tree_spec <- 
  decision_tree(
    cost_complexity = tune(), 
    tree_depth = tune(),
    min_n = tune()
  ) |> 
  set_mode("classification") |> 
  set_engine("rpart")
```

Cost_complexity (tmedida de complejidad alfa o parámetro de poda alfa):

Cost_complexity es un parámetro de regularización. Es una medida de la penalización que se aplica al árbol en función de su complejidad. Un valor más alto de cost_complexity implica una penalización más fuerte en la complejidad del árbol, lo que lleva a un árbol más pequeño y menos profundo. La idea es encontrar un valor óptimo de cost_complexity que equilibre la precisión y la complejidad del árbol, reduciendo tanto el sesgo como la varianza.

Tree_depth (profundidad del árbol):

Tree_depth se refiere a la longitud máxima del camino más largo desde la raíz hasta una hoja en un árbol de decisión. Un árbol más profundo es más complejo y puede capturar relaciones más complicadas en los datos. Sin embargo, un árbol demasiado profundo también puede ser propenso al sobreajuste, ya que puede adaptarse demasiado a las peculiaridades de los datos de entrenamiento.

Min_n (mínimo número de muestras para dividir un nodo):

Min_n es un parámetro que controla el número mínimo de datos requeridas para dividir un nodo en un árbol de decisión. Un valor más alto de min_n implica que el árbol será menos profundo, ya que se requerirán más muestras para realizar una división en cada nodo. Un valor más bajo de min_n permite que el árbol se divida más fácilmente y, por lo tanto, puede resultar en un árbol más complejo y profundo.


```{r}
tree_grid <- grid_regular(cost_complexity(range = c(-10L, -1L)), 
                          tree_depth (range = c(5L, 10L)), 
                          min_n(range = c(5L, 30L)))
```

```{r}
doParallel::registerDoParallel()

set.seed(345)
tree_rs <-
  tune_grid(
    tree_spec,
    PYALC ~ .,
    resamples = crossvalidation,
    grid = tree_grid,
    metrics = metric_set(accuracy, roc_auc, sensitivity, specificity)
  )

doParallel::stopImplicitCluster()

```

```{r}
show_best(tree_rs)

autoplot(tree_rs)
```

```{r}
simpler_tree <- select_best(tree_rs, min_n, metric = "accuracy")

final_tree <- finalize_model(tree_spec, simpler_tree)
```

```{r}
final_fit <- fit(final_tree, PYALC ~ ., datos_entrenamiento)
```

```{r}
final_cart <- last_fit(final_tree, PYALC ~ ., datos_divididos,
  metrics = metric_set(accuracy, roc_auc, sensitivity, specificity)
)
```

```{r}
collect_metrics(final_cart)
```

```{r}
cart_trained <- 
  final_cart  |> extract_fit_parsnip()

cart_tree_fit <- cart_trained$fit

treemisc::tree_diagram(cart_tree_fit, roundint=FALSE)
```

## Random Forest 

- Número de predictores que se usan para cada árbol (mtry)
- Número de árboles (trees)
- Profundidad de los árboles (min_n)

### Random Forest


```{r especificar-model-rf}

rf_spec <- 
  rand_forest()  |> 
  set_mode("classification") |> 
  set_engine("ranger", importance = "permutation")

```


```{r estimar-model-rf}
rf_results <- rf_spec |> 
fit(PYALC ~ ., data = datos_entrenamiento)

library(vip)
importance_plot_rf <- 
  rf_results |> 
  vip() +
  ggtitle("Random Forest")
```

```{r predecir-model-rf}

rf_predicciones <- predict(rf_results, entrenamiento)

resultados_rf <- cbind(rf_predicciones,datos_entrenamiento) |> 
  tibble()

rf_metrics <- custom_metrics(resultados_rf,
               truth = PYALC,
               estimate = .pred_class) |>
  mutate(model="rf")

rbind(rf_metrics, tree2_metrics, tree_metrics, lm_metrics) |> 
  pivot_wider(names_from = model, values_from = .estimate)
```

```{r rf-testing}

rf_predicciones <- cbind(predict(rf_results, datos_prueba), datos_prueba2_out) |>  mutate(model="rf")

all_models <- 
rbind(lm_predicciones, rf_predicciones,tree_predicciones, tree2_predicciones) 


all_models2 <- all_models |> 
  group_split(model) %>%
   setNames(unique(all_models$model)) %>%
  map_dfr(., ~custom_metrics(.x,
               truth = PYALC,
               estimate = .pred_class), .id = "names")
all_models2 |> 
  pivot_wider(names_from = names, values_from = .estimate)

```


## Sesgo y varianza 

Estos dos conceptos son cruciales para entender el equilibrio entre la complejidad del modelo y su capacidad para generalizar a nuevos datos.

Sesgo (Bias):
a. Definición: El sesgo es la diferencia entre la predicción promedio de nuestro modelo y el valor verdadero que intentamos predecir.El sesgo, en términos estadísticos, se refiere a la diferencia sistemática entre la esperanza (o promedio) de las estimaciones que produce un estimador y el valor real del parámetro que se desea estimar. Un modelo con alta varianza es muy sensible a pequeñas variaciones en los datos de entrenamiento, lo que puede resultar en un sobreajuste. Es decir, el modelo se ajusta muy bien a los datos de entrenamiento, pero tiene un rendimiento deficiente en datos no vistos o de prueba.

b. Ejemplo: Un modelo de regresión lineal simple podría tener un alto sesgo si los datos reales tienen una relación no lineal.
c. Implicaciones: Un modelo con alto sesgo es demasiado simple y no captura la estructura subyacente de los datos. Esto conduce a un bajo rendimiento en el conjunto de entrenamiento y prueba.

Varianza (Variance):
a. Definición: La varianza es la cantidad de variabilidad en las predicciones del modelo para un punto de datos dado.
b. Ejemplo: Un modelo de árbol de decisión muy profundo podría tener alta varianza, ya que es muy sensible a pequeñas variaciones en los datos de entrenamiento.
c. Implicaciones: Un modelo con alta varianza tiende a sobreajustarse a los datos de entrenamiento, lo que resulta en un buen rendimiento en el conjunto de entrenamiento pero un bajo rendimiento en el conjunto de prueba.

El objetivo en Machine Learning es equilibrar el sesgo y la varianza para minimizar el error de predicción general en el modelo
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

Validación cruzada (cross-validation): Utilizar la validación cruzada, como k-fold cross-validation, te permite evaluar el rendimiento del modelo en diferentes subconjuntos del conjunto de datos de entrenamiento. Esto puede ayudarte a identificar si el modelo está sobreajustando los datos y ajustar la complejidad del modelo en consecuencia.

Utilizar diferentes modelos: Combinar varios modelos en un ensemble puede ayudar a reducir la varianza, ya que la variabilidad de cada modelo individual se promedia. Por ejemplo, puedes utilizar métodos de ensemble como Bagging (Bootstrap Aggregating) o Random Forest, que promedian las predicciones de múltiples árboles de decisión entrenados en subconjuntos aleatorios de los datos.

