# spatialr

Conjunto de funciones espaciales para R.

# boxmapr

boxmapr es una función para crear una infografía espacial de una variable cuantitativa. La infografía incluye:
  1. Un mapa de cuartiles/outliers (boxplot),
  
  2. Un Histograma
  
  3. Un Boxplot
  
  4. Un correlograma con otra variable, incluyendo la recta de regresión el coeficiente r de pearson.
  
Los rangos del mapa se calculan a partir de https://rpubs.com/helson/608377
inputs:

  1. Un dataframe con variables numéricas
  
  2. el nombre de una variabrle var= variable para el mapa, histograma y boxplot
  
  3. el nombre de una variable var2 para el scatterplot.

