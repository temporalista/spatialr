#### BOXMAP Mapa infográfico múltiple
# Infografía que incluye:
# 1. Un mapa de cuartiles/outliers (boxplot),
# 2. Un Histograma
# 3. Un BOxplot
# 4. Un scatterplot con una variable 2
# Los rangos del mapa se calculan a partir de https://rpubs.com/helson/608377
# imputs:
# 1. Un dataframe con variables numéricas
# 2. el nombre de una variabrle var= variable para el mapa, histograma y boxplot
# 3. el nombre de una variable var2 para el scatterplot.


boxmapr <- function(data,
                    var,
                    h = 1.5,
                    splot = TRUE,
                    vardep) {
  #based on https://rpubs.com/helson/608377
  invisible(lapply(
    c("sf", "ggplot2", "cowplot", "ggpubr"),
    require,
    character.only = TRUE
  ))
  x <- data[, var] %>% na.omit
  names(x)[1] <- 'newvar'
  
  map <- bxpmap(x,var, h = 1.5)
  
  
  bp <- ggplot(x, aes(x = "", y = newvar)) +
    geom_boxplot(fill = "gray80") +
    coord_flip() +
    theme_minimal() +
    theme(
      axis.title = element_blank(),
      axis.line.x = element_line(),
      panel.grid.major.y = element_blank(),
      # axis.title.y = element_text(),
      # axis.text.y=element_text(),
    )
  # bp <- ggplot_gtable(ggplot_build(bp))
  
  hist <-  ggplot(x, aes(newvar)) +
    geom_histogram(fill = "gray80",
                   col = "gray50",
                   aes(y = ..density..)) +
    geom_density(col = "blue") +
    theme_minimal() +
    theme(
      axis.title.x = element_blank(),
      # axis.line.x = element_line(),
      panel.grid = element_blank(),
      axis.text = element_blank(),
      panel.grid.major = element_blank(),
      axis.title.y = element_blank()
    )
  # hist <- ggplot_gtable(ggplot_build(hist))
  
  if (splot == TRUE)
    scpl <- ggscatter(
      data,
      x = var,
      y = vardep,
      # add = "loess",
      add = "reg.line",
      add.params = list(color = "red", fill = "lightcoral"),
      size = 0.6,
      conf.int = TRUE,
      ellipse = F,
      cor.coef = TRUE,
      cor.coeff.args = list(
        method = "pearson",
        label.x.npc = "center",
        label.y.npc = "top"
      ),
      # cor.coef.coord=c(2,2),
      xlab = var,
      ylab = vardep,
      ggtheme = theme_minimal()
    )
  
  if (splot == TRUE) {
    gd <-
      plot_grid(
        hist,
        bp,
        scpl,
        ncol = 1,
        align = 'v',
        rel_heights = c(0.35, 0.15, 0.4)
      )
    
  } else{
    gd <-
      plot_grid(
        hist,
        bp,
        ncol = 1,
        align = 'v',
        rel_heights = c(0.8, 0.2)
      )
    
  }
  g <- plot_grid(map, gd)
  g
}


##Funcion para crear el boxplot map
bxpmap <- function(x,var, h) {
  invisible(lapply(c("sf", "ggplot2"), require, character.only = TRUE))
  
  # Breaks
  qants <- quantile(x$newvar)
  minv <-  qants[[1]]
  q1 <- qants[[2]]
  q2 <- qants[[3]]
  q3 <- qants[[4]]
  q4 <- qants[[4]]
  maxv <- qants[[5]]
  vab <- q1 - (q3 - q1) * h
  vaa <- q3 + (q3 - q1) * h
  
  #labels
  
  lq1 <- paste0("<25%    [", round(vab, 3), "-", round(q1, 3), ")")
  lq2 <- paste0("25%-50% [", round(q1, 3), "-", round(q2, 3), ")")
  lq3 <- paste0("50%-75% [", round(q2, 3), "-", round(q3, 3), ")")
  lq4 <- paste0(">75%    [", round(q3, 3), "-", round(q4, 3), ")")
  lvab <-
    paste0("VAB     [", round(minv, 3), "-", round(vab, 3), ")")
  lvaa <-
    paste0("VAA     [", round(vaa, 3), "-", round(maxv, 3), "]")
  
  lbls <- c(lvab, lq1, lq2, lq3, lq4, lvaa)
  
  
  #new classified variable
  for (i in c(1:length(x$newvar))) {
    x$newvar1[i] <-
      ifelse(x$newvar[i] >= quantile(x$newvar)[1] &
               x$newvar[i] < quantile(x$newvar)[2],
             'Q1',
             0)
    x$newvar1[i] <-
      ifelse(x$newvar[i] < quantile(x$newvar)[2] - (quantile(x$newvar)[4] - quantile(x$newvar)[2]) *
               h,
             'VAB',
             x$newvar1[i])
    x$newvar1[i] <-
      ifelse(
        x$newvar[i] >= quantile(x$newvar)[2] &
          x$newvar[i] < quantile(x$newvar)[3],
        'Q2',
        x$newvar1[i]
      )
    x$newvar1[i] <-
      ifelse(
        x$newvar[i] >= quantile(x$newvar)[3] &
          x$newvar[i] < quantile(x$newvar)[4],
        'Q3',
        x$newvar1[i]
      )
    x$newvar1[i] <-
      ifelse(
        x$newvar[i] >= quantile(x$newvar)[4] &
          x$newvar[i] < quantile(x$newvar)[5],
        'Q4',
        x$newvar1[i]
      )
    x$newvar1[i] <-
      ifelse(x$newvar[i] >= quantile(x$newvar)[4] + (quantile(x$newvar)[4] - quantile(x$newvar)[2]) *
               h,
             'VAA',
             x$newvar1[i])
    x$varplot <- as.numeric(as.factor(x$newvar1))
  }
  x$newvar1 <-
    factor(x$newvar1, levels = c("VAB", 'Q1', 'Q2', "Q3", "Q4", "VAA"))
  
  
  # Colours
  pal <- c("#1A9850",
           "#91CF60",
           "#D9EF8B",
           "#FEE08B",
           "#FC8D59",
           "#D73027")
  
  ggplot(data = x) +
    ggtitle(var) +
    geom_sf(aes(fill = newvar1, col = newvar1)) +
    scale_colour_manual(
      values = pal,
      labels =  lbls,
      aesthetics = c("colour", "fill"),
      drop = FALSE
    ) +
    guides(colour = "none") +
    guides(fill = guide_legend(reverse = T)) +
    theme_minimal() +
    theme(
      axis.line = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      plot.title.position =  "plot",
      panel.grid.major = element_blank(),
      legend.key.height = unit(0.8, "lines"),
      legend.box.background = element_blank(),
      legend.position = c(.95, .05),
      legend.justification = c("right", "bottom"),
      legend.margin = margin(0),
      legend.title = element_blank()
    )
}
