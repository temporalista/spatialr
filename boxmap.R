#### BOXMAPR Mapa infográfico múltiple
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


boxmapr <- function(data,var, var2, h=1.5){
  #based on https://rpubs.com/helson/608377
  invisible(lapply(c("sf", "ggplot2","gridExtra","ggpubr"), require, character.only = TRUE))
  x <- data[,var]
  names(x)[1] <- 'newvar'
  
  # brkpts <- boxplot(x$newvar,range = h)$stats
  # vab <- quantile(x$newvar)[2] - (quantile(x$newvar)[4]- quantile(x$newvar)[2])*h
  # minv <- quantile(x$newvar)[1]
  # q1 <- 
  #   min()
  
  for (i in c(1:length(x$newvar))) {
    x$newvar1[i] <- ifelse(x$newvar[i] >= quantile(x$newvar)[1] & x$newvar[i] < quantile(x$newvar)[2], 'Q1', 0)
    x$newvar1[i] <- ifelse(x$newvar[i] < quantile(x$newvar)[2] - (quantile(x$newvar)[4]- quantile(x$newvar)[2])*h, 'VAB',  x$newvar1[i])
    x$newvar1[i] <- ifelse(x$newvar[i] >= quantile(x$newvar)[2] & x$newvar[i] < quantile(x$newvar)[3], 'Q2', x$newvar1[i])
    x$newvar1[i] <- ifelse(x$newvar[i] >= quantile(x$newvar)[3] & x$newvar[i] < quantile(x$newvar)[4], 'Q3', x$newvar1[i])
    x$newvar1[i] <- ifelse(x$newvar[i] >= quantile(x$newvar)[4] & x$newvar[i] < quantile(x$newvar)[5], 'Q4', x$newvar1[i])
    x$newvar1[i] <- ifelse(x$newvar[i] >= quantile(x$newvar)[4] + (quantile(x$newvar)[4]- quantile(x$newvar)[2])*h, 'VAA', x$newvar1[i])
    x$varplot <- as.numeric(as.factor(x$newvar1))
  }
  x$newvar1 <- factor(x$newvar1, levels=c("VAB",'Q1','Q2',"Q3","Q4","VAA"))
  
  pal <- c("VAB"="#1A9850",
           'Q1'="#91CF60",
           'Q2'="#D9EF8B",
           "Q3"="#FEE08B",
           'Q4'="#FC8D59",
           "VAA"="#D73027")
  
  lbls <- c(paste("VAB"="#1A9850",
                  'Q1'="#91CF60",
                  'Q2'="#D9EF8B",
                  "Q3"="#FEE08B",
                  'Q4'="#FC8D59",
                  "VAA"="#D73027"))
  
  map <- ggplot(data= x) +
    ggtitle(var) +
    geom_sf(aes(fill = newvar1, col=newvar1))+
    scale_colour_manual(values = pal,  
                        aesthetics = c("colour", "fill"),
                        drop = FALSE)+
    guides(colour = "none")+
    guides(fill = guide_legend(reverse=T))+
    theme_minimal()+
    theme(axis.line=element_blank(),
          axis.text.x=element_blank(),
          axis.text.y=element_blank(),
          plot.title.position =  "plot",
          panel.grid.major=element_blank(),
          legend.key.height=unit(0.8, "lines"),
          legend.box.background = element_blank(),
          legend.position = c(.9, .05),
          legend.justification = c("right", "bottom"),
          legend.margin = margin(0),
          legend.title=element_blank()
    )
  
  bp <- ggplot(x, aes(x="",y=newvar)) +
    geom_boxplot(fill="gray80")+
    coord_flip()+
    theme_minimal()+
    theme(axis.title = element_blank(),
          panel.grid.major =element_blank(),
          axis.title.y = element_blank(),
          axis.text=element_blank()
    )
  
  hist <-  ggplot(x, aes(newvar)) +
    geom_histogram(fill="gray80",
                   col="gray50",
                   aes(y =..density..))+
    geom_density(col="blue") + 
    theme_minimal()+
    theme(axis.title.x = element_blank(),
          panel.grid =element_blank(),
          axis.text=element_blank(),
          panel.grid.major =element_blank(),
          axis.title.y = element_blank()
    )
  
  scpl <- ggscatter(data, x = var, y = var2, 
                    # add = "loess",
                    add = "reg.line",
                    add.params=list(color = "red",fill = "lightcoral"),
                    size=0.6,
                    conf.int = TRUE, 
                    ellipse=F,
                    cor.coef = TRUE, 
                    cor.coeff.args = list(method = "pearson", label.x.npc = "center", label.y.npc = "top"),
                    # cor.coef.coord=c(2,2),
                    xlab = var, ylab = var2,
                    ggtheme=theme_minimal()
  )
  
  
  g <- grid.arrange(map,                             # First row with one plot spaning over 2 columns
                    arrangeGrob(hist, bp,scpl, nrow = 3, heights=c(0.4,0.1,0.4)), # Second row with 2 plots in 2 different columns
                    nrow = 1   # Number of rows
                    
  )                      
}
