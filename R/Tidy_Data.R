########################################################################
##########                        Modelos                     ##########
##########     Funciones para estructura y construcción       ##########
########################################################################



#' @title KS (Coeficiente de Kolmogorov-Smirvov)
#' @description Funcion para seleccionar variables que generan mayor divergencia entre las distribuciones de dos muestras aleatorias.
#' @param x variable continua
#' @param y variable continua
#' @export
TestKS <- function(x, y){
  if(class(x)!="character"){
    vars <- data.frame(y,x)
    vars_e <- subset(vars,subset=vars[,1]==1)
    vars_f <- subset(vars,subset=vars[,1]==0)
    ks <- suppressWarnings(ks.test(vars_e[,2],vars_f[,2],alternative="two.sided"))
    ks <- round(as.numeric(ks$statistic),4)
  } else{
    ks <- 0
  }
  return(ks)
}

#' @title IV (Valor de Informacion)
#' @description Funcion que mide el poder predectivo de una variable categorica para  problemas de clasifciacion binaria.
#' @param x variable categorica
#' @param y variable categorica
#' @export
TestVI <- function(x,y){
  if(class(x)=="character"){
    tc <- table(y,x)
    f1 <- tc[1,]
    f2 <- tc[2,]
    aux1 <- ifelse(f1/sum(f1)==0,0.001,ifelse(f1/sum(f1)==1,0.999, f1/sum(f1)))
    aux2 <- ifelse(f2/sum(f2)==0,0.001,ifelse(f2/sum(f2)==1,0.999, f2/sum(f2)))
    wof <- log(aux2/aux1)
    wof <- ifelse(wof==-Inf,0,wof)
    VI <-   sum(((f2/sum(f2))-(f1/sum(f1)))*wof)
  }else{
    VI <- 0
  }
  return(VI)
}

#' @title Correlacion (Correlacion superior a un valor dado)
#' @description Funcion que indica fuerza y direccion de una relacion lineal y proporcionalidad entre dos variables.
#' @param data list, data.frame o data.table
#' @param corr.max numero entre 0 y 1
#' @return  list, data.frame o data.table
#' @export
DVarCorr <- function(data, corr.max = 0.75){
  COR.AUX <- cor(data)
  pos <- which(((abs(COR.AUX)>=corr.max) & (row(COR.AUX) < col(COR.AUX))), arr.ind=T)
  if(nrow(pos)>0){
    col_elim <- numeric(nrow(pos))
    for(i in seq(1:nrow(pos))){
      aux_col_elim <- c(pos[i,1],pos[i,2])
      if (!any(col_elim %in% aux_col_elim)){
        col_elim [i] <- pos[i,which.max(c(pos[i,1],
                                          pos[i,2]))]
      }
    }
    if(length(col_elim)>0){
      col_elim <- unique(col_elim[col_elim>0])
      vars <- names(data)[-(col_elim)]
      data <- data.frame(data[,-(col_elim)])
      colnames(data) <- vars
    }
  }
  return(data)
}

#' @title Analisis de variables constantes
#' @description Funcion que analiza el porcentaje de variables constantes en una lista o data.frame.
#' @param x vector de datos
#' @param corr.max numero entre 0 y 1
#' @export
constante <- function(x){
  if(class(x)=="numeric"){
    cte <- min(x, na.rm = TRUE)==max(x, na.rm = TRUE)
  } else {
    tc <- prop.table(table(x))>=0.99
    cte <- any(tc)
  }
  return(cte)
}

#' @title Porcentaje de NA's
#' @description Funcion que analiza el porcentaje de NA's en un vector de datos.
#' @param x vector de datos
#' @export
porcNA <- function(x){
  porc <- mean(is.na(x))
  return(porc)
}

#' @title Diferencia de fechas en anios
#' @description Funcion que realiza la diferencia de fechas en anios.
#' @param end_date fecha final
#' @param start_date fecha inicial
#' @export
num_anios <- function(end_date, start_date) {
  ed <- as.POSIXlt(end_date)
  sd <- as.POSIXlt(start_date)
  anio <- floor((12 * (ed$year - sd$year) + (ed$mon - sd$mon))/12)
  return(anio)
}

#' @title Diferencia de fechas en meses
#' @description Funcion que realiza la diferencia de fechas en meses.
#' @param end_date fecha final
#' @param start_date fecha inicial
#' @export
num_meses <- function(end_date, start_date){
  ed <- as.POSIXlt(end_date)
  sd <- as.POSIXlt(start_date)
  mes <- floor(12 * (ed$year - sd$year) + (ed$mon - sd$mon))
  return(mes)
}

#' @title Funcion de reemplazo NA's
#' @description Funcion que reemplaza NA's de una base de datos con un valor indicado.
#' @param dt list, data.frame, data.table
#' @param vars variables de la base de datos
#' @param valor numero por el cual se reemplaza los NA's
#' @export
reemplazo_col = function(dt, vars, valor){
  na.replace = function(v, value=valor) { v[is.na(v)] = value; v }
  for (i in vars)
    eval(parse(text=paste("dt[,",i,":=na.replace(",i,")]")))
}

#' @title Encerar codigo PROV-CANT-PARR
#' @description Funcion encera el codigo provincia, canton, parroquia.
#' @param vector vector de datos
#' @export
cod2dig <- function(vector){
  vector <- ifelse(str_length(vector)==1, paste0('0', vector), vector)
  return(vector)
}
