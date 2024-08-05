
# este es una prueba de github
# y1<-as.numeric(format(F_Cierre,"%Y"))
# m1<-as.numeric(format(F_Cierre,"%m"))

# Se cargan las bases de datos para el siguiente corte:
BD_inicial <- read_delim("Tablas/BD_inicio_Mes/BD_inicial_Garantizada.csv", 
                         ";", escape_double = FALSE, locale = locale(decimal_mark = ","), 
                         trim_ws = TRUE)
Inflacion <- setDT(read_excel("Tablas/Inflacion.xlsx"))

Probabilidad_revocar <- read_excel("Tablas/Probabilidad_revocar_202207.xlsx")
Probabilidad_decremento <- read_excel("Tablas/Probabilidad_decremento_202207.xlsx")
Prob_Rev<-setDT(Probabilidad_revocar)

######################################################################################################
# Esta tabla se actualiza en Enero, abril, julio y octubre con la actualizaci?n de las tarifas
BD_TRM <- read_excel("Tablas/BD_TRM.xlsx")
######################################################################################################

# Resultados del mes anterior
#load("R/Rva_Mat_Cierre_Anterior.RData")

##################################
#        DATOS DEL CIERRE        #
##################################

Tasa_ret<-0.9365
source("R/Funciones_Edu.R")
BD_inicial <- read_delim("Tablas/BD_inicio_Mes/BD_inicial_Garantizada.csv", 
                         ";", escape_double = FALSE, locale = locale(decimal_mark = ","), 
                         trim_ws = TRUE)

#################################

Mat<-setDT(Garantizado)

# Se incluye la informaci?n del fiu, inter?s t?cnico y gastos
BD_Primas <- Compra
Mat <- merge(Mat,BD_inicial,by.x = "nro_pol",by.y = "Poliza",all.x=TRUE)

source("R/Nuevos_garantizada.R")

Mat<-setDT(Garantizado)
Mat<-merge(Mat,BD_final,by.x = "nro_pol",by.y = "Poliza",all.x=TRUE)
Mat<-Mat[max_fec_maduracion>as.Date("2009-01-01")] # No se incluyen en reserva las p?lizas que terminaron vigecia antes de esta fecha

#######################################
#--------------- ALERTA --------------#
#######################################

# Validar las siguientes p?lizas que no cruzaron con la tabla de tarifas
ALERTA<-Nuevos_3

#Esta es la validaci?n de los elementos no encontrados
Polizas_Borradas<-Mat[is.na(fiu)][,.(nro_pol,Maduracion)] # Si no encuentra una p?liza lo m?s probable es que hayan pasado más de 10 a?os desde la fecha de maduración sin activación.

# Sacamos las pólizas para la que no se tiene informaci?n
Mat<-Mat[!is.na(fiu)]


###########################################
#           CALCULO DE LA RESERVA         #
###########################################

# Se ajusta la fecha de maduración de esta póliza por cambio de beneficiario
Mat[nro_pol==26271]$Maduracion<-2030


# Meses hasta la maduraci?n

Mat$m<-pmax((Mat$Maduracion-(y1+1))*12+(12-m1),rep(0,dim(Mat)[1])) #### Sum? el +1 en (12-m1+1) para que coincida con actuar?a, sin embargo hay que quitarlo en el siguiente mes.

# Variable para almacenar la reserva por cada unidad monetaria

Mat$Fac<-rep(0,dim(Mat)[1])

# Construcci?n de la reserva, en funci?n del n?mero de semestres contratados
Mat$fiu<-as.numeric(Mat$fiu)
Mat$it<-ifelse(Mat$fec_emi_min>"2020-05-01",pmin(as.numeric(Mat$it),as.numeric(Mat$it_venta)),Mat$it)

#######################################################################################################
Mat[nro_pol==85204]$st<-6 # Se trata de un parche porque el sistema arroja 11 semestres ###VALIDAR#####
#######################################################################################################

Mat$Flujo_0<-ifelse(Mat$st-1>=0,(1+Mat$fiu)^(Mat$m/12+floor(6*0/12))*(1+Mat$it)^-((Mat$m+6*0)/12),0)*CS


######################################################################################
#                   Construcción de los flujos
######################################################################################


# Calcular el número máximo de periodos de flujo
max_periods <- max(Mat$st) * 6 + max(Mat$m)


# Función para crear los flujos de pagos
create_flows <- function(fiu, it, m, st, CS) {
  # Inicializar los flujos con ceros
  flows <- numeric(max_periods)
  
  # Calcular los flujos cada 6 meses a partir del mes m
  for (i in 1:st) {  # Usa 1:st para iterar correctamente por el número de semestres
    index <- m + 6 * (i - 1)
    if (index <= max_periods) {
      flows[index] <- (1 + fiu)^(m/12 + floor(6*(i-1)/12)) * (1 + it)^-((m + 6*(i-1))/12) * CS
    }
  }
  
  return(flows)
}

# Aplicar la función para crear los flujos en cada fila de Mat
Mat[, (paste0("Flujo_", seq_len(max_periods))) := as.list(create_flows(fiu, it, m, st, CS)), by = 1:nrow(Mat)]

Mat$Flujo_1<-ifelse(Mat$m==0, Mat$Flujo_0,Mat$Flujo_1)


columnas_flujos <- paste0("Flujo_", seq_len(max_periods))
flujos <- Mat[, c("nro_pol", columnas_flujos), with = FALSE]


# Eliminar las columnas de flujos de Mat
columnas_flujos <- paste0("Flujo_", seq_len(max_periods))
Mat[, (columnas_flujos) := NULL]  # Esto elimina las columnas de flujo de Mat

# Calcular la suma de todos los flujos en cada fila de la matriz flujos y añadir como nueva columna
flujos[, Suma_Flujos := rowSums(.SD), .SDcols = columnas_flujos]


######################################################################################


#Informaci?n para la construcci?n de los flujos
Mat$Flujo_0<-ifelse(Mat$st-1>=0,(1+Mat$fiu)^(Mat$m/12+floor(6*0/12))*(1+Mat$it)^-((Mat$m+6*0)/12),0)*CS
Mat$Flujo_1<-ifelse(Mat$st-1>=1,(1+Mat$fiu)^(Mat$m/12+floor(6*1/12))*(1+Mat$it)^-((Mat$m+6*1)/12),0)*CS
Mat$Flujo_2<-ifelse(Mat$st-1>=2,(1+Mat$fiu)^(Mat$m/12+floor(6*2/12))*(1+Mat$it)^-((Mat$m+6*2)/12),0)*CS
Mat$Flujo_3<-ifelse(Mat$st-1>=3,(1+Mat$fiu)^(Mat$m/12+floor(6*3/12))*(1+Mat$it)^-((Mat$m+6*3)/12),0)*CS
Mat$Flujo_4<-ifelse(Mat$st-1>=4,(1+Mat$fiu)^(Mat$m/12+floor(6*4/12))*(1+Mat$it)^-((Mat$m+6*4)/12),0)*CS
Mat$Flujo_5<-ifelse(Mat$st-1>=5,(1+Mat$fiu)^(Mat$m/12+floor(6*5/12))*(1+Mat$it)^-((Mat$m+6*5)/12),0)*CS
Mat$Flujo_6<-ifelse(Mat$st-1>=6,(1+Mat$fiu)^(Mat$m/12+floor(6*6/12))*(1+Mat$it)^-((Mat$m+6*6)/12),0)*CS
Mat$Flujo_7<-ifelse(Mat$st-1>=7,(1+Mat$fiu)^(Mat$m/12+floor(6*7/12))*(1+Mat$it)^-((Mat$m+6*7)/12),0)*CS
Mat$Flujo_8<-ifelse(Mat$st-1>=8,(1+Mat$fiu)^(Mat$m/12+floor(6*8/12))*(1+Mat$it)^-((Mat$m+6*8)/12),0)*CS
Mat$Flujo_9<-ifelse(Mat$st-1>=9,(1+Mat$fiu)^(Mat$m/12+floor(6*9/12))*(1+Mat$it)^-((Mat$m+6*9)/12),0)*CS

Mat$VA_flujos<-Mat$Flujo_0+Mat$Flujo_1+Mat$Flujo_2+Mat$Flujo_3+Mat$Flujo_4+Mat$Flujo_5+Mat$Flujo_6+Mat$Flujo_7+Mat$Flujo_8+Mat$Flujo_9     


Mat[,.(nro_pol,VA_flujos)]


for (k in seq(0,9)) {
  Aux<-ifelse(Mat$st-1>=k,(1+Mat$fiu)^(Mat$m/12+floor(6*k/12))*(1+Mat$it)^-((Mat$m+6*k)/12),0)
  Mat$Fac<-Mat$Fac+Aux
}

# El t?rmino Fac est? dado por unidad monetaria, sin embargo falta multiplicarlo por el CS y por la tasa de retiro
Mat$Contratados<-Mat$st
Mat<-merge(Mat,Probabilidad_decremento,by.x =c("st","Contratados"),by.y = c("Reserva","Contratados"),all.x = T)
Mat$Reserva<- CS*Tasa_ret*Mat$Fac*(1-Mat$decremento)

# Resumen de los resultados
Mat[,.(Reserva=sum(Reserva),.N),by=.(Producto_Act)]
Mat[,sum(Reserva)]
Mat[,length(Reserva)]

#-----------------------------------------# 
#           CALCULO DEL RESCATE           #
#-----------------------------------------# 

# se toma la base de datos que contiene la informaci?n de la compra
Compra<-Compra1
setDT(Compra)
Compra$fec_vig_desde<-as.Date(Compra$fec_vig_desde)
Compra$fec_carga<-data.table::fifelse(is.na(Compra$fec_carga),as.Date(Compra$fec_vig_desde),as.Date(Compra$fec_carga))

# Ajustar el estado de las p?lizas que fueron canceladas posterior al cierre 
Compra$cod_estado<-ifelse(Compra$id_solicitud %in% Endo_Cancelada_anti$nro_pol,23,as.numeric(Compra$cod_estado))

# Incluir la fecha desde de cada aporte
Compra<-merge(Compra,Educativo_Copia[,.(id_pv,fec_emi)],by.x ="idPvEmi",by.y = "id_pv",all.x = T )
Compra$fec_carga<-data.table::fifelse(is.na(Compra$fec_emi),as.Date(Compra$fec_carga),as.Date(Compra$fec_emi))
BD_Primas<-Compra[fec_carga<=F_Cierre & cod_estado==23]

# Se toma la base de primas y se agrega la fecha de emisi?n m?nima de compra desde la base Educativo
BD_Primas<-merge(BD_Primas, 
                 Educativo[,.("fec_emi_mi"=min(fec_emi)),.(id_pv,nro_pol)],
                 by.x=c("nro_pol","idPvEmi"),
                 by.y = c("nro_pol","id_pv")
)

BD_Primas$imp_extraprima<-BD_Primas[,.(ifelse(is.na(imp_extraprima),0,imp_extraprima))]

y<-BD_Primas[,.("Poliza"= nro_pol,
                "Emision" = as.Date(fec_emi_mi),
                "Prima" = as.numeric(imp_prima_estim)-as.numeric(imp_extraprima),
                "Endoso"= nro_consec_sol)
][Emision<=F_Cierre]

y<-unique(y)

Rescate<-Rescate3(y)
Rescate<-unique(Rescate)

Primas <-dcast(BD_Primas,nro_pol~nro_consec_sol,sum, value.var = "imp_prima_estim")
names(Primas)=c("Poliza","Prima_0","Prima_1","Prima_2","Prima_3","Prima_4","Prima_5","Prima_6","Prima_7","Prima_8","Prima_9","Prima_10","Prima_11")
meses_resca<-Rescate[,.("meses"=min(meses)),.(Poliza)]
Rescate <-dcast(Rescate,Poliza~Endoso, sum , value.var = "Rescate")
names(Rescate)=c("Poliza","Rescate_0","Rescate_1","Rescate_2","Rescate_3","Rescate_4","Rescate_5","Rescate_6","Rescate_7","Rescate_8","Rescate_9","Rescate_10","Rescate_11")
Rescate<-merge(Rescate,meses_resca,by="Poliza")
Prim_Resc<-merge(Rescate,Primas,by.x = "Poliza",by.y = "Poliza")
Mat<-unique(Mat)
Mat<-merge(Mat,Prim_Resc,by.x = "nro_pol",by.y = "Poliza",all.x =TRUE )
Mat$Rescate_Total <- Mat$Rescate_0+Mat$Rescate_1+Mat$Rescate_2+Mat$Rescate_3+Mat$Rescate_4+Mat$Rescate_5+Mat$Rescate_6+Mat$Rescate_7+Mat$Rescate_8+Mat$Rescate_9+Mat$Rescate_10+Mat$Rescate_11
Mat[,.(Rescate_Total=sum(Rescate_Total)),by=.(Producto_Act)]
Mat$Rescate_Total<- Mat[,ifelse(is.na(Rescate_Total),0,Rescate_Total)]
Mat[,sum(Rescate_Total)]
Mat[,.N]

########################################### 
#       AJUSTE POR PROBABILIDADES         #
###########################################

#Mat$n<-round(((Mat$Maduracion-year(F_Cierre)-1)*12 + (12 - month(F_Cierre)))/12,0)    # Esta es la f?rmula correcta, pero no se puede utilizar hasta que Edwin realice el ajuste en el sistema
Mat$n<- pmin(18,round2(((Mat$Maduracion-(year(F_Cierre)+1))*12 + (12 - month(F_Cierre)+1))/12,0))  # Miestras tanto dejamos esta!

setkeyv(Mat,"n")
setkeyv(Prob_Rev,"n")
Mat<-Prob_Rev[Mat]
Mat$Reserva_Final<-Mat[,Reserva*Prob_estudiar+Rescate_Total*Prob_revocar]
Mat[,.(Reserva_Final=sum(Reserva_Final)),by=.(Producto_Act)]
Mat[,.(Reserva_Final=sum(Reserva_Final))]

########################################### 
#             RESERVA DE GASTOS           #
###########################################

Mat$miu<- pmax((Mat$Maduracion),year(F_Cierre))- year(Mat$fec_emi_min)+Mat$st/2
Mat$Altura<-floor(FACTOR_30_360(Mat$fec_emi_min,F_Cierre))+1
Mat$year <- as.numeric(format(Mat$fec_emi_min,"%Y"))
setkeyv(Mat,"year")
Inflacion_F<-Inflacion[,.(year,Acumulado_F)]
setkeyv(Inflacion_F,"year")
Mat <- Inflacion_F[Mat]

Mat$Reserva_Gastos<-as.numeric(Mat$Gasto_Inicial)*Mat$Acumulado_F*(Mat$miu-Mat$Altura)*Mat[,ifelse(Producto_Act=="ETAPAS"|Producto_Act=="SEMESTRES",st,1)]
Mat$Reserva_Balance<- Mat$Reserva_Gastos+Mat$Reserva_Final
Mat[,.(Reserva_Balance=sum(Reserva_Balance))]

########################################### 
#       VALIDACIONES Y RESULTADOS         #
###########################################

Mat<-unique(Mat)
Mat_result<-Mat
Mat_result$Producto_Act<-ifelse(Mat_result$Producto_Act=="ETAPAS","SEMESTRES",Mat_result$Producto_Act)

(Resultados<- Mat_result[,.("Reserva"=sum(Reserva),"Rescate"=sum(Rescate_Total), "Reserva_Final"=sum(Reserva_Final), "Reserva_Gastos"=sum(Reserva_Gastos), "Reserva_Balance"=sum(Reserva_Balance),"Cantidad"=length(Reserva)),by=.(Producto_Act)])

# Orden deseado
orden_deseado <- c("PUG", "UG", "PLATINO", "SEMESTRES")
Resultados[, Producto_Act := factor(Producto_Act, levels = orden_deseado)]
Resultados <- Resultados[order(Producto_Act)]


write.csv2(Resultados,paste("Resultados/Rva Matematica/Garantizada/GlobalLink Matematica Garantizada",F_Cierre," V4 .csv"))
write.csv2(Mat,paste("Resultados/Rva Matematica/Garantizada/Validacion Reserva Matematica Garantizado",F_Cierre," V4 .csv"))

Mat_Planes_Completos<-Mat
hs <- createStyle(textDecoration = "BOLD",halign = "CENTER" ,fontColour = "#FFFFFF",  border = "Bottom",fontSize=12, fontName="Arial Narrow", fgFill = "#4F80BD")


########################################### 
#               REASEGURO                 #
###########################################

Reaseguro <- Mat[,.(nro_pol,Reaseguro,Prima_pura,Cesion,Reserva_Balance,Reserva_Gastos)]
Reaseguro$Activo_contin<-(Reaseguro$Reserva_Balance-Reaseguro$Reserva_Gastos)*Reaseguro$Cesion*Reaseguro$Reaseguro
Act_contingente<-sum(Reaseguro$Activo_contin)

# Cesión mes anterior
cesion_anterior<-335885729532
Retefuente_anterior<-cesion_anterior*0.01
Deposito_anterior<-66952268121

# Cesión nuevos del mes
cesion_nuevos<-Mat[Nuevos==1,.("Prima"=sum(Prima_0,Prima_1,Prima_2),Prima_pura,Cesion),.(nro_pol)]
cesion_nuevos$cesion<-(cesion_nuevos$Prima*cesion_nuevos$Prima_pura/100)*cesion_nuevos$Cesion
Primas_mes<-sum(cesion_nuevos$Prima)
cesion_nuevos<-sum(cesion_nuevos$cesion)
print(c("Prima comercial nuevos del mes", format(Primas_mes, big.mark = ",", decimal.mark = ".", nsmall = 0)))
print(c("Cesión nuevos del mes", format(cesion_nuevos, big.mark = ",", decimal.mark = ".", nsmall = 0)))
cesion_nuevos/Primas_mes

Retefuente_nuevo<-cesion_nuevos*0.01
Deposito_nuevos<-cesion_nuevos*0.2

# Cesión adiciones de etapas

##################################################################
# cesión final
cesion_final<-cesion_anterior+cesion_nuevos

# Retención final
Retefuente_final<-Retefuente_anterior+Retefuente_nuevo

# Depósito final
Deposito_final<-Deposito_anterior+ Deposito_nuevos# - liberación de depósito por cancelaciones

# Contingencias a cargo de reasegurador
Contingencia_reasegurador<-Act_contingente-Deposito_final
print(c("Las contingencias a cargo del reasegurador son de:", format(Contingencia_reasegurador, big.mark = ",", decimal.mark = ".", nsmall = 0)))

# Deterioro
Per_Deterioro <- 0.0169
Deterioro<-Contingencia_reasegurador*Per_Deterioro
print(c("El deterioro del activo es de:", format(Deterioro, big.mark = ",", decimal.mark = ".", nsmall = 0)))




