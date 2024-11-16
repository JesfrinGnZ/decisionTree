install.packages("rpart")
install.packages("rpart.plot")

library(rpart)
library(rpart.plot)

data_hogar <-read.csv('/home/jesfrin/Documentos/maestria/4to trimestre/mineria-de-datos/tareas/tarea5/HOGAR_BDP.csv', sep = ",")

# lA VIVIENDA ESTA EN EL AREA RURAL O URBANA
# Modelo #1
arbol <- rpart(AREA ~ PCH1 + PCH2 + PCH4 + PCH5 + ZONA,
               data = data_hogar, method = "class")

# árbol #1
rpart.plot(arbol, type=2, extra=0, under=TRUE, fallen.leaves=TRUE,
           box.palette="BuGn", main="Predicción de AREA", cex=1)

#Cuenta este hogar con carro
# Modelo #2
arbol2 <- rpart(PCH9_M ~ 
                PCH9_D +
                PCH9_F +
                PCH9_G +
                PCH9_H +
                PCH9_K +
                PCH9_L,
               data = data_hogar, method = "class")

# árbol #2
rpart.plot(arbol2, type=2, extra=0, under=TRUE, fallen.leaves=TRUE,
           box.palette="BuGn", main="¿Cuenta este hogar con carro?", cex=1)


#Cuenta este hogar con carro
# Modelo #3
arbol3 <- rpart(PCH9_M ~ 
                  PCH9_G +
                  PCH9_H +
                  PCH9_K,
                data = data_hogar, method = "class")

# árbol #3
rpart.plot(arbol3, type=2, extra=0, under=TRUE, fallen.leaves=TRUE,
           box.palette="BuGn", main="¿Cuenta este hogar con carro?", cex=1)



# Crear un nuevo hogar con variables específicas
# El hogar cuenta con lavadora de ropa, no cuenta con computadora y no cuenta con sistema de agua caliente 
nuevo_hogar <- data.frame(
  PCH9_G = c(1),
  PCH9_H = c(2),
  PCH9_K = c(2)
)

#  predicción
resultado <- predict(arbol3, nuevo_hogar, type = "class")



# Crear un nuevo hogar con variables específicas
# El hogar cuenta con lavadora de ropa, cuenta con computadora y no cuenta con sistema de agua caliente 
nuevo_hogar <- data.frame(
  PCH9_G = c(1),
  PCH9_H = c(1),
  PCH9_K = c(2)
)

#  predicción
resultado <- predict(arbol3, nuevo_hogar, type = "class")



