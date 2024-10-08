# 0. Preparacion encuesta estudiantes ola 1. Se realiza un procesamiento del índice
#de evaluación de justicia en las notas

# 1. cargar librerias ---------------------------------------------------------

#install.packages("pacman")
pacman::p_load(dplyr, sjmisc, car, sjlabelled, stargazer, haven, sjPlot, summarytools)


# 2. cargar bbdd --------------------------------------------------------------
rm(list=ls())       # borrar todos los objetos en el espacio de trabajo
options(scipen=999) # valores sin notación científica


load("input/data/proc/ola1.RData")


# 3. procesamiento de variables -----------------------------------------------

# evaluacion de justicia ----

proc_datos <- proc_datos %>% mutate(justicia_nota = log(nota_obtenida/nota_pref))

summary(proc_datos$justicia_nota)

# prueba con pregunta que añade tiempo 
proc_datos <- proc_datos %>%
  mutate(
    justicia_nota_tiempo = dplyr::case_when(
      justicia_nota < 0  |notas_esfuerzo == 1 ~ 1,
      justicia_nota == 0 | notas_esfuerzo == 2 ~ 2, 
      justicia_nota > 0  | notas_esfuerzo == 3 ~ 3)) 

proc_datos$justicia_nota_tiempo <- factor(proc_datos$justicia_nota_tiempo, 
                                          levels=c(1,2,3),
                                          labels=c("injusticia sub-recompensa",
                                                   "justicia perfecta",
                                                   "injusticia sobre-recompensa"))


proc_datos <- proc_datos %>%
  mutate(
    justicia_nota = dplyr::case_when(
      justicia_nota < 0  ~ 1,
      justicia_nota == 0 | notas_merit == 2 ~ 2, 
      justicia_nota > 0  ~ 3
    )
  ) 

proc_datos$justicia_nota <- factor(proc_datos$justicia_nota, 
                                          levels=c(1,2,3),
                                          labels=c("injusticia sub-recompensa",
                                                   "justicia perfecta",
                                                   "injusticia sobre-recompensa"))
frq(proc_datos$justicia_nota)  



# 4. base procesada -----------------------------------------------------------
proc_datos <-as.data.frame(proc_datos)
stargazer(proc_datos, type="text")

save(proc_datos,file = "input/data/proc/ola1.RData")
