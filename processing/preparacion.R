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

# rendimiento ----

proc_datos <- proc_datos %>% rename(rendimiento = nota_obtenida)

# evaluacion de justicia ----

# nota preferida 
proc_datos <- proc_datos %>%
  mutate(nota_pref = case_when(
    is.na(nota_pref) & notas_merit == 2 ~ rendimiento ,  # Imputar el valor de 'notas_merit' (2) cuando 'nota_pref' es NA
    TRUE ~ nota_pref  # Mantener los valores existentes de 'nota_pref' cuando no son NA
  ))

# justicia 
proc_datos <- proc_datos %>% mutate(justicia_nota = log(rendimiento/nota_pref))
                                    
                              

summary(proc_datos$justicia_nota)




## justicia categorica

# prueba con pregunta que añade tiempo 
proc_datos <- proc_datos %>%
  mutate(
    justicia_nota_tiempo_ch = dplyr::case_when(
      justicia_nota < 0  |notas_esfuerzo == 1 ~ 1,
      justicia_nota == 0 | notas_esfuerzo == 2 ~ 2, 
      justicia_nota > 0  | notas_esfuerzo == 3 ~ 3)) 

proc_datos$justicia_nota_tiempo_ch <- factor(proc_datos$justicia_nota_tiempo_ch, 
                                          levels=c(1,2,3),
                                          labels=c("injusticia sub-recompensa",
                                                   "justicia perfecta",
                                                   "injusticia sobre-recompensa"))


proc_datos <- proc_datos %>%
  mutate(
    justicia_nota_ch = dplyr::case_when(
      justicia_nota < 0  ~ 1,
      justicia_nota == 0 ~ 2, 
      justicia_nota > 0  ~ 3
    )
  ) 

proc_datos$justicia_nota_ch <- factor(proc_datos$justicia_nota_ch, 
                                          levels=c(1,2,3),
                                          labels=c("injusticia sub-recompensa",
                                                   "justicia perfecta",
                                                   "injusticia sobre-recompensa"))
frq(proc_datos$justicia_nota_ch)  


# 4. base procesada -----------------------------------------------------------
proc_datos <-as.data.frame(proc_datos)
stargazer(proc_datos, type="text")

save(proc_datos,file = "input/data/proc/ola1.RData")
