# 0. Preparacion encuesta estudiantes experimento Ola 2.
# 1. cargar librerias ---------------------------------------------------------

#install.packages("pacman")
pacman::p_load(dplyr, sjmisc, car, sjlabelled, stargazer, haven)


# 2. cargar bbdd --------------------------------------------------------------

datos <- read_sav("input/data/original/251024 Edumer Ola2 ID.sav")
names(datos)
frq(datos$p1_o2)

# 3. seleccionar variables ----------------------------------------------------

datos_exp <- datos %>% select(T_pex_1_1_o2, T_pex_1_2_o2,  T_pex_2_1_o2, T_pex_2_2_o2, 
                              T_pex_3_1_o2, T_pex_3_2_o2, T_pex_4_1_o2, T_pex_4_2_o2, 
                              T_pex_4_3_o2, T_pex_4_4_o2, p14_o2, d3_def_o1, p3_o2,
                              p4_o2, p5_o2,p6_o2, p7_o2, p17_o2, p18_o2, p19_o2, p1_1_o2,
                              p1_2_o2, p1_9_o2, p1_10_o2, p2_1_o2, p2_2_o2, p2_3_o2) 

#renombrar 
datos_exp <- datos_exp %>% rename(c1_A = T_pex_1_1_o2,
                                    c1_B = T_pex_1_2_o2,
                                    c2_A = T_pex_2_1_o2, 
                                    c2_B = T_pex_2_2_o2,
                                    c3_A = T_pex_3_1_o2, 
                                    c3_B = T_pex_3_2_o2,
                                    c4_A = T_pex_4_1_o2, 
                                    c4_B = T_pex_4_2_o2,
                                    c4_C = T_pex_4_3_o2, 
                                    c4_D = T_pex_4_4_o2,
                                    genero = p14_o2,
                                    curso = d3_def_o1,
                                    school_pref = p3_o2,
                                    nota_ob = p4_o2, 
                                    nota_merit = p5_o2, 
                                    nota_pref = p6_o2, 
                                    nota_merit_esf = p7_o2,
                                    ne_madre = p17_o2,
                                    ne_padre = p18_o2, 
                                    libros_hogar = p19_o2,
                                  social_esfuerzo = p1_1_o2 ,
                                  social_talento = p1_2_o2, 
                                  igual_oportunidades = p1_9_o2, 
                                  social_merito = p1_10_o2,
                                  school_esfuerzo = p2_1_o2,
                                  school_talento = p2_2_o2, 
                                  school_merito = p2_3_o2
                                    
                                  
                                    )



# 4. procesamiento de variables -----------------------------------------------

#ordenar por variable (20)

# SOCIODEMOGRAFICOS ------------------------------------------------------------

## curso_estudiante ----

### a. descriptivo basico ----
frq(datos_exp$curso) #no tiene NA 

### b. recodificacion ----
#datos_exp <- datos_exp %>%
#mutate(curso_estudiante = case_when(
#grepl("^(2|1|II|segundo|Segundo|sugundo|Media)", curso_estudiante, ignore.case = TRUE) ~ 'Media',
#grepl("^(7|6|Basica|Septimo|Séptimo|septimo|séptimo|sexto)", curso_estudiante, ignore.case = TRUE) ~ 'Básica',
#TRUE ~ curso_estudiante  # mantener el valor original si no coincide con ninguna condición
#))

datos_exp <- datos_exp %>%
  mutate(curso = case_when(
    grepl("^(2|1)", curso, ignore.case = TRUE) ~ 'Media',
    grepl("^(7|6)", curso, ignore.case = TRUE) ~ 'Básica',
    TRUE ~ curso  # mantener el valor original si no coincide con ninguna condición
  ))

## genero_ES ----

### a. descriptivo basico ----
frq(datos_exp$genero) #muestra con una mayoria de muejeres 48.52% y 
#categoria otro: 4.52%. No tiene casos perdidos
datos_exp$genero <- recode(datos_exp$genero, "3=NA")

### b. recodificacion ----
datos_exp$genero <- factor(datos_exp$genero, 
                               levels=c(1,2),
                               labels=c("Hombre","Mujer"))


## ne más alto padres
datos_exp$ne_madre <- recode(datos_exp$ne_madre, "c(88,99)=NA")
datos_exp$ne_padre <- recode(datos_exp$ne_padre, "c(88,99)=NA")

datos_exp <- datos_exp %>% 
  dplyr::mutate(
    ne_madre = haven::zap_labels(ne_madre),
    ne_padre = haven::zap_labels(ne_padre),
    educ_max = case_when(
      !is.na(ne_madre) & is.na(ne_padre) ~ ne_madre,
      is.na(ne_madre) & !is.na(ne_padre) ~ ne_padre,
      !is.na(ne_madre) & !is.na(ne_padre) ~ pmax(ne_madre, ne_padre, na.rm = TRUE),
      TRUE ~ NA_real_
    )
  )

frq(datos_exp$educ_max)

datos_exp$educ_max <- recode(datos_exp$educ_max, "1=1; 2=1; 3=1; 4=2; 5=2; 6=2")

datos_exp$educ_max <- factor(datos_exp$educ_max, 
                              levels=c(1,2),
                              labels=c("Enseñanza media o menos","Estudios superiores"))

## libros_hogar ----
### a, descriptivo basico
frq(datos_exp$libros_hogar)

### b. recodificación ----
datos_exp$libros_hogar <- recode(datos_exp$libros_hogar, "c(88,99)=NA; 1=1;2=1; 3=2; 4=2;5=2;6=2")

datos_exp$libros_hogar <- factor(datos_exp$libros_hogar, 
                                  levels=c(1,2),
                                  labels=c("Menos de 25 libros","Más de 25 libros"))

# NOTAS ------------------------------------------------------------------------

## notas_merit ----

### a. descriptivo basico ----
frq(datos_exp$nota_merit) #Buen sentido. Casos perdidos: 
#88 (no sabe) 1 caso; 99 (preferiria no responder) 0 casos

### b. recodificacion ----
datos_exp$nota_merit <- recode(datos_exp$nota_merit, "c(88,99)=NA")


## notas_esfuerzo ----

### a. descriptivo basico ----
frq(datos_exp$nota_merit_esf) #Buen sentido. Casos perdidos: 
#88 (no sabe) 4 casos; 99 (preferiria no responder) 3 casos

### b. recodificacion ----
datos_exp$nota_merit_esf <- recode(datos_exp$nota_merit_esf, "c(88,99)=NA")

## justicia nota  ----

# rendimiento ----

datos_exp <- datos_exp %>% rename(rendimiento = nota_ob)

# evaluacion de justicia ----

# nota preferida 
datos_exp <- datos_exp %>%
  mutate(nota_pref = case_when(
    is.na(nota_pref) & nota_merit == 2 ~ rendimiento ,  # Imputar el valor de 'notas_merit' (2) cuando 'nota_pref' es NA
    TRUE ~ nota_pref  # Mantener los valores existentes de 'nota_pref' cuando no son NA
  ))

# justicia 
datos_exp <- datos_exp %>% mutate(justicia_nota = log(rendimiento/nota_pref))



summary(datos_exp$justicia_nota)

## justicia categorica

# prueba con pregunta que añade tiempo 
datos_exp <- datos_exp %>%
  mutate(
    justicia_nota_tiempo_ch = dplyr::case_when(
      justicia_nota < 0  |nota_merit_esf == 1 ~ 1,
      justicia_nota == 0 | nota_merit_esf == 2 ~ 2, 
      justicia_nota > 0  | nota_merit_esf == 3 ~ 3)) 

datos_exp$justicia_nota_tiempo_ch <- factor(datos_exp$justicia_nota_tiempo_ch, 
                                             levels=c(1,2,3),
                                             labels=c("injusticia sub-recompensa",
                                                      "justicia perfecta",
                                                      "injusticia sobre-recompensa"))


datos_exp <- datos_exp %>%
  mutate(
    justicia_nota_ch = dplyr::case_when(
      justicia_nota < 0  ~ 1,
      justicia_nota == 0 ~ 2, 
      justicia_nota > 0  ~ 3
    )
  ) 

datos_exp$justicia_nota_ch <- factor(datos_exp$justicia_nota_ch, 
                                      levels=c(1,2,3),
                                      labels=c("injusticia sub-recompensa",
                                               "justicia perfecta",
                                               "injusticia sobre-recompensa"))
frq(datos_exp$justicia_nota_ch)  

# EXPERIMENTO -----------------------------------------------------------------

# MERITOCRACIA ----------------------------------------------------------------
datos_exp$social_esfuerzo <- recode(datos_exp$social_esfuerzo, "c(88,99)=NA")
datos_exp$social_talento <- recode(datos_exp$social_talento, "c(88,99)=NA")
datos_exp$social_merito <- recode(datos_exp$social_merito, "c(88,99)=NA")
datos_exp$school_esfuerzo <- recode(datos_exp$school_esfuerzo, "c(88,99)=NA")
datos_exp$school_talento <- recode(datos_exp$school_talento, "c(88,99)=NA")
datos_exp$school_merito <- recode(datos_exp$school_merito, "c(88,99)=NA")
datos_exp$igual_oportunidades <- recode(datos_exp$igual_oportunidades, "c(88,99)=NA")

# 5. base procesada -----------------------------------------------------------
datos_exp <-as.data.frame(datos_exp)
stargazer(datos_exp, type="text")

save(datos_exp,file = "input/data/proc/ola2.RData")
