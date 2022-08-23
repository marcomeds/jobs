#' Create files for upload in SIREDE to create PDFs with the calculators and 
#' summons to send via email. Version for the new Drive control sheets.
#' 
#' @description Creates files to upload to SIREDE.
#'
#' @author Marco Medina
#'

# Load packages
pacman::p_load(here, readr, readxl, lubridate, dplyr, tidyr, stringr, googlesheets4)

# Today's date for the directory and files sufix
date_suffix <- str_remove_all(Sys.Date(), pattern = "-")

# ---- Clean and filter the control from drive ----
drive_citas <- read_sheet("https://docs.google.com/spreadsheets/d/1B9ITOVeiiZk8JIxU6KMS3s5RwzUMfd5eLkpp38Zs6RI/edit#gid=0") %>%
  
  # Drop empty observations & keep only those observations with scheduled summon
  filter(!is.na(randomization_id) & !is.na(fecha_cita)) %>%
  
  # Keep observations that should have their summons generated
  filter(enviar_citatorio == 1, is.na(correo_enviado)) %>%
  
  # Apply appropiate format to fecha_cita & hora_cita
  mutate(fecha_cita = as.character(format(fecha_cita, format = "%d/%m/%Y")),
         hora_cita = str_extract(hora_cita, pattern = "[0-9]+:[0-9]{2}")) %>%
  
  # Keep relevant variables
  select(randomization_id, tratamiento, nombre_completo_actor, fecha_cita, hora_cita, correo_enviado)



# ---- Merge with jobs_data ----
jobs_citas <- read_csv(here("01_Data", "03_Working", "jobs_data.csv")) %>%
  
  # Select variables we need from jobs_data.csv
  select(randomization_id, demanda_id, folio_ofipart, anio_folio, junta, expediente, anio, created_at) %>%
  
  # Merge with drive_citas
  right_join(drive_citas, by = "randomization_id")



# ---- Clean demandados.csv ----
sirede_demandados <- read_csv(here("01_Data", "01_Raw", "01_SIREDE", "demandados.csv")) %>%
  
  # Filter those case files that have scheduled appointments
  right_join(jobs_citas %>% distinct(demanda_id), by = "demanda_id") %>%
  
  # --- Clean ---
  # Clean names
  rename(nombre_demandado = nombre,
         primer_apellido_demandado = primer_apellido,
         segundo_apellido_demandado = segundo_apellido) %>%
  
  # Capitalize name strings
  mutate(across(c(razon_social,
                  nombre_demandado,
                  primer_apellido_demandado, 
                  segundo_apellido_demandado,
                  calle,
                  numero_exterior,
                  numero_interior,
                  colonia,
                  municipio,
                  estado), 
                toupper)) %>%
  
  # Create demandado's name
  mutate(nombre_completo_demandado = str_c(str_replace_na(nombre_demandado, ""),
                                           str_replace_na(primer_apellido_demandado, ""),
                                           str_replace_na(segundo_apellido_demandado, ""),
                                           sep = " ")) %>%
  
  # Remove extra spaces created if an observation has NA's in name variables.
  mutate(nombre_completo_demandado = str_squish(nombre_completo_demandado)) %>%
  
  # Mutate the name if the defendant is a firm
  mutate(nombre_completo_demandado = ifelse(tipo == "moral", razon_social, nombre_completo_demandado)) %>%
  
  # Order names: first firms, the people, both arranged alphabetically
  group_by(demanda_id) %>%
  arrange(desc(tipo), nombre_completo_demandado, .by_group = T) %>%
  ungroup() %>%
  
  # Keep variables from demandados.csv
  select(demanda_id, nombre_completo_demandado, calle, numero_exterior, numero_interior,
         colonia, municipio, estado, codigo_postal) %>%
  
  # Number each of the defendant names for each address in the case file
  group_by(demanda_id, calle, numero_exterior, numero_interior, colonia, municipio, estado, codigo_postal) %>%
  mutate(id = row_number()) %>%
  ungroup() %>%
  
  # Pivot wider to have one observation per case file address
  pivot_wider(names_from = id, names_prefix = "nombre_demandado_",
              values_from = nombre_completo_demandado, values_fill = "") %>%
  
  # Create a single string with the name of all the defendants
  unite(nombres_demandados, starts_with("nombre_demandado_"), sep="; ", remove = TRUE) %>%
  
  # Removes extra ; from the names string
  mutate(nombres_demandados = str_remove(nombres_demandados, pattern = "(; )+$")) %>%
  
  # Number the citatorios for each case file
  group_by(demanda_id) %>%
  mutate(consecutivo_citatorio = row_number()) %>%
  ungroup()

# --- Merge with sirede_demandados ---
jobs_citatorios <- jobs_citas %>%
  
  # Merge with the info from the defendants
  right_join(sirede_demandados, by = "demanda_id") %>%
  
  # Rename variables for the table
  rename(id_demanda = demanda_id,
         nombres_actores = nombre_completo_actor) %>%
  
  # Order variables
  select(id_demanda, folio_ofipart, anio_folio, junta, expediente, anio,
         created_at, tratamiento, consecutivo_citatorio, nombres_actores,
         nombres_demandados, calle, numero_exterior, numero_interior, colonia,
         municipio, estado, codigo_postal, fecha_cita, hora_cita)



# ---- Create and append to archivo_citatorios ----

# Create today's directory
dir.create(here("01_Data", "04_Campanias", date_suffix))

# Create today's citatorios files
write_csv(jobs_citatorios,
          here("01_Data", "04_Campanias", date_suffix, str_c("archivo_citatorios_", date_suffix, ".csv")),
          na = "")

# Append today's citatorios to archivo_citatorios.csv
write_csv(jobs_citatorios,
          here("01_Data", "04_Campanias", "archivo_citatorios.csv"),
          na = "",
          append = T)



# ---- Prepare revised data for calculatores ----

# Get the revised info from google drive
drive_calculadoras <- read_sheet("https://docs.google.com/spreadsheets/d/1UMEHZ13EzA9rFi3jZiEBVx9q_DKIhaqcpG8R8ssBTZk/edit#gid=0") %>%
  
  # Filter those case files with calculator treatment that have citatorios to be sent today
  right_join(jobs_citatorios %>% filter(tratamiento ==  "C2") %>% distinct(folio_ofipart), by = "folio_ofipart") %>%
  
  # Drop not relevant variables
  select(-REVISOR, -fecha_agendada)



# Get data ready for the calculator
data_calculadoras <- read_csv(here("01_Data", "02_Clean", "jobs_clean.csv")) %>%
  
  # Drop variables that have been manually revised
  select(-percepcion_neta, -frecuencia_cobro, -horas_sem, -fecha_inicio, -fecha_termino,
         -tipo_jornada, -gen, -accion_principal, -categoria_trabajo) %>%
  
  # Merge with drive data 
  right_join(drive_calculadoras) %>%
  
  # Calculate antig_dias, sal_diario, hextra & trabajador base
  mutate(antig_dias = as.numeric(fecha_termino - fecha_inicio),
         
         salario_diario = case_when(
           frecuencia_cobro == "BIMESTRAL" ~ percepcion_neta / 60,
           frecuencia_cobro == "CATORCENAL" ~ percepcion_neta / 14,
           frecuencia_cobro == "DECENAL" ~ percepcion_neta / 10,
           frecuencia_cobro == "DIARIO" ~ percepcion_neta,
           frecuencia_cobro == "MENSUAL" ~ percepcion_neta / 30,
           frecuencia_cobro == "QUINCENAL" ~ percepcion_neta / 15,
           frecuencia_cobro == "SEMANAL" ~ percepcion_neta / 7,
         ),
         
         # Calculate hextra (extra hours) depending on tipo_jornada
         hextra = case_when(
           tipo_jornada == 1 & horas_sem > 48 ~ horas_sem - 48,
           tipo_jornada == 2 & horas_sem > 42 ~ horas_sem - 42,
           tipo_jornada == 3 & horas_sem > 45 ~ horas_sem - 45,
           TRUE ~ 0
         ),
         
         # Create dummy for trabajador de confianza (even though the dummy name is trabajador_base)
         trabajador_base = as.numeric(str_detect(categoria_trabajo, pattern = "GERENTE|DIRECTOR|CHOFER PERSONAL|CHOFER PARTICULAR|DOMÉSTIC|DOMESTIC|REPRESENTANTE LEGAL|ABOGADO GENERAL|ADMINISTRADOR GENERAL|CONTADOR GENERAL|DEL HOGAR"))
  )
  

# ---- Calculator encoding ----

# Create encoding for prediction
jobs_encoded <- data_calculadoras %>%
  
  # --- Dummies encoding (for the calculator) ---
  # Giro empresa
  mutate(dummy = 1) %>%
  pivot_wider(names_from = giro_empresa, names_prefix = "giro_empresa_", values_from = dummy, values_fill = 0) %>%
  #select(-giro_empresa_46) %>%
  # Accion principal
  mutate(dummy = 1) %>%
  pivot_wider(names_from = accion_principal, names_prefix = "accion_principal_", values_from = dummy, values_fill = 0) %>%
  # Gender
  mutate(dummy = 1) %>%
  pivot_wider(names_from = gen, names_prefix = "gen_", values_from = dummy, values_fill = 0) %>%
  # Tipo jornada
  mutate(dummy = 1) %>%
  pivot_wider(names_from = tipo_jornada, names_prefix = "tipo_jornada_", values_from = dummy, values_fill = 0) %>%
  # Trabajador base
  mutate(dummy = 1) %>%
  pivot_wider(names_from = trabajador_base, names_prefix = "trabajador_base_", values_from = dummy, values_fill = 0) %>%
  # Auxiliar dummies
  mutate(giro_empresa_0 = 0,
         giro_empresa_11 = 0,
         giro_empresa_22 = 0,
         giro_empresa_23 = 0,
         giro_empresa_31 = 0,
         giro_empresa_32 = 0,
         giro_empresa_33 = 0, 
         giro_empresa_43 = 0,
         #giro_empresa_48 = 0,
         giro_empresa_49 = 0,
         giro_empresa_51 = 0,
         giro_empresa_52 = 0,
         giro_empresa_53 = 0,
         giro_empresa_54 = 0,
         giro_empresa_55 = 0,
         giro_empresa_61 = 0,
         #giro_empresa_62 = 0,
         giro_empresa_64 = 0,
         giro_empresa_71 = 0,
         giro_empresa_72 = 0,
         giro_empresa_81 = 0,
         giro_empresa_93 = 0,
         tipo_jornada_2 = 0,
         tipo_jornada_3 = 0,
         tipo_jornada_4 = 0,
         trabajador_base_1 = 0,
         gen_1 = 0
  )



# ---- Settlement Amount ----

# Load the regression model for settlement amount
load(here("01_Data",
          "01_Raw",
          "03_Calculadora",
          "amount_settlement_model.RData"))

# Load settlement amount training data to rescale jobs_data
amount_settlement_train <- readRDS(here("01_Data",
                                        "01_Raw",
                                        "03_Calculadora",
                                        "amount_settlement_train.RDS"))

# Standarize data for amount_settlement
# Note: data has to be standarized with the scale the model was trained
jobs_amount_settlement <- jobs_encoded

jobs_amount_settlement$antig_dias <- scale(jobs_amount_settlement$antig_dias,
                                           attr(amount_settlement_train$antig_dias, "scaled:center"),
                                           attr(amount_settlement_train$antig_dias, "scaled:scale"))

jobs_amount_settlement$salario_diario <- scale(jobs_amount_settlement$salario_diario,
                                               attr(amount_settlement_train$salario_diario, "scaled:center"), 
                                               attr(amount_settlement_train$salario_diario, "scaled:scale"))

jobs_amount_settlement$horas_sem <- scale(jobs_amount_settlement$horas_sem,   
                                          attr(amount_settlement_train$horas_sem, "scaled:center"), 
                                          attr(amount_settlement_train$horas_sem, "scaled:scale"))

jobs_amount_settlement$hextra <- scale(jobs_amount_settlement$hextra,     
                                       attr(amount_settlement_train$hextra, "scaled:center"), 
                                       attr(amount_settlement_train$hextra, "scaled:scale"))

# Get prediction
jobs_amount_settlement$pred <- predict(lr_1, newdata = jobs_amount_settlement)

# Unscale
jobs_amount_settlement <- jobs_amount_settlement %>%
  mutate(avg_amount_settlement = pred * attr(amount_settlement_train$liq_total, "scaled:scale") + attr(amount_settlement_train$liq_total, "scaled:center")) %>%
  mutate(avg_amount_settlement = ifelse(avg_amount_settlement < 0, 0, avg_amount_settlement)) %>%
  mutate(avg_amount_settlement = round(avg_amount_settlement)) %>%
  select(demanda_id, expediente_id, folio_ofipart, avg_amount_settlement)




# ---- Paid Amount (Laudo) ----

# Load the random forest model for amount paid
load(here("01_Data",
          "01_Raw",
          "03_Calculadora",
          "amount_paid_model.RData"))

# Load amount paid training data to rescale jobs_data
amount_paid_train <- readRDS(here("01_Data",
                                  "01_Raw",
                                  "03_Calculadora",
                                  "amount_paid_train.RDS"))

# Standarize data for amount_paid
# Note: data has to be standarized with the scale the model was trained
jobs_amount_paid <- jobs_encoded

jobs_amount_paid$antig_dias <- scale(jobs_amount_paid$antig_dias,
                                     attr(amount_paid_train$antig_dias, "scaled:center"),
                                     attr(amount_paid_train$antig_dias, "scaled:scale"))

jobs_amount_paid$salario_diario <- scale(jobs_amount_paid$salario_diario,
                                         attr(amount_paid_train$salario_diario, "scaled:center"), 
                                         attr(amount_paid_train$salario_diario, "scaled:scale"))

jobs_amount_paid$horas_sem <- scale(jobs_amount_paid$horas_sem,   
                                    attr(amount_paid_train$horas_sem, "scaled:center"), 
                                    attr(amount_paid_train$horas_sem, "scaled:scale"))

jobs_amount_paid$hextra <- scale(jobs_amount_paid$hextra,  
                                 attr(amount_paid_train$hextra, "scaled:center"), 
                                 attr(amount_paid_train$hextra, "scaled:scale"))

# Get prediction
jobs_amount_paid$pred <- predict(rf_1, newdata = jobs_amount_paid)

# Unscale
jobs_amount_paid <- jobs_amount_paid %>%
  mutate(avg_amount_payment = pred * attr(amount_paid_train$liq_total, "scaled:scale") + attr(amount_paid_train$liq_total, "scaled:center")) %>%
  mutate(avg_amount_payment = ifelse(avg_amount_payment < 0, 0, avg_amount_payment)) %>%
  mutate(avg_amount_payment = round(avg_amount_payment)) %>%
  select(demanda_id, expediente_id, folio_ofipart, avg_amount_payment)


# ---- Probability of getting zero ----

# Load the random forest for probability of getting zero
load(here("01_Data",
          "01_Raw",
          "03_Calculadora",
          "prob_getting_zero_model.RData"))

# Load probability of getting zero training data to rescale jobs_data
# Note: This model wasn't trained with standarized data
prob_getting_zero_train <- readRDS(here("01_Data",
                                        "01_Raw",
                                        "03_Calculadora",
                                        "prob_getting_zero_train.RDS"))

# Note: This model wasn't trained with standarized data
jobs_getting_zero <- jobs_encoded

# Get prediction
jobs_getting_zero$pred_ <- predict(rf_3, newdata = jobs_getting_zero, type = "prob")
jobs_getting_zero$pred <- jobs_getting_zero$pred_[,"1"]

# Get prob as a number from 0 to 100
jobs_getting_zero <- jobs_getting_zero %>% 
  select(-pred_) %>%
  mutate(prob_getting_zero = round(pred*100)) %>%
  select(demanda_id, expediente_id, folio_ofipart, prob_getting_zero)



# ---- Create features for SIREDE ----

# --- Merge ---
# Delete auxiliary databases
rm(list=setdiff(ls(), list("date_suffix", "drive_citas", "jobs_citatorios", "drive_calculadoras", "data_calculadoras", "jobs_amount_settlement", "jobs_amount_paid", "jobs_getting_zero")))

# Load randomization
jobs_randomization <- read_csv(here("01_Data", "03_Working", "jobs_randomization.csv"))

# Merge and create features
jobs_calculadoras <- data_calculadoras %>%
  left_join(jobs_randomization) %>%
  left_join(jobs_amount_settlement) %>%
  left_join(jobs_amount_paid) %>%
  left_join(jobs_getting_zero) %>%
  
  # Rename variables
  rename(id_demanda = demanda_id,
         id_actor = actor_id,
         genero = sexo,
         antiguedad_dias = antig_dias,
         horas_semanales = horas_sem) %>%
  
  # Create string with the defendants names for the calculator
  mutate(aux_nombre_demandados = str_extract(nombre_demandado, pattern = "^.*?(?=;)")) %>%
  mutate(muchos_demandados = str_detect(nombre_demandado, pattern = ";.*;")) %>%
  mutate(nombre_demandados = case_when(
    muchos_demandados == T ~ str_c(aux_nombre_demandados, " Y OTROS"),
    muchos_demandados == F & !is.na(aux_nombre_demandados) ~ str_c(aux_nombre_demandados, " Y OTRO"),
    T ~ nombre_demandado
  )) %>%
  
  # Create features for the calculator
  mutate(antiguedad_anios = round(antiguedad_dias / 365, 2)) %>%
  
  # Get the minimum wage for the year the worker ended it's work relation
  mutate(anio_termino = case_when(
    !is.na(fecha_termino) ~ year(fecha_termino),
    is.na(fecha_termino) ~ year(date_mx)
  )) %>%
  left_join(read_csv(here("01_Data", "01_Raw", "03_Calculadora", "salario_minimo.csv")), by = "anio_termino") %>%
  
  # Calculate the proportion of this year worked
  mutate(antiguedad_anio_actual = antiguedad_anios - floor(antiguedad_anios),
  
  # Calculate the days of vacation acording to tenure
  dias_vacaciones = case_when(
    antiguedad_anios < 1 ~ 6*antiguedad_anios,
    antiguedad_anios < 5 ~ 6 + (floor(antiguedad_anios) - 1)*2,
    T ~ 12 + floor(antiguedad_anios/5)*2
  )) %>%
  
  # Calculate payments that should be made to the worker
  mutate(c_indemnizacion = round(90*salario_diario, 2),
         c_prima_antig = case_when(
           salario_diario < 2*sal_min ~ round(antiguedad_anios*12*salario_diario, 2),
           T ~ round(antiguedad_anios*12*2*sal_min, 2)
         ),
         c_aguinaldo = case_when(
           antiguedad_anio_actual < antiguedad_anios ~ round(antiguedad_anio_actual*15*salario_diario, 2),
           T ~ round(antiguedad_anios*15*salario_diario, 2)
         ),
         c_vacaciones = round(dias_vacaciones*1.25*salario_diario, 2)) %>%
  
  # Change calculator estimations from mxn to number of wages
  mutate(avg_amount_settlement = round(avg_amount_settlement / salario_diario),
         avg_amount_payment = round(avg_amount_payment / salario_diario)) %>%
  
  # Replace the estimation to 20 days if the estimation is lower than 20.
  mutate(avg_amount_settlement = ifelse(avg_amount_settlement < 20, 20, avg_amount_settlement),
         avg_amount_payment = ifelse(avg_amount_payment < 20, 20, avg_amount_payment)) %>%
  
  # Select variables
  select(id_actor, id_demanda, folio_ofipart, anio_folio, junta, expediente, anio,
         created_at, tratamiento, nombre_completo_actor, genero, tipo_jornada,
         horas_semanales, salario_diario, antiguedad_dias, antiguedad_anios,
         nombre_demandados, accion_principal, giro_empresa, avg_amount_settlement,
         prob_getting_zero, avg_amount_payment, c_indemnizacion, c_prima_antig,
         c_aguinaldo, c_vacaciones)



# ---- Create and append to archivo_calculadoras ----

# Create today's calculadoras files
write_csv(jobs_calculadoras,
          here("01_Data", "04_Campanias", date_suffix, str_c("archivo_calculadoras_", date_suffix, ".csv")),
          na = "")

# Append today's calculadoras to archivo_calculadoras.csv
write_csv(jobs_calculadoras,
          here("01_Data", "04_Campanias", "archivo_calculadoras.csv"),
          na = "",
          append = T)

