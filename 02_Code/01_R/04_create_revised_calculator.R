#' Create calculator data for jobs experiment.
#
#' 
#' @description Creates calculator data for jobs experiment.
#'
#' @author Marco Medina
#'

# Load packages
pacman::p_load(here, readr, readxl, lubridate, dplyr, tidyr, stringr, randomForest, googlesheets4)

# Get observations from google
revision_calculadoras_presencial <- read_sheet("https://docs.google.com/spreadsheets/d/1mxkWFc4lqDOfl1A_vfO5hSbZ86UBxrE0q1hujRsxhvU/edit#gid=0") %>%
  select(expediente_id, fecha_cita, percepcion_neta, frecuencia_cobro, horas_sem, fecha_inicio,
         fecha_termino, tipo_jornada, gen, accion_principal, categoria_trabajo) %>%
  filter(fecha_cita == "22/08/2022") %>%
  select(-fecha_cita) 

# Load jobs_clean.csv
jobs_clean <- read_csv(here("01_Data", "02_Clean", "jobs_clean.csv")) %>%
  select(-percepcion_neta, -frecuencia_cobro, -horas_sem, -fecha_inicio, -fecha_termino,
         -tipo_jornada, -gen, -accion_principal, -categoria_trabajo) %>%
  right_join(revision_calculadoras_presencial) %>%
  # Calculate antig_dias and sal_diario
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


# Create encoding for prediction
jobs_encoded <- jobs_clean %>%
  
  # --- Dummies encoding (for the calculator) ---
  # Giro empresa
  mutate(dummy = 1) %>%
  pivot_wider(names_from = giro_empresa, names_prefix = "giro_empresa_", values_from = dummy, values_fill = 0) %>%
  select(-giro_empresa_46) %>%
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
    giro_empresa_31 = 0,
    giro_empresa_32 = 0,
    giro_empresa_33 = 0, 
    giro_empresa_48 = 0,
    giro_empresa_49 = 0,
    giro_empresa_51 = 0,
    giro_empresa_52 = 0,
    giro_empresa_55 = 0,
    giro_empresa_61 = 0,
    giro_empresa_62 = 0,
    giro_empresa_64 = 0,
    giro_empresa_71 = 0,
    giro_empresa_81 = 0,
    giro_empresa_93 = 0,
    tipo_jornada_2 = 0,
    tipo_jornada_4 = 0,
    trabajador_base_1 = 0
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



# --- Merge ---
# Delete auxiliary databases
rm(list=setdiff(ls(), list("jobs_clean", "jobs_amount_settlement", "jobs_amount_paid", "jobs_getting_zero")))

# Load randomization
jobs_randomization <- read_csv(here("01_Data", "03_Working", "jobs_randomization.csv"))

# Create jobs_data
jobs_data <- jobs_clean %>%
  left_join(jobs_randomization) %>%
  left_join(jobs_amount_settlement) %>%
  left_join(jobs_amount_paid) %>%
  left_join(jobs_getting_zero)

# Save jobs_data.csv
write_csv(jobs_data, here("01_Data", "03_Working", "jobs_data_revision.csv"))

