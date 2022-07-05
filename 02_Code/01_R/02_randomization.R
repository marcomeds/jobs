#' Run the daily randomization for JOBS experiment.
#'
#' @description Run the daily randomization for JOBS experiment.
#'
#' @author Marco Medina
#'

# Load packages
pacman::p_load(here, readr, lubridate, dplyr, tidyr, stringr)

# ---- Randomization ----
#set.seed(5143883)
#set.seed(4426759)
#set.seed(3987658)
#set.seed(4701465)
set.seed(6138353)

jobs_data <- read_csv(here("01_Data", "02_Clean", "jobs_data.csv"))

jobs_daily_randomization <- jobs_data %>%
  filter(fecha_sirede >= Sys.Date() - 3) %>%
  mutate(tratamiento = sample(c("A1", "B2", "C2"), n(), replace = T)) %>%
  mutate(randomization_id = str_c(str_remove_all(Sys.Date(), pattern = "-"), "-", row_number()))

jobs_randomization <- read_csv("01_Data/03_Working/jobs_randomization.csv")

jobs_randomization <- rbind(jobs_randomization, jobs_daily_randomization)

jobs_control <- jobs_randomization %>%
  filter(tratamiento != "A1") %>%
  select(randomization_id, demanda_id, fecha_sirede, expediente_id,
         nombre_completo_actor, telefono_actor, correo_actor,
         nombre_completo_representante, abogado_ingresa, tratamiento, folio_ofipart)

write_csv(jobs_randomization, 
          here("01_Data", "03_Working", "jobs_randomization.csv"))

write_csv(jobs_randomization %>% filter(tratamiento != "A1"), 
          here("01_Data", "03_Working", "jobs_treatment_group.csv"))

write_csv(jobs_control, 
          here("01_Data", "03_Working", "jobs_control.csv"))

  
         