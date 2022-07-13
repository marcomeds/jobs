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
#set.seed(6138353)
#set.seed(5309079)
#set.seed(6771454)
#set.seed(3418726)
#set.seed(2164043)
#set.seed(9916871)
#set.seed(1787537)
set.seed(2119361)

# Load jobs_clean.csv
jobs_clean <- read_csv(here("01_Data", "02_Clean", "jobs_clean.csv"))

# Load jobs_randomization.csv
jobs_randomization <- read_csv(here("01_Data", "03_Working", "jobs_randomization.csv"))

# Perform daily randomization
jobs_daily_randomization <- jobs_clean %>%
  # Keep casefiles that haven't been randomized. 
  anti_join(jobs_randomization, by = "demanda_id") %>%
  # Select treatment group. For the pilot, we only have control (A1), summons 
  # with encouragement (B2), and summons with encourgament + calculator (C2).
  mutate(tratamiento = sample(c("A1", "B2", "C2"), n(), replace = T)) %>%
  # Create an id to identify the day the casefile was randomized
  mutate(randomization_id = str_c(str_remove_all(Sys.Date(), pattern = "-"), "-", row_number()))

# Append daily randomization to the overall randomization
write_csv(jobs_daily_randomization %>% 
            select(demanda_id, expediente_id, folio_ofipart, tratamiento, randomization_id), 
          append = T,
          here("01_Data", "03_Working", "jobs_randomization.csv"))

# Append daily randomization to the for the WhatsApp control
write_csv(jobs_daily_randomization %>% 
            filter(tratamiento != "A1") %>%
            select(randomization_id, demanda_id, fecha_sirede, expediente_id,
                   nombre_completo_actor, telefono_actor, correo_actor,
                   nombre_completo_representante, nombre_demandado, abogado_ingresa, tratamiento, folio_ofipart),
          append = T,
          here("01_Data", "03_Working", "jobs_control.csv"))

  



  
         