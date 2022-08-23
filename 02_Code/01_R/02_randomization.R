#' Run the daily randomization for JOBS experiment.
#'
#' @description Run the daily randomization for JOBS experiment.
#'
#' @author Marco Medina
#'

# Load packages
pacman::p_load(here, readr, lubridate, dplyr, tidyr, stringr)

# ---- Randomization ----
# Set daily randomization seed
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
#set.seed(2119361)
#set.seed(8074341)
#set.seed(8509443)
#set.seed(7043099)
#set.seed(6872403)
#set.seed(5895691)
#set.seed(1777542)
#set.seed(6440713)
#set.seed(7313277)
#set.seed(5678875)
#set.seed(7701316)
#set.seed(8540014)
#set.seed(4555265)
#set.seed(7365792)
#set.seed(6535592)
#set.seed(1687673)
#set.seed(5044871)
#set.seed(9920712)
#set.seed(8663974)
#set.seed(8129407)
#set.seed(2319022)
#set.seed(1598931)
#set.seed(3914035)
#set.seed(9476111)
#set.seed(4476484)
#set.seed(7833335)
#set.seed(4937908)
#set.seed(3645531)
set.seed(1225492)

# Load jobs_clean.csv
jobs_clean <- read_csv(here("01_Data", "02_Clean", "jobs_clean.csv"))

# Load jobs_randomization.csv
jobs_randomization <- read_csv(here("01_Data", "03_Working", "jobs_randomization.csv"))

# Perform daily randomization
jobs_daily_randomization <- jobs_clean %>%
  # Keep casefiles that haven't been randomized. 
  anti_join(jobs_randomization, by = "folio_ofipart") %>%
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
                   nombre_completo_representante, telefono_representante, correo_representante,
                   nombre_demandado, abogado_ingresa, tratamiento, folio_ofipart),
          append = T,
          here("01_Data", "03_Working", "jobs_control.csv"))

  



  
         