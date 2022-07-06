#' Create files for upload in SIREDE to create PDFs with the calculators and 
#' summons to send via email.
#
#' 
#' @description Creates files to upload in SIREDE.
#'
#' @author Marco Medina
#'

# Load packages
pacman::p_load(here, readr, readxl, lubridate, dplyr, tidyr, stringr)

# ---- Clean and filter the control from drive ----
control_mensajes <- read_excel(here("01_Data", "01_Raw", "Control Cadena de mensajes.xlsx")) %>%
  # Rename variables
  rename(correo_actor_wa = `Correo WA`,
         fecha_cita = `Fecha Cita`,
         hora_cita = `Hora Cita`) %>%
  # Drop empty observations & keep only those observations with scheduled summon
  filter(!is.na(randomization_id) & !is.na(fecha_cita)) %>%
  # Apply appropiate format to fecha_cita & hora_cita
  mutate(fecha_cita = format(fecha_cita, format = "%d/%m/%Y"),
         hora_cita = str_extract(hora_cita, pattern = "[0-9]+:[0-9]{2}")) %>%
  # Keep relevant variables
  select(demanda_id, tratamiento, nombre_completo_actor, correo_actor_wa, fecha_cita, hora_cita)

jobs_correos_envio <- control_mensajes %>% 
  rename(id_demanda = demanda_id) %>% 
  select(id_demanda, correo_actor_wa)

write_csv(jobs_correos_envio,
          here("01_Data", "03_Working", "jobs_correos_envio.csv"))

# ---- Clean demandados.csv ----
sirede_demandados <- read_csv(here("01_Data", "01_Raw", "01_SIREDE", "demandados.csv")) %>%
  right_join(control_mensajes, by = "demanda_id") %>%
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
         colonia, municipio, estado, codigo_postal, tratamiento, nombre_completo_actor, 
         correo_actor_wa, fecha_cita, hora_cita) %>%
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
  ungroup() %>%
  # Rename the variables for actor's name & id to merge with demandas.csv
  rename(nombres_actores = nombre_completo_actor,
         id = demanda_id)

# --- Merge with demandas.csv ---
archivo_citatorios <- read_csv(here("01_Data", "01_Raw", "01_SIREDE", "demandas.csv")) %>%
  # Select variables we need from demandas.csv
  select(id, folio_ofipart, anio_folio, junta, expediente, anio, created_at) %>%
  right_join(sirede_demandados, by = "id") %>%
  # Rename the id for the table
  rename(id_demanda = id) %>%
  # Order variables
  select(id_demanda, folio_ofipart, anio_folio, junta, expediente, anio,
         created_at, tratamiento, consecutivo_citatorio, nombres_actores,
         nombres_demandados, calle, numero_exterior, numero_interior, colonia,
         municipio, estado, codigo_postal, fecha_cita, hora_cita)

archivo_citatorios_acumul <- read_csv("01_Data/04_Campanias/archivo_citatorios.csv")

archivo_citatorios_daily <- archivo_citatorios %>%
  anti_join(archivo_citatorios_acumul, by = "id_demanda")

write_excel_csv(archivo_citatorios,
                here("01_Data", "04_Campanias", "archivo_citatorios.csv"),
                na = "")

write_excel_csv(archivo_citatorios_daily,
                here("01_Data", "04_Campanias", "archivo_citatorios_20220705.csv"),
                na = "")
  
