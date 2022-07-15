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
control_mensajes <- read_excel(here("01_Data", "01_Raw", "02_CDA", "Control PILOTO.xlsx")) %>%
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
  ungroup()

# --- Merge with jobs_data.csv ---
archivo_citatorios <- read_csv(here("01_Data", "03_Working", "jobs_data.csv")) %>%
  # Select variables we need from jobs_data.csv
  select(demanda_id, folio_ofipart, anio_folio, junta, expediente, anio, created_at) %>%
  right_join(sirede_demandados, by = "demanda_id") %>%
  # Rename the id for the table
  rename(id_demanda = demanda_id,
         nombres_actores = nombre_completo_actor) %>%
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

dir.create(here("01_Data", "04_Campanias", "20220714"))

write_excel_csv(archivo_citatorios_daily,
                here("01_Data", "04_Campanias", "20220714", "archivo_citatorios_20220714.csv"),
                na = "")
  


# ---- Archivo Calculadoras ----
archivo_calculadoras <- read_csv(here("01_Data", "03_Working", "jobs_data.csv")) %>%
  right_join(control_mensajes) %>%
  # Filter treatment arm with calculator
  filter(tratamiento == "C2") %>%
  # Rename variables
  rename(id_demanda = demanda_id,
         id_actor = actor_id,
         genero = sexo,
         antiguedad_dias = antig_dias,
         horas_semanales = horas_sem) %>%
  # Create string for the calculator
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
  mutate(antiguedad_anio_actual = case_when(
    !is.na(fecha_termino) ~ round(as.numeric(fecha_termino - as_date("2022-01-01")) / 365, 2),
    is.na(fecha_termino) ~ round(as.numeric(date_mx - date("2022-01-01")) / 365, 2)
  ),
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
  
archivo_calculadoras_acumul <- read_csv("01_Data/04_Campanias/archivo_calculadoras.csv")

archivo_calculadoras_daily <- archivo_calculadoras %>%
  anti_join(archivo_calculadoras_acumul, by = "id_demanda")

write_excel_csv(archivo_calculadoras,
                here("01_Data", "04_Campanias", "archivo_calculadoras.csv"),
                na = "")

write_excel_csv(archivo_calculadoras_daily,
                here("01_Data", "04_Campanias", "20220714", "archivo_calculadoras_20220714.csv"),
                na = "")
  
  
