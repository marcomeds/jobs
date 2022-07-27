#' Clean and filter SIREDE files.
#' 
#' @description Clean and filter the case files in SIREDE for JOBS experiment.
#'
#' @author Marco Medina
#'

# Load packages
pacman::p_load(here, readr, lubridate, dplyr, tidyr, stringr, readxl)


# ----Clean and filter demandas.csv ----
sirede_demandas <- read_csv(here("01_Data", "01_Raw", "01_SIREDE", "demandas.csv")) %>%
  
  # --- Clean ---
  # Rename id as demanda_id
  rename(demanda_id = id) %>%
  # Change the timestamps to Mexico City timezone
  mutate(created_at_mx = with_tz(created_at, "America/Mexico_City")) %>%
  # Get the date of the file creation from the timestamp
  mutate(date_mx = date(created_at_mx)) %>%
  mutate(fecha_sirede = format(date_mx, format = "%d/%m/%Y")) %>%
  # Correct subcourt id's
  mutate(junta = case_when(
    junta == 42 ~ 18,
    junta == 44 ~ 19,
    junta == 45 ~ 20,
    TRUE ~ junta,
  )) %>%
  # Create expediente_id
  mutate(expediente_id = str_c(junta, expediente, anio, sep = "/")) %>%
  # Join with Sebas's SCIAN database to get the giro_empleador
  left_join(read_xlsx(here("01_Data", "01_Raw", "02_CDA", "giro_empleador.xlsx")),
            by = "giro_empleador") %>%
  #mutate(giro_empresa = ifelse(is.na(giro_empresa), 0, giro_empresa)) %>%
  # --- Filter case files ---
  # 1) Keep case files that entered after the experiment started (2022-06-27).
  # 2) Drop case files from subcourts 17, 19 y 20.
  # 3) Keep case files which main action is compensation or reinstatement.
  filter(date_mx >= date("2022-06-27") & date_mx < Sys.Date(),
         !junta %in% c(17, 19, 20),
         accion_principal_segundo_nivel %in% c("INDEMNIZACIÓN CONSTITUCIONAL",
                                               "REINSTALACIÓN"),
         !is.na(folio_ofipart),
         !is.na(junta),
         !is.na(expediente),
         !is.na(anio))  %>%
  # Create dummy for main action, where 1 is reinstatement and 0 is compensation
  mutate(accion_principal = as.numeric(accion_principal_segundo_nivel == "REINSTALACIÓN"))


# Filter using actores.csv
sirede_actores <- read_csv(here("01_Data", "01_Raw", "01_SIREDE", "actores.csv")) %>%
  right_join(sirede_demandas, by = "demanda_id",
             suffix = c("_actores", "_demandas")) %>%
  # --- Filter case files ---
  # 4) Drop case files with more than one plaintiff.
  # 5) Drop case files where the wage periodicity won't allow to calculate the daily wage.
  filter(!frecuencia_cobro %in% c("COMISIÓN",
                                  "OBRA",
                                  "OTRA PERIODICIDAD",
                                  "PRECIO ALZADO")) %>%
  count(demanda_id) %>% 
  filter(n == 1) %>%
  # Keep the case files ids to filter SIREDE_demandas
  select(demanda_id) %>%
  distinct()

# Filter using demandados.csv
sirede_demandados <- read_csv(here("01_Data", "01_Raw", "01_SIREDE", "demandados.csv")) %>%
  right_join(sirede_demandas, by = "demanda_id",
             suffix = c("_demandados", "_demandas")) %>%
  # --- Filter case files ---
  # 6) Drop case files with any defendant outside CDMX.
  # 7) Drop case files that only sue public institutions such as IMSS, INFONAVIT, SAT, CONSAR, STPS.
  # Create a dummy for each defendant that is outside CDMX or a public institution.
  mutate(outside_cdmx = estado_demandados != "CIUDAD DE MÉXICO",
         imss = str_detect(razon_social, "(INSTITUTO MEXICANO DEL SEGURO SOCIAL|IMSS)"),
         infonavit = str_detect(razon_social, "(INSTITUTO (DEL FONDO NACIONAL|NACIONAL DEL FONDO) (DE|PARA) LA VIVIENDA (DE|PARA) (LOS )?TRABAJADORES|INFONAVIT)"),
         consar = str_detect(razon_social, "(COMISION NACIONAL DEL SISTEMA DE AHORRO PARA EL RETIRO|CONSAR)"),
         sat = str_detect(razon_social, "(SERVICIO DE ADMINISTRACION TRIBUTARIA| SAT )"),
         stps = str_detect(razon_social, "(SECRETARIA DE(L)? TRABAJO Y PREVISION SOCIAL|STPS)")) %>%
  mutate(public_inst = imss | infonavit | consar | sat | stps) %>%
  mutate(public_inst = ifelse(is.na(public_inst), 0, public_inst)) %>%
  # Filter at case file level
  group_by(demanda_id) %>%
  mutate(case_outside_cdmx = max(outside_cdmx),
         case_public_inst = min(public_inst)) %>%
  ungroup() %>%
  filter(case_outside_cdmx == 0, 
         case_public_inst == 0) %>%
  # Keep the case files ids to filter SIREDE_demandas
  select(demanda_id) %>%
  distinct()

# Filter demandas.csv using actores.csv and demandados.csv
jobs_demandas <- sirede_demandas %>%
  inner_join(sirede_actores, by = "demanda_id") %>%
  inner_join(sirede_demandados, by = "demanda_id")

# Delete the auxiliary databases
rm(list=setdiff(ls(), "jobs_demandas"))
  


# ---- Clean actores.csv ----
jobs_actores <- read_csv(here("01_Data", "01_Raw", "01_SIREDE", "actores.csv")) %>%
  
  # --- Clean ---
  # Change the timestamps to Mexico City timezone
  mutate(created_at_mx = with_tz(created_at, "America/Mexico_City")) %>%
  # Get the date of the file creation from the timestamp
  mutate(date_mx = date(created_at_mx)) %>%
  # Rename id as actor_id and other variables
  rename(actor_id = id,
         nombre_actor = nombre,
         primer_apellido_actor = primer_apellido,
         segundo_apellido_actor = segundo_apellido,
         horas_sem = horas_semanales) %>%
  # Capitalize name strings
  mutate(across(c(nombre_actor,
                  primer_apellido_actor, 
                  segundo_apellido_actor,
                  categoria_trabajo), 
                toupper)) %>%
  # Create actor's name
  mutate(nombre_completo_actor = str_c(str_replace_na(nombre_actor, ""),
                                       str_replace_na(primer_apellido_actor, ""),
                                       str_replace_na(segundo_apellido_actor, ""),
                                       sep = " ")) %>%
  # Remove extra spaces created if an observation has NA's in name variables.
  mutate(nombre_completo_actor = str_squish(nombre_completo_actor)) %>%
  # Calculate antig_dias and sal_diario
  mutate(antig_dias = case_when(
           !is.na(fecha_termino) ~ as.numeric(fecha_termino - fecha_inicio),
           is.na(fecha_termino) ~ as.numeric(date_mx - fecha_inicio)
         ),
         salario_diario = case_when(
           frecuencia_cobro == "BIMESTRAL" ~ percepcion_neta / 60,
           frecuencia_cobro == "CATORCENAL" ~ percepcion_neta / 14,
           frecuencia_cobro == "DECENAL" ~ percepcion_neta / 10,
           frecuencia_cobro == "DIARIO" ~ percepcion_neta,
           frecuencia_cobro == "MENSUAL" ~ percepcion_neta / 30,
           frecuencia_cobro == "QUINCENAL" ~ percepcion_neta / 15,
           frecuencia_cobro == "SEMANAL" ~ percepcion_neta / 7,
         ),
         # Create tipo_jornada from jornada
         tipo_jornada = case_when(
           jornada == "DIURNA" ~ 1,
           jornada == "NOCTURNA" ~ 2,
           jornada == "MIXTA" ~ 3,
         ),
         # Calculate hextra (extra hours) depending on tipo_jornada
         hextra = case_when(
           tipo_jornada == 1 & horas_sem > 48 ~ horas_sem - 48,
           tipo_jornada == 2 & horas_sem > 42 ~ horas_sem - 42,
           tipo_jornada == 3 & horas_sem > 45 ~ horas_sem - 45,
           TRUE ~ 0
         ),
         # Create dummy for gender
         gen = as.numeric(sexo == "MUJER"),
         # Create dummy for trabajador de confianza (even though the dummy name is trabajador_base)
         trabajador_base = as.numeric(str_detect(categoria_trabajo, pattern = "GERENTE|DIRECTOR|CHOFER PERSONAL|CHOFER PARTICULAR|DOMÉSTIC|DOMESTIC|REPRESENTANTE LEGAL|ABOGADO GENERAL|ADMINISTRADOR GENERAL|CONTADOR GENERAL|DEL HOGAR"))
         ) %>%
  
  # --- Select ---
  select(actor_id, demanda_id, nombre_completo_actor, edad, sexo, gen,
         categoria_trabajo, trabajador_base,
         jornada, tipo_jornada, 
         frecuencia_cobro, horas_sem, hextra, percepcion_neta, salario_diario,
         fecha_inicio, fecha_termino, antig_dias)



# ---- Clean contactos_actores.csv ----
jobs_contactos_actores <- read_csv(here("01_Data", "01_Raw", "01_SIREDE", "contactos_actores.csv")) %>%
  
  # --- Clean ---
  # Drop id to be able to pivot wider
  select(-id, -created_at, -updated_at) %>%
  mutate(tipo = ifelse(tipo == "TELÉFONO CELULAR", "CELULAR", tipo)) %>%
  # Keep the first contact information of each type
  distinct(actor_id, tipo, .keep_all = T) %>%
  # Make the database wider
  pivot_wider(names_from = tipo, values_from = contacto) %>%
  # Create telefono_actor variable. Give preference to cellphones, keep the other phone 
  # number if the cellphone is missing.
  mutate(telefono_actor = ifelse(is.na(CELULAR), TELÉFONO, CELULAR)) %>%
  rename(correo_actor = EMAIL) %>%
  
  # --- Select ---
  select(actor_id, telefono_actor, correo_actor)



# ---- Clean representantes.csv ----
jobs_representantes <- read_csv(here("01_Data", "01_Raw", "01_SIREDE", "representantes.csv")) %>%
  
  # --- Clean ---
  # Rename id
  rename(representante_id = id,
         despacho_representante = despacho) %>%
  # Capitalize name strings
  mutate(across(c(nombre,
                  primer_apellido,
                  segundo_apellido,
                  despacho_representante), 
                toupper)) %>%
  # Create representante's name
  mutate(nombre_completo_representante = str_c(str_replace_na(nombre, ""),
                                               str_replace_na(primer_apellido, ""),
                                               str_replace_na(segundo_apellido, ""),
                                               sep = " ")) %>%
  # Remove extra spaces created if an observation has NA's in name variables.
  mutate(nombre_completo_representante = str_squish(nombre_completo_representante)) %>%
  
  # --- Select ---
  select(representante_id, demanda_id, nombre_completo_representante, despacho_representante)



# ---- Clean contactos_representantes.csv ----
jobs_contactos_representantes <- read_csv(here("01_Data", "01_Raw", "01_SIREDE", "contactos_representantes.csv")) %>%
  
  # --- Clean ---
  # Drop id to be able to pivot wider
  select(-id, -created_at, -updated_at) %>%
  mutate(tipo = ifelse(tipo == "TELÉFONO CELULAR", "CELULAR", tipo)) %>%
  # Keep the first contact information of each type
  distinct(representante_id, tipo, .keep_all = T) %>%
  # Make the database wider
  pivot_wider(names_from = tipo, values_from = contacto) %>%
  # Create telefono_representantes variable. Give preference to cellphones, keep the other phone 
  # number if the cellphone is missing.
  mutate(telefono_representante = ifelse(is.na(CELULAR), TELÉFONO, CELULAR)) %>%
  rename(correo_representante = EMAIL) %>%
  
  # --- Select ---
  select(representante_id, telefono_representante, correo_representante)



# ---- Clean demandados.csv ----
jobs_demandados <- read_csv(here("01_Data", "01_Raw", "01_SIREDE", "demandados.csv")) %>%

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
  select(demanda_id, nombre_completo_demandado) %>%
  # Number each of the defendant names in the case file
  group_by(demanda_id) %>%
  mutate(id = row_number()) %>%
  ungroup() %>%
  # Pivot wider to have one observation per case file address
  pivot_wider(names_from = id, names_prefix = "nombre_demandado_",
              values_from = nombre_completo_demandado, values_fill = "") %>%
  # Create a single string with the name of all the defendants
  unite(nombre_demandado, starts_with("nombre_demandado_"), sep="; ", remove = TRUE) %>%
  # Removes extra ; from the names string
  mutate(nombre_demandado = str_remove(nombre_demandado, pattern = "(; )+$"))



# ---- Merge ----
jobs_clean <- jobs_demandas %>%
  left_join(jobs_actores, by = "demanda_id") %>%
  left_join(jobs_contactos_actores, by = "actor_id") %>%
  left_join(jobs_representantes, by = "demanda_id") %>%
  left_join(jobs_contactos_representantes, by = "representante_id") %>%
  left_join(jobs_demandados, by = "demanda_id") %>%
  select(demanda_id, expediente_id, folio_ofipart, anio_folio, junta, expediente, anio,
         accion_principal, accion_principal_segundo_nivel, giro_empleador, giro_empresa,
         actor_id, nombre_completo_actor, telefono_actor, correo_actor,
         edad, sexo, gen, categoria_trabajo, trabajador_base, jornada, tipo_jornada,
         frecuencia_cobro, horas_sem, hextra, percepcion_neta, salario_diario,
         fecha_inicio, fecha_termino, antig_dias,
         representante_id, nombre_completo_representante, telefono_representante,
         correo_representante, despacho_representante, abogado_ingresa, 
         nombre_demandado, created_at, created_at_mx, date_mx, fecha_sirede)

# ---- Final data ----
write_csv(jobs_clean, here("01_Data", "02_Clean", "jobs_clean.csv"))

# Delete the auxiliary databases
rm(list=setdiff(ls(), "jobs_clean"))

