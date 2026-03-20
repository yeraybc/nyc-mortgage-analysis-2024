# SCRIPT: 01_Data_Cleaning.R
# OBJETIVO: Extraer, manipular datos y tipificación de variables

# 1.CARGA LIBRERIAS
library(tidyverse)
library(vroom)  
library(janitor)   
library(skimr)

# 2.CONFIGURACIÓN DE SEMILLA Y CARGA
set.seed(1234)

data_path <- "data/raw/hmda_ny_homepurchase.csv"

# Cargamos todo como 'character' inicialmente para evitar truncamiento de datos
raw_data <- vroom(data_path, col_types = cols(.default = 'c'))

# Primera toma contacto
glimpse(raw_data)


# 3. SELECCIÓN DE VARIABLES PARA LIMPIEZA 
# Definimos las variables que NO aportan valor predictivo
# Usaremos expresiones regulares para capturar la estructura de todas las variables

# Variables que no aportan variabilidad (Constantes en NY 2024)
cols_constantes <- c("activity_year", "state_code", "loan_purpose")

# Variables de "Data Leakage" (Información post-decisión que sesga el modelo)
cols_fuga <- c(
  "interest_rate", "rate_spread", "total_loan_costs", "total_points_and_fees", 
  "origination_charges", "discount_points", "lender_credits", "loan_term",
  "denial_reason_1", "denial_reason_2", "denial_reason_3", "denial_reason_4",
  "purchaser_type", "preapproval", "hoepa_status"
)

# Variables de Redundancia (Multicolinealidad)
cols_proceso_redundancia <- c(
  "applicant_age_above_62", "co_applicant_age_above_62", # Duplicidad con applicant_age
  "lei", "census_tract", "county_code",                   
  "open_end_line_of_credit",
  "reverse_mortgage",   # No aplican a Home Purchase estándar
  "business_or_commercial_purpose" # No aplican a Home Purchase estándar
)

# 4. PIPELINE DE TRANSFORMACION

# Definimos los niveles para los factores ordinales
niveles_unidades <- c('1','2','3','4','5-24','25-49','50-99','100-149','>149')
niveles_edad     <- c('<25','25-34','35-44','45-54','55-64','65-74','>74')
niveles_dti      <- c('<20%', '20%-<30%', '30%-<36%', '36%', '37%', '38%', 
                      '39%', '40%', '41%', '42%', '43%', '44%', '45%', 
                      '46%', '47%', '48%', '49%', '50%-60%', '>60%')

datos_final <- raw_data %>%
  # Estandarizamos los nombres de las variables
  janitor::clean_names() |> 
  distinct() |> # Eliminamos filas idénticas
  # Seleccionamos las variables
  select(
    -any_of(cols_constantes),
    -any_of(cols_fuga),
    -any_of(cols_proceso_redundancia),
    -starts_with("aus"),           # Sistemas de scoring internos
    -contains("observed"),         # Método de recolección (evitamos ruido operativo)
    -matches("ethnicity_[1-5]"),   # Usaremos las versiones 'derived'
    -matches("race_[1-5]")         # Usaremos las versiones 'derived'
  ) %>%
  
  # Solo tendremos en cuenta decisiones finales (1 = Aprobado, 3 = Denegado)
  filter(action_taken %in% c("1", "3")) %>%
  mutate(target = if_else(action_taken == "1", 1, 0)) %>%
  
  # Limpieza de Numéricos (Manejo de "Exempt" y "NA") ---
  mutate(across(
    c(loan_to_value_ratio, property_value, multifamily_affordable_units, income, loan_amount),
    ~ as.numeric(str_replace_all(str_trim(.x), "^(NA|Exempt|)$", NA_character_))
  )) %>%
  # Limpieza de códigos 8888/9999 en la variable Edad
  mutate(across(
    c(applicant_age, co_applicant_age),
    ~ factor(
      if_else(.x %in% c('8888', '9999', 'NA'), NA_character_, .x),
      levels = niveles_edad, 
      ordered = TRUE
    )
  )) %>%
  
  # DTI pasamos a Factor Ordinal
  mutate(debt_to_income_ratio = factor(
    if_else(debt_to_income_ratio %in% c('NA', 'Exempt'), NA_character_, debt_to_income_ratio),
    levels = niveles_dti, ordered = TRUE
  )) %>%
  
  # Unidades pasamos a Factor Ordinal
  mutate(total_units = factor(total_units, levels = niveles_unidades, ordered = TRUE)) %>%
  
  # Convertimos los Factores Nominales (Categorías)
  mutate(across(
    any_of(c(
      "occupancy_type", "derived_loan_product_type", 
      "derived_ethnicity", "derived_race", "derived_sex", 
      "conforming_loan_limit", "applicant_credit_score_type", 
      "co_applicant_credit_score_type", "submission_of_application", 
      "initially_payable_to_institution"
    )),
    as.factor
  ))


# 5.GUARDADO DE ESTA VERSION FINAL
saveRDS(datos_final, "data/processed/hmda_ny_limpio.rds")
skim(datos_final)
