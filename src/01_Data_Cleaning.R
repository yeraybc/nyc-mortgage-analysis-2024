# Script técnico: 01_Data_Cleaning.R
# Objetivo: Extraer, manipular datos y tipificación de variables

# 1. Carga de librerías
library(tidyverse)
library(vroom)  
library(janitor)   
library(skimr)

# 2.Configuración de datos y establecimiento se semilla
set.seed(1234)

data_path <- 'data/raw/hmda_ny_homepurchase.csv'

# Cargamos todo como 'character' inicialmente para evitar truncamiento de datos
raw_data <- vroom(data_path, col_types = cols(.default = 'c'))

# Primera toma contacto
glimpse(raw_data)


# 3. Selección de variables para la limpieza
# Definimos las variables que NO aportan valor predictivo
# Usaremos expresiones regulares para capturar la estructura de todas las variables

# Variables que no aportan variabilidad (Constantes en NY 2024)
cols_constantes <- c('activity_year', 'state_code', 'loan_purpose')

# Variables de 'Data Leakage' (Información post-decisión que sesga el modelo)
cols_fuga <- c(
  'interest_rate', 'rate_spread', 'total_loan_costs', 'total_points_and_fees', 
  'origination_charges', 'discount_points', 'lender_credits', 'loan_term',
  'denial_reason_1', 'denial_reason_2', 'denial_reason_3', 'denial_reason_4',
  'purchaser_type', 'preapproval', 'hoepa_status'
)

# Variables de Redundancia (Multicolinealidad)
cols_proceso_redundancia <- c(
  'applicant_age_above_62', 'co_applicant_age_above_62', # Duplicidad con applicant_age
  'lei', 'census_tract', 'county_code',                   
  'open_end_line_of_credit',
  'reverse_mortgage',   # No aplican a Home Purchase estándar
  'business_or_commercial_purpose' # No aplican a Home Purchase estándar
)

# 4. Pipeline de transformación

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
    -starts_with('aus'),           # Sistemas de scoring internos
    -contains('observed'),         # Método de recolección (evitamos ruido operativo)
    -matches('ethnicity_[1-5]'),   # Usaremos las versiones 'derived'
    -matches('race_[1-5]'),         # Usaremos las versiones 'derived'
    -multifamily_affordable_units #Eliminada por 99% de NAs
  ) %>%
  
  # Solo tendremos en cuenta decisiones finales (1 = Aprobado, 3 = Denegado)
  filter(action_taken %in% c('1', '3')) %>%
  mutate(target = factor(if_else(action_taken == '1', '1', '0'), 
                         levels = c('0', '1'))) %>%
  select(-action_taken) %>%   # Eliminamos la variable original para que el modelo no haga trampa si la mantenemos
  
  # Modificación valores exentos, texto libre, valores erroneos
  mutate(across(
    where(is.character),
    ~ {
      # Limpiamos los espacios ocultos de toda la columna
      valor_limpio <- str_trim(.x)
      # Aplicamos el filtro de seguridad de palabras no admitidas
      case_when(
        valor_limpio %in% c('1111', 'Exempt', 'U', 'Free Form Text Only', 'NA', '8888', '9999') ~ NA_character_,
        TRUE ~ valor_limpio
      )
    }
  )) %>%

  # Conversion a numéricas y winsorización
  mutate(across(
    c(loan_to_value_ratio, property_value, income, loan_amount, 
      starts_with('tract_'), ffiec_msa_md_median_family_income),
    as.numeric
  )) %>%
  
  mutate(
    income = if_else(income <= 0, NA_real_, income),
    
    income              = pmin(income, quantile(income, 0.99, na.rm = TRUE)),
    loan_to_value_ratio = pmin(loan_to_value_ratio, quantile(loan_to_value_ratio, 0.99, na.rm = TRUE)),
    loan_amount         = pmin(loan_amount, quantile(loan_amount, 0.99, na.rm = TRUE)),
    property_value      = pmin(property_value, quantile(property_value, 0.99, na.rm = TRUE))
  ) %>%
  
  # Conversión factores ordinales
  mutate(across(
    c(applicant_age, co_applicant_age),
    ~ factor(.x, levels = niveles_edad, ordered = TRUE)
  )) %>%
  
  mutate(debt_to_income_ratio = factor(
    if_else(is.na(debt_to_income_ratio), 
            'No_Reportado', 
            debt_to_income_ratio),
    
    levels = c(niveles_dti, 'No_Reportado'), ordered = TRUE
  )) %>%
  
  mutate(total_units = factor(total_units, levels = niveles_unidades, ordered = TRUE)) %>%
  
  # Conversión dinámica al resto de categóricas. De esta froma nos aseguramos que el codigo es estable a largo plazo, pero tiene consecuencias
  mutate(across(where(is.character), as.factor))


# 5.Guardado final
saveRDS(datos_final, 'data/processed/hmda_ny_limpio.rds')
skim(datos_final)
