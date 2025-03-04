simulaBifactor <- function(sampleSize = 1000, 
                           nFactors = 4, 
                           itemsPerFactor = 6,
                           loadingGeneral = 0.8, 
                           loadingSpecific = 0.3, 
                           fluctuation = 0.05,
                           type_problematic = "perfect",  # Opciones: "perfect", "1f", "2f", "5i", "3i", etc.
                           mod_gen_factor = 0.5,          # Multiplicador para loadingGeneral en ítems problemáticos
                           mod_spec_factor = 1.5,         # Multiplicador para loadingSpecific en ítems problemáticos
                           problematic_prob = 0.75,       # Probabilidad de que los ítems problemáticos provengan de un mismo factor (solo para tipo "i")
                           estimator = "ML",              # Argumento para lavaan
                           seed = 12345) {
  
  set.seed(seed)
  
  totalItems <- nFactors * itemsPerFactor
  
  # Simulamos el factor general (FG) ~ N(0,1)
  FG <- rnorm(sampleSize, mean = 0, sd = 1)
  
  # Simulamos los factores específicos: cada uno ~ N(0,1)
  specs <- matrix(rnorm(sampleSize * nFactors, mean = 0, sd = 1), 
                  nrow = sampleSize, ncol = nFactors)
  
  # Determinar qué ítems o factores serán problemáticos
  problematic_factors <- NULL
  problematic_indices <- NULL
  
  if(type_problematic != "perfect") {
    # Extraer el último caracter y el número
    last_char <- substr(type_problematic, nchar(type_problematic), nchar(type_problematic))
    num_value <- as.numeric(substr(type_problematic, 1, nchar(type_problematic)-1))
    
    if(last_char == "f") {
      # Se interpretan como factores problemáticos: 
      # Se seleccionan min(num_value, nFactors) factores de entre los disponibles.
      problematic_factors <- sort(sample(1:nFactors, size = min(num_value, nFactors), replace = FALSE))
    } else if(last_char == "i") {
      # Se interpretan como número de ítems problemáticos
      problematic_n <- min(num_value, totalItems)
      if(runif(1) < problematic_prob) {
        # Mayor probabilidad de que provengan de un mismo factor.
        prob_factor <- sample(1:nFactors, 1)
        indices_in_factor <- ((prob_factor - 1) * itemsPerFactor + 1):(prob_factor * itemsPerFactor)
        problematic_indices <- sample(indices_in_factor, size = min(problematic_n, length(indices_in_factor)), replace = FALSE)
      } else {
        problematic_indices <- sample(1:totalItems, size = problematic_n, replace = FALSE)
      }
    }
  }
  
  # Inicializamos matriz para datos y vectores para almacenar las cargas usadas
  data_mat <- matrix(NA, nrow = sampleSize, ncol = totalItems)
  loadings_gen <- numeric(totalItems)
  loadings_spec <- numeric(totalItems)
  item_names <- c()
  
  counter <- 1
  for (g in 1:nFactors) {
    for (i in 1:itemsPerFactor) {
      # Fluctuación aleatoria para las cargas base
      fluct_gen  <- runif(1, -fluctuation, fluctuation)
      fluct_spec <- runif(1, -fluctuation, fluctuation)
      
      # Valores base para este ítem:
      current_loading_gen  <- loadingGeneral + fluct_gen
      current_loading_spec <- loadingSpecific + fluct_spec
      
      # Si se especificó un tipo problemático de factores ("f"), 
      # y el factor actual g está en la lista problematic_factors, se modifican todas las cargas.
      if(!is.null(problematic_factors) && (g %in% problematic_factors)) {
        current_loading_gen  <- loadingGeneral * mod_gen_factor + runif(1, -fluctuation, fluctuation)
        current_loading_spec <- loadingSpecific * mod_spec_factor + runif(1, -fluctuation, fluctuation)
      }
      
      # Si se especificó un tipo problemático de ítems ("i"),
      # y el contador (ítem) está en problematic_indices, se modifican las cargas.
      if(!is.null(problematic_indices) && (counter %in% problematic_indices)) {
        current_loading_gen  <- loadingGeneral * mod_gen_factor + runif(1, -fluctuation, fluctuation)
        current_loading_spec <- loadingSpecific * mod_spec_factor + runif(1, -fluctuation, fluctuation)
      }
      
      loadings_gen[counter] <- current_loading_gen
      loadings_spec[counter] <- current_loading_spec
      
      # Calcular la varianza del error para que la varianza total del ítem sea 1
      err_var <- 1 - (current_loading_gen^2 + current_loading_spec^2)
      if (err_var < 0) err_var <- 0.001
      
      error_term <- rnorm(sampleSize, mean = 0, sd = sqrt(err_var))
      
      # Formar el ítem con la suma ponderada de la contribución del factor general y el específico
      data_mat[, counter] <- current_loading_gen * FG + 
        current_loading_spec * specs[, g] + 
        error_term
      
      # Nombrar el ítem
      item_names <- c(item_names, paste0("item_", g, "_", i))
      counter <- counter + 1
    }
  }
  
  sim_data <- as.data.frame(data_mat)
  colnames(sim_data) <- item_names
  
  ###############################
  # Especificación del modelo bifactor en lavaan
  ###############################
  bifactor_model <- "F =~ "
  bifactor_model <- paste0(bifactor_model, paste(item_names, collapse = " + "), "\n")
  
  for (g in 1:nFactors) {
    items_g <- item_names[ grep(paste0("^item_", g, "_"), item_names) ]
    line <- paste0("S", g, " =~ ", paste(items_g, collapse = " + "), "\n")
    bifactor_model <- paste0(bifactor_model, line)
  }
  
  # Restricciones de ortogonalidad
  for (g in 1:nFactors) {
    bifactor_model <- paste0(bifactor_model, "F ~~ 0*S", g, "\n")
  }
  if(nFactors > 1) {
    for (g1 in 1:(nFactors-1)) {
      for (g2 in (g1+1):nFactors) {
        bifactor_model <- paste0(bifactor_model, "S", g1, " ~~ 0*S", g2, "\n")
      }
    }
  }
  
  ###############################
  # Especificación del modelo multifactorial (correlacionado)
  ###############################
  multifactor_model <- ""
  for (g in 1:nFactors) {
    items_g <- item_names[ grep(paste0("^item_", g, "_"), item_names) ]
    line <- paste0("F", g, " =~ ", paste(items_g, collapse = " + "), "\n")
    multifactor_model <- paste0(multifactor_model, line)
  }
  
  # Ajuste de los modelos con lavaan, pasando el argumento estimator y std.lv = TRUE

  fit_bifactor <- cfa(bifactor_model, data = sim_data, 
                      estimator = estimator,
                      std.lv = TRUE)
  fit_multi <- cfa(multifactor_model, data = sim_data, 
                   estimator = estimator,
                   std.lv = TRUE)
  
  return(list(sim_data = sim_data,
              loadings_gen = loadings_gen,
              loadings_spec = loadings_spec,
              bifactor_model = bifactor_model,
              multifactor_model = multifactor_model,
              fit_bifactor = fit_bifactor,
              fit_multi = fit_multi))
}
# 
# # Ejemplos de uso:
# 
# # 1. Escenario ideal ("perfect")
# result_ideal <- simulaBifactor(sampleSize = 1000, 
#                                nFactors = 4, 
#                                itemsPerFactor = 6,
#                                loadingGeneral = 0.8, 
#                                loadingSpecific = 0.3, 
#                                fluctuation = 0.05,
#                                type_problematic = "perfect",
#                                estimator = "ML")
# 
# # 2. Escenario problemático: "1f" (todos los ítems del primer factor se modifican)
# result_1f <- simulaBifactor(sampleSize = 1000, 
#                             nFactors = 4, 
#                             itemsPerFactor = 6,
#                             loadingGeneral = 0.8, 
#                             loadingSpecific = 0.3, 
#                             fluctuation = 0.05,
#                             type_problematic = "1f",
#                             mod_gen_factor = 0.3, 
#                             mod_spec_factor = 1.8,
#                             estimator = "MLR")
# 
# # 3. Escenario problemático: "3i" (3 ítems serán modificados, con mayor probabilidad de provenir de un mismo factor)
# result_3i <- simulaBifactor(sampleSize = 1000, 
#                             nFactors = 4, 
#                             itemsPerFactor = 6,
#                             loadingGeneral = 0.8, 
#                             loadingSpecific = 0.3, 
#                             fluctuation = 0.05,
#                             type_problematic = "3i",
#                             mod_gen_factor = 0.5, 
#                             mod_spec_factor = 1.5,
#                             problematic_prob = 0.75,
#                             estimator = "ML")
# 
# # Ejemplo: Resumen del modelo bifactor para el escenario "1f"
# summary(result_1f$fit_bifactor, fit.measures = TRUE, standardized = TRUE)
# 
# # Ejemplo: Resumen del modelo multifactorial para el escenario "1f"
# summary(result_1f$fit_multi, fit.measures = TRUE, standardized = TRUE)
