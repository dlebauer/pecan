##' @title Subset ensemble data for downscaling
##' @name SDA_downscale_preprocess
##' @author Sambhav Dixit, David LeBauer
##'
##' @param ensemble_data EFI standard tibble or data.frame
##' @param site_coords data.frame with unique site id
##' @param date Date. The date for the run, must be a date within `ensemble_data`.
##' @param carbon_pool Character. Carbon pool of interest. Name must match the carbon pool name in ensemble_data. 
##' found within the file or object supplied to 'ensemble_data'.
##' @details This function subsets ensemble data and ensures that the specified date and 
##' carbon pool are present in the ensemble data.
##'
##' @return A list containing the cleaned site coordinates and the ensemble carbon output for the 
##' specified date and carbon pool.
##'
##' @export

subset_ensemble <- function(ensemble_data, site_coords, date, carbon_pool) {

  # Confirm date is in ensemble data
  if (!any(lubridate::date(unique(ensemble_data$datetime)) == lubridate::date(date))) {
    PEcAn.logger::logger.error(paste(
      "Provided date", date,
      "is not found in the ensemble_data input."
    ))
  }

  # Ensure the carbon pool exists in the input data
  if (!carbon_pool %in% unique(ensemble_data$variable)) {
    PEcAn.logger::logger.error("Carbon pool", carbon_pool, "not found in the input data.")
  }

  # Ensure the sites are in the ensemble data
  if (!all(unique(site_coords$site_id) %in% unique(ensemble_data$site_id))) {
    PEcAn.logger::logger.error("Some sites in site_coords are not present in the ensemble_data.")
  }

  # Filter the ensemble data to the specified date and carbon pool
  ensemble_data <- ensemble_data |>
    dplyr::filter(
      lubridate::date(datetime) == lubridate::date(date),
      site_id %in% unique(site_coords$site_id),
      variable == carbon_pool
    ) |>
    dplyr::select(site_id, ensemble, prediction)  # use site_id instead of site

  if (nrow(ensemble_data) == 0) {
    PEcAn.logger::logger.error("No carbon data found for the specified carbon pool.")
  }

  PEcAn.logger::logger.info("Ensemble data subset completed successfully.")
  return(ensemble_data)
}

## Helper function to convert table with lat, lon into an sf object
.convert_coords_to_sf <- function(coords) {
  if (inherits(coords, "sf")) {
    return(coords)
  } else if (is.data.frame(coords)) {
    if (!all(c("lon", "lat") %in% names(coords))) {
      PEcAn.logger::logger.error("Coordinates data frame must contain 'lon' and 'lat'.")
    }
    return(sf::st_as_sf(coords, coords = c("lon", "lat"), crs = 4326))
  } else {
    PEcAn.logger::logger.error("Unsupported coordinates format. Must be an sf object or a data.frame.")
  }
}

## Helper function to convert sf object into table with lat, lon
.convert_sf_to_coords <- function(sf_obj) {
  # Check if it's an sf object
  if (!inherits(sf_obj, "sf")) {
    PEcAn.logger::logger.error("Input must be an 'sf' object.")
  }

  # Extract the geometry into columns named lon/lat
  coord_mat <- sf::st_coordinates(sf_obj)
  colnames(coord_mat) <- c("lon", "lat")

  # Drop the geometry column from the sf, then bind coordinate columns
  out <- sf_obj %>%
    sf::st_drop_geometry() %>%
    tibble::as_tibble() %>%
    dplyr::bind_cols(as.data.frame(coord_mat))
  return(out)
}

##' @noRd
##'
##' @title Create folds function
##' @name .create_folds
##' @author Sambhav Dixit
##'
##' @param y Vector. A vector of outcome data or indices.
##' @param k Numeric. The number of folds to create.
##' @param list Logical. If TRUE, returns a list of fold indices. If FALSE, returns a vector.
##' @param returnTrain Logical. If TRUE, returns indices for training sets. If FALSE, returns indices for test sets.
##' @details This function creates k-fold indices for cross-validation. It can return either training or test set indices, and the output can be in list or vector format.
##'
##' @description This function generates k-fold indices for cross-validation, allowing for flexible output formats.
##'
##' @return A list of k elements (if list = TRUE), each containing indices for a fold, or a vector of indices (if list = FALSE).

.create_folds <- function(y, k, list = TRUE, returnTrain = FALSE) {
  n <- length(y)
  indices <- seq_len(n)
  folds <- split(indices, cut(seq_len(n), breaks = k, labels = FALSE))

  if (!returnTrain) {
    folds <- folds # Test indices are already what we want
  } else {
    folds <- lapply(folds, function(x) indices[-x]) # Return training indices
  }

  if (!list) {
    folds <- unlist(folds)
  }

  return(folds)
}



##' @title Ensemble Downscale
##' @name ensemble_downscale
##' @author Joshua Ploshay, Sambhav Dixit, David LeBauer
##'
##' @param ensemble_data EFI standard tibble or data.frame. Contains carbon data for downscaling.
##' @param site_coords data.frame, tibble, or sf object. Design points. If not sf object, must have
##' 'lon' and 'lat' columns. Must have unique identifier 'site' field. 
##' @param covariates table containing numeric predictors to be used in downscaling. 
##' Must have unique identifier 'site_id' field and predictor attributes
##' @param seed Numeric or NULL. Optional seed for random number generation. Default is NULL.
##' @details This function will downscale forecast data to unmodeled locations using covariates and site locations
##'
##' @return A list containing the model, predictions for all values of covariates as well as test data and test predictions for downstream 
##' statistics.
##' 
##' @export

ensemble_downscale <- function(ensemble_data, site_coords, covariates, seed = NULL) {
  
  ## TODO 
  ## - Accept raster stack as covariates
  ## - Split into separate train and predict functions
  ## - Add CNN functionality, use tidymodels?

  # Dynamically get covariate names
  covariate_names <- colnames(covariates |> dplyr::select(-site_id))

  # scale to N(0,1) (defaults of scale function)
  scaled_covariates <- covariates |>
    dplyr::mutate(dplyr::across(dplyr::all_of(covariate_names), scale))

  # Create a single data frame with all predictors and ensemble data
  design_pt_data <- ensemble_data |>                 # from SIPNET ensemble runs
    dplyr::left_join(scaled_covariates, by = "site_id") # n = nrow(site_coords) * ensemble_size

  # Split the observations into training and testing sets
  if (!is.null(seed)) {
    set.seed(seed) # Only set seed if provided
  }

  ## TODO: Use groups from the 01_cluster_and_select_design_points.R
  sample <- sample(seq_len(nrow(design_pt_data)),
    size = round(0.75 * nrow(design_pt_data))
  )
  train_data <- design_pt_data[sample, ]
  test_data <- design_pt_data[-sample, ]

  ensembles <- unique(ensemble_data$ensemble)
  n_ensembles <- length(ensembles)

  PEcAn.logger::logger.info(
    paste("Start downscaling with", n_ensembles, "ensembles.")
  )

  results <- furrr::future_map(seq_along(ensembles), function(i) {
    formula <- stats::as.formula(
      paste("prediction ~", paste(covariate_names, collapse = " + "))
    )
      
    # Filter train and test for ensemble i
    .train_data <- train_data |>
      dplyr::filter(ensemble == i)
    
    .test_data <- test_data |>
      dplyr::filter(ensemble == i)
    
    PEcAn.logger::logger.info(
      paste(
        "Fitting model for ensemble", i, "of", n_ensembles,
        "with", nrow(.train_data), "training points.\n",
        "and ", nrow(.test_data), "testing points."
      )
    )
    model <- randomForest::randomForest(formula,
      data = .train_data,
      ntree = 1000,
      na.action = stats::na.omit,
      keep.forest = TRUE,
      importance = TRUE,
      seed = seed
    )
    
    prediction <- stats::predict(model, scaled_covariates)  # was map in SDA_downscale
    # can use prediction <- terra::predict(model, scaled_covariates) for raster stack
    test_prediction <- stats::predict(model, .test_data)    # was preds
    
    list(
      model = model,
      prediction = prediction, 
      test_data = .test_data,
      test_prediction = test_prediction
    )
  }, 
  .progress = TRUE,
  .options = furrr::furrr_options(seed = seed)
  )

  # Organize the results into a single output list
  # TODO: need to disambiguate terms design point, sipnet prediction @ design points (which become 'test'
  # vs downscaling prediction
  downscale_output <- 
    list(
      data = list(training = train_data, testing = test_data), # should these be the scaled versions?
      model = purrr::map(results, "model"),
      predictions = purrr::map(results, "prediction"),
      test_data = purrr::map(results, "test_data"),
      test_predictions = purrr::map(results, "test_prediction")
    )

  return(downscale_output)
}

##' @title Calculate Metrics for Downscaling Results
##' @name downscale_metrics
##' @author Sambhav Dixit, David LeBauer
##'
##' @param downscale_output List. Output from the downscale function, containing data, models, maps, predictions, 
##' and test predictions for each ensemble.
##' @param carbon_pool Character. Name of the carbon pool used in the downscaling process.
##'
##' @details This function calculates performance metrics for the downscaling results. It computes Mean Squared Error (MSE), 
##' Mean Absolute Error (MAE), and R-squared for each ensemble. The function uses the actual values from the testing data and 
##' the predictions generated during the downscaling process.
##'
##' @description This function takes the output from the downscale function and computes various performance metrics for each ensemble. 
##' It provides a way to evaluate the accuracy of the downscaling results without modifying the main downscaling function.
##'
##' @return A list of metrics for each ensemble, where each element contains MAE , MSE ,R_squared ,actual values from testing data and predicted values for the testing data
##'
##' @export
downscale_metrics <- function(downscale_output) {
  
  test_data_list <- lapply(downscale_output$test_data, function(x) dplyr::pull(x, prediction))
  predicted_list <- downscale_output$test_prediction

  metric_fn <- function(actual, predicted){ # Could use PEcAn.benchmark pkg?    
    mean <- mean(actual, na.rm = TRUE)
    mse <- mean((actual - predicted)^2, na.rm = TRUE)
    mae <- mean(abs(actual - predicted), na.rm = TRUE)
    r_squared <- 1 - sum((actual - predicted)^2, na.rm = TRUE) /
      sum((actual - mean(actual, na.rm = TRUE))^2)
    # scaled mse
    cv <- 100 * sqrt(mse) / mean(actual, na.rm = TRUE)

    data.frame(
      mean = mean,
      MSE = mse,
      MAE = mae,
      R_squared = r_squared,
      CV = cv
    ) |>
      signif(digits = 2)
  }
  metrics <- purrr::map2(test_data_list, predicted_list, metric_fn) |>
    bind_rows(.id = "ensemble")

  return(metrics)
}
