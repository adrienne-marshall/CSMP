#' Assess climate-sensitivity of model performance
#'
#' @param airtemp Numeric vector of observed annual average air temperatures (°C)
#' @param precip Numeric vector of observed annual average precipitation values (mm)
#' @param objective Numeric vector of model objective function results (e.g., RMSE, NSE)
#' @param fixed_var If desired, a variable to be used for a fixed effect. Should be a vector of observations of this effect that correspond to the positions of other vectors given to the function. This vector can be of class numeric, factor, or character.
#' @param fixed_return If a fixed effect variable is provided, this should be a numeric value of the fixed effect variable that should be used to return answers. Must be a value from within the fixed effect variable vector. If not provided, will take first value in fixed_var
#' @param ref_airtemp The reference value of air temperature (°C) in a historical average year. If not provided, will be set to mean of observed vector.
#' @param ref_precip The reference value of precipitation (mm) in a historical average year. If not provided, will be set to mean of observed vector.
#' @param new_airtemp The air temperature values (°C) to be used for comparison. If not supplied, will be set to the 5th and 95th percentiles of the range of observations. This should be a vector of length one (representing unidirectional climate change) or two (representing potential change in two directions - e.g., wetting or drying).
#' @param new_precip The precipitation values (mm) for comparison. See notes for new_airtemp.
#' @param return_model Should the function return the model object? This is useful for examining residuals, but could lead to large outputs.
#' @param return_plot Should the function return a contour plot showing the results? The plot returned is a ggplot() object and can be modified with additional calls to ggplot2 (see package readme).
#' @param return_plot_df Should the function return a data frame that can be used to make a contour plot?
#' @param return_pred Should the function return predicted error for the supplied values of airtemp and precip?
#' @param airtemp_limits Limits of range on which to predict air temp if returning plot_df
#' @param precip_limits Limits of range on which to predict precip if returning plot_df
#' @param k_val Number of knots to use in gam - equivalent to k in gam(). Defaults to 3.
#'
#' @return A list that may contain the following items, depending on input parameters:
#' \itemize{
#'  \item {stats: }{A one-row data frame with CSMP statistics}
#'  \item {model_object: }{The fitted GAM}
#'  \item {contour_plot: }{A ggplot object showing the shape of the fitted GAM}
#'  \item {predict_df: }{data frame showing predicted values of the objective function for ref_airtemp, ref_precip, new_airtemp, and new_precip}
#'  \item {plot_df: }{The data frame used to create contour_plot (same information as predict_df, but predicted over more points for plotting)}
#'  }
#' @export
#'
#' @author Adrienne M. Marshall
#'
#' @examples
#' airtemp <- rnorm(20, mean = 0, sd = 1)
#' precip <- rnorm(20, mean = 500, sd = 100)
#' objective <- rbeta(20, 1, 1)
#'
#' ans <- csmp(airtemp, precip, objective, return_plot = TRUE)
#' ans$stats
#' ans$contour_plot
#'
#'
csmp <- function(airtemp,
                 precip,
                 objective,
                 fixed_var = NULL,
                 fixed_return = NULL,
                 ref_airtemp = NULL,
                 ref_precip = NULL,
                 new_airtemp = NULL,
                 new_precip = NULL,
                 return_model = F,
                 return_plot_df = F,
                 return_plot = F,
                 return_pred = F,
                 airtemp_limits = c(-3, 1),
                 precip_limits = c(250, 580),
                 k_val = 3){

  # Check inputs:
  if(!is.numeric(airtemp)){stop("airtemp vector must be numeric.")}
  if(!is.numeric(precip)){stop("precip vector must be numeric.")}
  if(!is.numeric(objective)){stop("objective vector must be numeric.")}

  if(length(airtemp) != length(precip) | length(airtemp) != length(objective)){
    stop("airtemp, precip, and objective vectors must all be the same length.")
  }
  if(!is.null(fixed_var) & length(fixed_var) != length(objective)){
    stop("If supplied, fixed_var must be same length as objective (as well as air temperature and precipitation).")
  }



  # Set up a data frame for fitting.
  df <- data.frame(airtemp = airtemp,
                   precip = precip,
                   objective = objective)

  # If there's no fixed effect variable:
  if(is.null(fixed_var)){
    # Fit a gam
    fit <- mgcv::gam(objective ~ s(airtemp, bs = "cr", k = k_val) +
                 s(precip, bs = "cr", k = k_val),
               data = df,
               family = mgcv::betar())
  } else { # if a fixed effect variable is provided ...
    df$fixed_var <- fixed_var

    # Fit a gam with fixed effect variable.
    fit <- mgcv::gam(objective ~ s(airtemp, bs = "cr", k = k_val) +
                 s(precip, bs = "cr", k = k_val) +
                 s(fixed_var, bs = "re"),
               data = df,
               family = mgcv::betar())

    # Assign fixed effect variable for return if not specified.
    if(is.null(fixed_return)){fixed_return <- fixed_var[1]}
    } # ends check to see if fixed effect variable is provided.

  # Set reference and new values if not provided.
  if(is.null(ref_airtemp)){ref_airtemp <- mean(airtemp)}
  if(is.null(ref_precip)){ref_precip <- mean(precip)}
  if(is.null(new_airtemp)){new_airtemp <- stats::quantile(airtemp, c(0.05, 0.95))}
  if(is.null(new_precip)){new_precip <- stats::quantile(precip, c(0.05, 0.95))}

  # Set up a data frame on which to predict results.
  if(!is.null(fixed_var)){
    predict_df <- expand.grid(airtemp = c(ref_airtemp, new_airtemp),
                              precip = c(ref_precip, new_precip),
                              fixed_var = unique(fixed_var))
  } else {
    predict_df <- expand.grid(airtemp = c(ref_airtemp, new_airtemp),
                              precip = c(ref_precip, new_precip))
  }

  predict_df <- predict_df %>%
    dplyr::mutate(predicted_objective = mgcv::predict.gam(fit, newdata = predict_df, type = "response"))

  # Make a version of predict_df with the fixed_var only.
  if(!is.null(fixed_var)){
    predict_df2 <- predict_df %>% dplyr::filter(fixed_var == fixed_return)
  } else {predict_df2 <- predict_df}

  # Get reference predicted objective.
  ref_obj <- predict_df2 %>%
    dplyr::filter(airtemp == ref_airtemp,
           precip == ref_precip) %>%
    dplyr::pull(predicted_objective)

  # Extract climate-sensitive model parameters
  # CSMP cool:
  if(min(new_airtemp) < ref_airtemp){
    csmp_cool <- predict_df2 %>%
      dplyr::filter(airtemp == min(new_airtemp),
             precip == ref_precip) %>%
      dplyr::pull(predicted_objective) - ref_obj
    } else {csmp_cool <- NA}

  # CSMP warm:
  if(max(new_airtemp) > ref_airtemp){
    csmp_warm <- predict_df2 %>%
      dplyr::filter(airtemp == max(new_airtemp),
             precip == ref_precip) %>%
      dplyr::pull(predicted_objective) - ref_obj
  } else {csmp_warm <- NA}

  # CSMP dry:
  if(min(new_precip) < ref_precip){
    csmp_dry <- predict_df2 %>%
      dplyr::filter(airtemp == ref_airtemp,
             precip == min(new_precip)) %>%
      dplyr::pull(predicted_objective) - ref_obj
  } else {csmp_dry <- NA}

  # CSMP wet:
  if(max(new_precip) > ref_precip){
    csmp_wet <- predict_df2 %>%
      dplyr::filter(airtemp == ref_airtemp,
             precip == max(new_precip)) %>%
      dplyr::pull(predicted_objective) - ref_obj
  } else {csmp_wet <- NA}

  # Extract summary parameters.
  x <- summary(fit)
  stats <- data.frame(rsquared = x$r.sq,
                      p_airtemp = x$s.table[1,4],
                      p_precip = x$s.table[2,4],
                      csmp_warm = csmp_warm,
                      csmp_cool = csmp_cool,
                      csmp_dry = csmp_dry,
                      csmp_wet = csmp_wet)

  # Data frame to plot contours:
  if(return_plot_df == TRUE | return_plot == TRUE){
    if(!is.null(fixed_var)){
      plot_df <- expand.grid(airtemp = seq(min(airtemp_limits), max(airtemp_limits), length.out = 20),
                             precip = seq(min(precip_limits), max(precip_limits), length.out = 20),
                             fixed_var = unique(fixed_var))
    } else {
      plot_df <- expand.grid(airtemp = seq(min(airtemp_limits), max(airtemp_limits), length.out = 20),
                             precip = seq(min(precip_limits), max(precip_limits), length.out = 20))
    }

    plot_df <- plot_df %>%
      dplyr::mutate(predicted_objective = mgcv::predict.gam(fit, newdata = plot_df, type = "response"))

  } else {plot_df <- NULL}

  # Figure
  if(return_plot == TRUE){
    p <- ggplot2::ggplot(plot_df, ggplot2::aes(x = airtemp, y = precip, z = predicted_objective)) +
      ggplot2::geom_contour_filled() +
      ggplot2::theme_minimal()
  } else {p <- NULL}

  # Only return model object if return_model = T. Could be large with lots of runs.
  if(return_model == F){fit <- NULL}

  # Only return predictions if return_pred = T.
  if(return_pred == F){predict_df <- NULL}

  # Return: summary parameters, the gam model object, and a plot of the results.
  ans <- list(stats = stats,
              model_object = fit,
              contour_plot = p,
              predict_df = predict_df,
              plot_df = plot_df)

  return(ans)
}

