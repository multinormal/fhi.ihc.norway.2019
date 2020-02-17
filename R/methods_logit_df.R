logit_df_helper <- function(object) {
  object %<>%
    dplyr::mutate(
      # Intercept.
      Intercept                    = `(Intercept)` %>% exp() %>% signif(2),
      Intercept_lower              = `(Intercept)_lower` %>% exp() %>% signif(2),
      Intercept_upper              = `(Intercept)_upper` %>% exp() %>% signif(2),
      # Male.
      Male                         = `SexMale`       %>% exp() %>% signif(2),
      Male_lower                   = `SexMale_lower` %>% exp() %>% signif(2),
      Male_upper                   = `SexMale_upper` %>% exp() %>% signif(2),
      # Research training.
      `Research training`          = `Research_trainingYes` %>% exp() %>% signif(2),
      `Research training_lower`    = `Research_trainingYes_lower` %>% exp() %>% signif(2),
      `Research training_upper`    = `Research_trainingYes_upper` %>% exp() %>% signif(2),
      # Research participant.
      `Research participant`       = `Research_participantYes`       %>% exp() %>% signif(2),
      `Research participant_lower` = `Research_participantYes_lower` %>% exp() %>% signif(2),
      `Research participant_upper` = `Research_participantYes_upper` %>% exp() %>% signif(2),
      # ISCED Levels 3-4.
      `ISCED Levels 3-4`           = `ISCED_LevelLevels 3-4` %>% exp() %>% signif(2),
      `ISCED Levels 3-4_lower`     = `ISCED_LevelLevels 3-4_lower` %>% exp() %>% signif(2),
      `ISCED Levels 3-4_upper`     = `ISCED_LevelLevels 3-4_upper` %>% exp() %>% signif(2),
      # ISCED Levels 5-8.
      `ISCED Levels 5-8`           = `ISCED_LevelLevels 5-8` %>% exp() %>% signif(2),
      `ISCED Levels 5-8_lower`     = `ISCED_LevelLevels 5-8_lower` %>% exp() %>% signif(2),
      `ISCED Levels 5-8_upper`     = `ISCED_LevelLevels 5-8_upper` %>% exp() %>% signif(2),
      # Medical education.
      `Medical education`          = `Medical_educationYes`       %>% exp() %>% signif(2),
      `Medical education_lower`    = `Medical_educationYes_lower` %>% exp() %>% signif(2),
			`Medical education_upper`    = `Medical_educationYes_upper` %>% exp() %>% signif(2))
}

#' Summarize a \code{logit_df} object as a table
#'
#' @param object an object of class \code{logit_df}.
#'
#' @return A \code{tibble} with the summary.
#' @importFrom magrittr "%>%"
#' @importFrom magrittr "%<>%"
#' @export
summary.logit_df <- function(object) {
  object <- logit_df_helper(object)

  # Ensure that all the confidence intervals contain the point estimates.
  names_to_check <- c("Intercept", "Male", "Research training",
                      "Research participant", "ISCED Levels 3-4",
                      "ISCED Levels 5-8", "Medical education")
  for (name in names_to_check) {
    lower_correct <- object[, paste0(name, "_lower")] <= object[, name]
    upper_correct <- object[, name] <= object[, paste0(name, "_upper")]
    both_correct <- lower_correct && upper_correct
    if (!all(both_correct)) {
      print(name)
      print(object[!both_correct, c(paste0(name, "_lower"), name, paste0(name, "_upper"))])
    }
    stopifnot(both_correct)
  }

  # Now make the table for display purposes.
	object %>%
		# Rename the columns for nicer formatting ...
		dplyr::rename(` ` = concept) %>%
		# ... make nicely-formatted confidence intervals ...
		dplyr::mutate(
			Sample = sprintf("%i / %i", number_correct, total),
			`  ` = sprintf("(%s%%)", 100 * raw %>% signif(2)),
			Intercept = sprintf("%s (%s to %s)",
			                    `Intercept`,
			                    `Intercept_lower`,
			                    `Intercept_upper`),
      Male =      sprintf("%s (%s to %s)",
			                    `Male`      ,
			                    `Male_lower`,
			                    `Male_upper`),
      `Research training` = sprintf("%s (%s to %s)",
			                    `Research training`      ,
			                    `Research training_lower`,
			                    `Research training_upper`),
			`Research participant` = sprintf("%s (%s to %s)",
			                    `Research participant`      ,
			                    `Research participant_lower`,
			                    `Research participant_upper`),
			`ISCED Levels 3-4` = sprintf("%s (%s to %s)",
                          `ISCED Levels 3-4`,
                          `ISCED Levels 3-4_lower`,
                          `ISCED Levels 3-4_upper`),
			`ISCED Levels 5-8` = sprintf("%s (%s to %s)",
                          `ISCED Levels 5-8`,
                          `ISCED Levels 5-8_lower`,
                          `ISCED Levels 5-8_upper`),
			`Medical education` = sprintf("%s (%s to %s)",
			                    `Medical education`      ,
			                    `Medical education_lower`,
			                    `Medical education_upper`)) %>%
		dplyr::select(c(` `, Sample, `  `, Intercept, Male, `Research training`,
		                `Research participant`, `Research training`,
		                `ISCED Levels 3-4`, `ISCED Levels 5-8`,
		                `Medical education`))
}

#' \code{plot} method for \code{logit_df} objects.
#'
#' This method makes a forest plot to estimated odds ratios.
#'
#' @param object an object of class \code{logit_df}.
#' @param concept_subset a vector of concept names to include in the plot. By default, all
#'   concepts are included for plots of concepts (as opposed to attitudes and intentions).
#'
#' @return A \code{ggplot2} object representing the figure.
#'
#' @importFrom magrittr "%>%"
#' @importFrom magrittr "%<>%"
#' @export
plot.logit_df <- function(object, simple = FALSE, concept_subset = NULL) {
	attitudes_intentions <- attr(object, "attitudes_intentions")
	stopifnot(!is.null(attitudes_intentions))

	# If a subset of concepts is not specified, use all the concepts.
	if (is.null(concept_subset)) concept_subset <- object$concept
	
  # Specify the breaks and limits to use in the plots.
  breaks <- c(0.1, 1.0, 10, 100)
  break_labels <- c("0.1", "1", "10", "10²")
  limits <- c(0.01, 100)

  # Get the quantities to plot.
	object <- logit_df_helper(object)

  # Define a function to make a forest plot for a particular covariate.
  forest_plot_for <- function(covar, label, include_categories = TRUE) {
    # Modify the CIs, so that if they are outside the limits, they are adjusted
    # to lie within the limits.
    ci_lower <- rlang::sym(paste0(covar, "_lower"))
    ci_upper <- rlang::sym(paste0(covar, "_upper"))
    object %<>% dplyr::mutate(!!ci_lower := dplyr::if_else(!!ci_lower < limits[1],
                                                          limits[1],
                                                          !!ci_lower),
                              !!ci_upper := dplyr::if_else(!!ci_upper > limits[2],
                                                          limits[2],
                                                          !!ci_upper))
    result <- object %>%
      ggplot2::ggplot(ggplot2::aes(x = concept)) +
      ggplot2::geom_point(ggplot2::aes(x = concept, y = !!rlang::sym(covar))) +
      ggplot2::geom_linerange(ggplot2::aes(ymin = !!rlang::sym(paste0(covar, "_lower")), ymax = !!rlang::sym(paste0(covar, "_upper"))),
                                color = "steelblue", alpha = 0.25, size = 4) +
      ggplot2::coord_flip() +
      ggplot2::theme_minimal() +
      ggplot2::theme(axis.text.y = ggplot2::element_text(size = 11, face = "bold"),
                      panel.grid.minor = ggplot2::element_blank()) +
      ggplot2::xlab(NULL) +
      ggplot2::scale_y_continuous(trans = "log10", breaks = breaks, labels = break_labels, limits = c(0.01, 100)) +
      ggplot2::geom_hline(yintercept = 1) +
      ggplot2::ylab(label)
    
    if (!include_categories) {
      result <- result + ggplot2::theme(axis.text.y = ggplot2::element_blank(),
                                        axis.ticks = ggplot2::element_blank())
    }
  
    result
  }

  intercept <- forest_plot_for("Intercept", label = "Intercept")
  male <- forest_plot_for("Male", label = "Male", include_categories = FALSE)
  research_training <- forest_plot_for("Research training", label = "Research\ntraining", include_categories = FALSE)
  research_participant <- forest_plot_for("Research participant", label = "Research\nparticipant", include_categories = FALSE)
  isced_3_4 <- forest_plot_for("ISCED Levels 3-4", label = "ISCED 3-4", include_categories = FALSE)
  isced_5_8 <- forest_plot_for("ISCED Levels 5-8", label = "ISCED 5-8", include_categories = FALSE)
  medical_education <- forest_plot_for("Medical education", label = "Medical\neducation", include_categories = FALSE)
  
  # Join the plots together.
  return(patchwork::wrap_plots(intercept, male,
                               research_training, research_participant,
                               isced_3_4, isced_5_8, medical_education,
                               widths = rep.int(1, 7), ncol = 7))
}
