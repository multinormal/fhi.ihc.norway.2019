#' Summarize a \code{prop_df} object as a table
#'
#' @param object an object of class \code{prop_df}.
#'
#' @return A \code{tibble} with the table of participant characteristics.
#' @importFrom magrittr "%>%"
#' @export
summary.prop_df <- function(object) {
	object %>%	
		# Arrange the plot with best-understood concepts at the top and least at the bottom.
		dplyr::arrange(-proportion) %>%
		# ... rename the columns for nicer formatting ...
		dplyr::rename(` ` = concept) %>%
		# ... make nicely-formatted confidence intervals ...
		dplyr::mutate(
			Sample = sprintf("%i / %i", number_correct, total),
			`  ` = sprintf("(%s%%)", 100 * raw %>% signif(2)),
			`Post-stratified Estimate (95% CI)` = sprintf("%s%% (%s%% to %s%%)",
																							 100 * proportion %>% signif(2),
																							 100 * ci_lower %>% signif(2),
																							 100 * ci_upper %>% signif(2))) %>%
		# ... deal with NA estimates where there are problems estimating ...
		{
			.$`Post-stratified Estimate (95% CI)`[is.na(.$proportion)] <- ""
			.
		} %>%
		# ... and return the columns of interest in their correct order.
		dplyr::select(c(` `, Sample, `  `, `Post-stratified Estimate (95% CI)`))
}

#' \code{plot} method for \code{prop_df} objects.
#'
#' This method makes a forest plot to display raw and estimated proportions. The reference
#' line shows what we would expect if participants perform no better than random on a
#' key concept with two questions with 3 and 2 options, respectively.
#'
#' @param object an object of class \code{prop_df}.
#' @param simple if \code{TRUE}, make a simpler plot more suitable for presentations.
#' @param concept_subset a vector of concept names to include in the plot. By default, all
#'   concepts are included for plots of concepts (as opposed to attitudes and intentions).
#'
#' @return A \code{ggplot2} object representing the figure.
#'
#' @importFrom magrittr "%>%"
#' @importFrom magrittr "%<>%"
#' @export
plot.prop_df <- function(object, simple = FALSE, concept_subset = NULL) {
	attitudes_intentions <- attr(object, "attitudes_intentions")
	stopifnot(!is.null(attitudes_intentions))

	# If a subset of concepts is not specified, use all the concepts.
	if (is.null(concept_subset)) concept_subset <- object$concept

  # Add markers to indicate questions supported by only one question.
  dagger <- "\U2020"
  object$concept[object$concept == concepts$more_not_better] <- paste0(concepts$more_not_better, dagger)

  for (i in 1:nrow(object)) {
    label_length <- nchar(object$concept[i])
    object$concept[i] <- stringr::str_wrap(object$concept[i], width = 5 + round(label_length / 2))
  }

	# Arrange the plot with best-understood concepts at the top and least at the bottom.
	object$concept <- factor(object$concept, levels = object$concept[order(object$proportion)])

	# Make the forest plot.
  forest_plot <- object %>%
  	ggplot2::ggplot(ggplot2::aes(x = concept)) +
    ggplot2::geom_point(ggplot2::aes(x = concept, y = proportion)) +
    ggplot2::geom_linerange(ggplot2::aes(ymin = ci_lower, ymax = ci_upper),
    													color = "steelblue", alpha = 0.25, size = 4) +
    ggplot2::coord_flip() +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.y = ggplot2::element_blank()) +
    ggplot2::xlab(NULL) +
    ggplot2::ylab(NULL) +
  	ggplot2::scale_y_continuous(labels = scales::percent, breaks = seq(0, 1, 0.2), limits = c(0, 1))

  # Customize the plot for the two different types of data we present.
  if (attitudes_intentions) {
  		forest_plot <- forest_plot + ggplot2::labs(subtitle = "Norwegians answering in the affirmative")
  } else {
  		forest_plot <- forest_plot +
  			ggplot2::geom_errorbar(ggplot2::aes(x = concept,
																				 ymin = expected_probability,
                                         ymax = expected_probability),
                            data = object,
                            color = "black", width = 0.5) +
  			ggplot2::labs(subtitle = "Estimated percent of Norwegian adults\nwho understand each Key Concept*")
  }

	# Define a function to make a "table" plot.
  table_plot <- function(label_f, subtitle, show_concepts = FALSE) {
    if (show_concepts) {
      axis_text_y <- ggplot2::element_text(size = 9)
    } else {
      axis_text_y <- ggplot2::element_blank()
    }

    object %>%
      label_f() %>%
      ggplot2::ggplot(ggplot2::aes(x = concept)) +
      ggplot2::coord_flip() +
      ggplot2::geom_text(ggplot2::aes(x = concept, y = 0, label = label), hjust = "inward", size = (5/15) * 9, lineheight = 1) +
      ggplot2::scale_y_continuous(limits = c(0, 1), expand = ggplot2::expand_scale(mult = c(0, 0))) +
      ggplot2::theme_minimal() +
      ggplot2::xlab(NULL) +
      ggplot2::ylab(NULL) +
      ggplot2::labs(subtitle = subtitle) +
      ggplot2::theme(# Blank out the grid and axis tick labels.
                     axis.text.x = ggplot2::element_blank(),
                     axis.text.y = axis_text_y,
                     panel.grid.major = ggplot2::element_blank(),
                     panel.grid.minor = ggplot2::element_blank(),
                   	 plot.subtitle = ggplot2::element_text(hjust = 0))
  }

  # Define labelling functions to create text labels for means and CIs.
  counts_label_f <- function(x) {
		dplyr::mutate(x, label = sprintf("%i / %i\n(%s%%)", number_correct, total, 100 * raw %>% signif(2)))
  }
  proportion_label_f <- function(x) {
		dplyr::mutate(x, label = sprintf("%s%%\n(%s%% to %s%%)", 100 * proportion %>% signif(2),
                                                             100 * ci_lower %>% signif(2),
                                                             100 * ci_upper %>% signif(2)))
  }

  # Make the "table" plots.
  counts_table <- table_plot(label_f = counts_label_f, subtitle = "N\n(%)", show_concepts = TRUE)
  proportions_table <- table_plot(label_f = proportion_label_f, subtitle = "Post-stratified estimate\n(95% CI)")

  # Combine the plots and return the result.
  result <- if (simple) {
  	patchwork::wrap_plots(forest_plot, proportions_table,
                        widths = c(0.4, 0.1), ncol = 2)
  } else {
  	patchwork::wrap_plots(counts_table, proportions_table, forest_plot,
                        widths = c(0.5, 1, 2), ncol = 3)
  }

  caption <- "* Vertical lines indicate expected results if participants guessed at random. Understanding is based on both questions being answered correctly for each concept. † There was only one question for these concepts. ‡ Confidence intervals have been Bonferroni-corrected."
  caption <- stringr::str_wrap(caption, width = 140)
	if (!attitudes_intentions) result <- result + patchwork::plot_annotation(caption = caption)
	result
}
