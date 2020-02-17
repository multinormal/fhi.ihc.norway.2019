#' Add weighting columns to a data frame for use in post-stratification
#'
#' @param data a data frame in long format containing the data to recode.
#' 
#' @return a version of \code{data} with new columns \code{ISCED 2011 Level} (education
#'   levels), and \code{NUTS2} (in which counties have been mapped to NUTS 2
#'   regions).
#' @importFrom magrittr "%>%"
add_weight_columns <- function(data) {
	data %>%
		# Map the existing Education levels to ISCED 2011 levels.
		dplyr::mutate(`ISCED_Level` = forcats::fct_collapse(Education,
			"Levels 0-2" = "Primary",
			"Levels 3-4" = "Secondary",
			"Levels 5-8" = c("Tertiary (1-2 years)", "Tertiary (3-5 years)", "Masters degree", "PhD")
			) %>% ordered()) %>%
		# Map the existing County levels to NUTS 2 levels.
		dplyr::mutate(`NUTS2` = forcats::fct_collapse(County,
			"Oslo og Akershus" = c("Akershus", "Oslo"),
			"Hedmark og Oppland" = c("Hedmark", "Oppland"),
			"Sør-Østlandet" = c("Buskerud", "Østfold", "Telemark", "Vestfold"),
			"Agder og Rogaland" = c("Aust-Agder", "Rogaland", "Vest-Agder"),
			"Vestlandet" = c("Hordaland", "Møre og Romsdal", "Sogn og Fjordane"),
			"Trøndelag" = "Trøndelag",
			"Nord-Norge" = c("Finnmark", "Nordland", "Troms")
			))
}

#' Obtain a weighted survey design object for a data set (e.g., for a concept)
#'
#' @param data a data frame in long format containing the data.
#' 
#' @return a survey design object.
#' @importFrom magrittr "%>%"
weighted_design <- function(data) {
	# Define marginal distributions.
	
	# Data from Eurostat edat_lfse_03 data set (data from 2018).
	isced_level_marginal <- tibble::tribble(
		~`ISCED_Level`,				~Freq,
		"Levels 0-2",					0.239,
		"Levels 3-4",					0.386,
		"Levels 5-8",					0.375) %>% dplyr::mutate(Freq = nrow(data) * Freq)
	
	# Data from the Eurostat TGS00096 data set (population for 1/1/2019).
	nuts2_marginal <- tibble::tribble(
		~`NUTS2`,			~Freq,
		"Oslo og Akershus",		1287495,
		"Hedmark og Oppland",	386836,
		"Sør-Østlandet",			999639,
		"Agder og Rogaland",	777279,
		"Vestlandet",					899627,
		"Trøndelag",					458742,
		"Nord-Norge",					486001) %>% dplyr::mutate(Freq = nrow(data) * (Freq / sum(Freq)))
	
	# Data from Eurostat TPS00002 data set (data for 1/1/2019)
	sex_marginal <- tibble::tribble(
		~Sex,									~Freq,
		"Male",								2668371,
		"Female",							2627248) %>% dplyr::mutate(Freq = nrow(data) * (Freq / sum(Freq)))

	# Make an unweighted design object to begin with.
	unweighted_design <- survey::svydesign(ids = ~1, data = data, probs = 1)
	
	# Iterative post-stratify to marginal distributions of the sample and known population.
	survey::rake(design = unweighted_design,
							 sample.margins = list(~`ISCED_Level`, ~`NUTS2`, ~Sex),
							 population.margins = list(isced_level_marginal, nuts2_marginal, sex_marginal))
}