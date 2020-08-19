#' Translate: "Ja" -> "Yes" and "Nei" -> Nei
#' @importFrom magrittr "%>%"
translate_yes_no <- function(data) {
	data %>%
		dplyr::mutate_all(dplyr::funs(stringr::str_replace(., "Ja", "Yes"))) %>%
    dplyr::mutate_all(dplyr::funs(stringr::str_replace(., "Nei", "No"))) %>%
    dplyr::mutate_all(dplyr::funs(stringr::str_replace(., "Galt", "Wrong"))) %>%
    dplyr::mutate_all(dplyr::funs(stringr::str_replace(., "Rett", "Right")))
}

#' Translate a Sex column of a data frame from Norwegian to English
#' @importFrom magrittr "%>%"
translate_sex <- function(data) {
	data %>% dplyr::mutate(Sex = forcats::fct_recode(Sex, Male = "Mann", Female = "Kvinne"))
}

#' Translate an Education column of a data frame from Norwegian to English
#' @importFrom magrittr "%>%"
translate_education <- function(data) {
	data %>% dplyr::mutate(Education = forcats::fct_recode(Education,
												 "Primary" = "Grunnskolen",
												 "Secondary" = "Videregående",
												 "Tertiary (1-2 years)" = "Høgskole/universitet (ett til to år)",
												 "Tertiary (3-4 years)" = "Høgskole/universitet (tre til fire år)",
												 "Masters degree" = "Høgskole/universitet (master)",
												 "PhD" = "Høgskole/universitet (PhD)") %>%
												 forcats::fct_relevel(c(
												 		"Primary",
												 		"Secondary",
												 		"Tertiary (1-2 years)",
												 		"Tertiary (3-4 years)",
												 		"Masters degree",
												 		"PhD")))
}

#' Translate "all" (i.e., certain) columns
#' @importFrom magrittr "%>%"
translate_all <- function(data) {
	data %>%
	 translate_yes_no() %>%
	 translate_sex() %>%
	 translate_education()
}
