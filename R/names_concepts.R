#' The key concepts.
#' @export
concepts <- list(
	dramatic_effects =            "Large, dramatic effects are rare",     # "1.1b",
	#      
	explanations_wrong =          "Beliefs alone about how treatments work are not reliable predictors of the presence or size of effects",     # "1.2b",
	association_causation =       "An outcome may be associated with a treatment but not caused by it",     # "1.2d",
	comparisons_required =        "Identifying effects of treatments depends on making comparisons",     # "1.2f",
	consider_all_comparisons =    "The results of one study considered in isolation can be misleading",     # "1.2g",
	common_practice =             "Widely used treatments or those that have been used for decades are not necessarily beneficial or safe",     # "1.2h",
	newer_not_better =            "Treatments that are new or technologically impressive may not be better than available alternatives",     # "1.2i",
	more_not_better =             "Increasing the amount of a treatment does not necessarily increase its benefits and may cause harm",     # "1.2j",
	earlier_not_better =          "Earlier detection of ‘disease’ is not necessarily better",     # "1.2k",
	#      
	conflicting_interests =       "Competing interests may result in misleading claims",     # "1.3b",
	anecdotes =                   "Personal experiences or anecdotes alone are an unreliable basis for most claims",     # "1.3c",
	experts_not_correct =         "Opinions alone are not a reliable basis for claims",     # "1.3d",
	#      
	groups_similar_at_start =     "Comparison groups should be as similar as possible",     # "2.1a",
	treat_comparators_equally =   "The people being compared should be cared for similarly apart from the treatments being studied",     # "2.1c",
	patient_blinding =            "If possible, people should not know which of the treatments being compared they are receiving",     # "2.1d",
	assess_outcomes_equally =     "Outcomes should be assessed in the same way in all the groups being compared",     # "2.1e",
	all_should_be_followed_up =     "It is important to assess outcomes in all (or nearly all) the people in a study",     # "2.1g",
	itt_principle =          "People’s outcomes should be counted in the group to which they were allocated",     # "2.1h",
	#      
	systematic_reviews =          "Reviews of studies comparing treatments should use systematic methods",     # "2.2a",
	#    
	relative_effects_misleading = "Relative effects of treatments alone can be misleading",    # "2.3b",
	averages_misleading =         "Average differences between treatments can be misleading",   # "2.3c",
	sampling_error =              "Small studies may be misleading",   # "2.3d",
	subgroups_misleading =        "Results for a selected group of people within a study can be misleading",   # "2.3e",
	p_values_misleading =         "The use of p-values may be misleading; confidence intervals are more informative",   # "2.3f",
	confidence_intervals =        "Deeming results to be “statistically significant” or “nonsignificant” can be misleading",   # "2.3g",
	#    
	outcomes_matter_to_you =      "Attention should focus on all important effects of treatments, and not surrogate outcomes",   # "3.2a",
	are_you_different =           "Fair comparisons of treatments in animals or highly selected groups of people may not be relevant",   # "3.2b",
	practical_your_setting =      "The treatments compared should be similar to those of interest",   # "3.2c",
	#    
	advantages_disadvantages =    "Weigh the benefits and savings against the harms and costs of acting or not",   # "3.3a",
	certainty_of_evidence =       "Consider how certain you can be about each advantage and disadvantage"   # "3.3b"
)

#' Key concepts common to all four questionnaires and must be Bonferroni-corrected.
#' @export
common_concepts <- c(concepts$association_causation,
										 concepts$common_practice,
										 concepts$groups_similar_at_start,
										 concepts$consider_all_comparisons)
