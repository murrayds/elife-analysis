# elife-analysis
This is the code and data used to conduct the analysis and produce final values and figures for the paper titled "Peer review and the role of author and gatekeeper homophily", submitted to PLoS Biology. 

## Abstract
*The fairness of scholarly peer review has been challenged by evidence of disparities in publication outcomes based on author characteristics.  To assess this, we conducted an exploratory analysis of peer review outcomes of 23,876 initial submissions and 7,192 full submissions that were submitted to the biosciences journal eLife between 2012 and 2017. Women and authors from nations outside of North America and Europe were underrepresented both as gatekeepers (editors and peer reviewers) and authors. We found evidence of a homophilic relationship between the demographics of the gatekeepers and authors and the outcome of peer review; that is, there were higher rates of acceptance in the case of gender and country homophily.  The acceptance rate for manuscripts with male last authors was 3.5 percentage points higher than for female last authors (95% CI = [0.5, 6.4]), and this gender inequity was greatest, at 4.7 percentage points (95% CI = [0.3, 9.1]), when the team of reviewers was all male; mixed-gender gatekeeper teams had no significant difference in the outcomes by the gender of author.  Homogeny between countries of the gatekeeper and the corresponding author was also associated with higher acceptance rates for many countries.  To test for the persistence of these effects in the face of potentially confounding variables, we conducted a logistic regression including document and author metadata.  Disparities in acceptance rates associated with gender and country of affiliation and the homophilic associations remained.  We conclude with a discussion of mechanisms that could contribute to this effect, directions for future research, and policy implications. Code and anonymized data have been made available at https://github.com/murrayds/elife-analysis*

## Data
Data used in this analysis was graciously provided to us by *eLife*, an open source journal that published in the life sciences. This data contains information on 23,876 initial submissions and 7,192 full submissions that were submitted between 2012 and 2017. These information consist of peer review outcomes and metadata for authors, gatekeepers (editors, reviewers) the submission itself. These data were further enriched with metadata relating to individual's country and institution (see the manuscirpt for details). 

We have made every attempt to make this data and analysis as available as possible while also protecting the sensitive data contained. The code for all analysis has been made publicly available in this repsitory. However, only processed and anonymized data has been made available. Anonymized data does not include information on author or gatekeeper names, institions, or countries; additionally, this data does not include the gender of senior editors, due to their low number. The anonymized and processed data will allow a user to conduct all analyses except for the those in the figures/general_information directory, which rely on a non-anonymized version that has not been made available. 

## Analysis
Analysis is contained in the figures/ directory. Each file within is an independent R notebook that, when run in full, will output numeric values or a figure that appears in the manuscript. Users will need to update load and save paths with the appropriate path on their personal computer. There are also a host of packages that are necessary to run these code, including tidyverse, stargazer, grid and gridExtra. 

Fig 1: Produced using data and in the formatted_data/elife_alluvial_data.csv file. The actual figure was produced using [RAWGraphs](https://rawgraphs.io/), and so no code appears here. 

Fig 2: figures/selectivity_over_time/selectivity_over_time.rmd

Fig 3: figures/gatekeeper_representation/gatekeeper_representation.rmd

Fig 4: figures/author_outcomes/submission_outcome_by_gender.rmd

Fig 5: figures/gatekeeper_author_outcomes/gatekeeper_author_outcomes.rmd

Fig 6: figures/regression_analysis/regression_analysis_simple.rmd

Fig 7: figures/regression_analysis/regression_analysis_interaction.rmd

Fig SI 1: figures/revision_information/average_revisions.rmd

Fig SI 2: figures/appeals/gender_and_appeals.rmd

Fig SI 3: figures/author_outcomes/supp_submission_outcomes.rmd

Fig SI 4: figures/selectivity_over_time/country_composition_shift.rmd

Fig SI 5: figures/general_infromation/supp_gender_prop_by_country.rmd

Fig SI 6: figures/gatekeeper_author_outcomes/supp_homophily_outcomes.rmd

Fig SI 7: figures/author_outcomes/supp_outcomes_16_countries.rmd

Fig SI 8: figures/gatekeeper_representation/supp_reviewing_editor_composition.rmd

Fig SI 9: figures/gatekeeper_representation/reviewing_editor_continental_comp.rmd


## Contact
If you have questions regarding the code and the anaysis, please direct them to myself, Dakota Murray, at dakota.s.murray@gmail.com. Please feel free to follow up if I am slow to respond.



