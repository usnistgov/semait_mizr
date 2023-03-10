# mizr CHANGELOG

## v2023.03.10

Migration to new location and new overall name semait_mizr

## v2023.03.07

This version is the overhaul of the software to prepare it for release.

* Removed yml_meta parameter
* Removed extract_metadata_information.R as we are currently removing this method of metadata
extraction from the code.
* Added in our public dataset semait_sdms_1.cs, as well as the scripts to produce this dataset in the `scripts_data` directory
* Updated documentations with new example vignettes, revisions to conform with the new package, and and updated README.
* Added SEMAIT analysis script, both a documented script (as a vignette) and the pure R code
to the package.

## v2023.01.30

Still preparing for release. Added first version of plots for ``Towards a Structured Evaluation Methodology for Artificial Intelligence
Technology'' (SEMAIT)

Additional Updates
* Added `mizr_sign_test_plot()`
* Added tweaks to block plots.

## v2022.12.06

Last version before cleanup for review and release

Additional Updates
* added `mizr_block_rank_plot()` for ranking comparisons of systems; this is paired with a 
block plot.
* Fixed bug in `tile_table_produce_plot()` and `title_table_count_produce_plot()` so that plots 
with a single value are produced.  This is common when all of the counts of a count plot are
1.
* Fixed bug in `tile_table_produce_plot()` and `title_table_count_produce_plot()` so that the '(all)' columns are at the right and bottom.

## v 2022.04.15

Added additional plots

Additional Updates
* Added `mizr_order_plot`
* Added `base_size` parameter to `mizr_set_theme`
* Add `mizr_tf_count_plot` for count plots when only one or two factors matter, and analagous
`mizr_tile_tf_plot` for average response variables for those factor combinations.


## v 2022.04.05

Removed use of facet nesting, removing requirement to work with teunbrand/gg4hx. The dependency 
has been removed from the package and the documentation.

Additional updates
* Updated `tile_table_count_plot`, `tile_table_plot`, and `block_plot` to improve speed and to remove
facet_nested.
* Formatted the code to work with the automated code formatting `styler` package.

## v 2021.05.27

Updated to work with new dependency teunbrand/gg4hx  (https://github.com/teunbrand/ggh4x) and to work on later versions of R, including R 4.1. This new version requires ggplot2 >= 3.3.0, which in turn requires R 3.6.2 or higher.

Some Additional Updates:
* Ported test and code to work with updated version of R.
* Added `mixr_tile_table_count()` plot
* Added feature to change the text size in select plots including `mizr_tile_table_plot()` with parameter `tile_text_size` (Default value based on relative size, which is `rel(1.6)`)
* Added a LICENSE.txt file.

## v 2020.06.09

Provided various bugfixes to plots

* Fixed the computation of the grand mean() to handle missing entries
* Changed name of style `boxplot` of `mizr_scatter_doe_plot()` to style name `boxplot_points`
* Added style with previous name `boxplot` of `mizr_scatter_doe_plot()` that plots the boxplots but without the scatter points.
* Made default colors for `mizr_tile_table_plot()` more transparent

## v 2020.02.12

Renaming of these functions is so that all functions conform to being at most 30 characters,
which is a desirable property of R style (as checked by the lintr)

* Added a `weighted` boolean (TRUE, FALSE) to the `compute_grand_mean()` to either first avergae
* by factor settings and then average (weighted = TRUE) or take an average of all trials (weighted = FALSE)
* Added a `weighted` setting to `mizr_main_effects_plot()` to adjust the plot to produce weighted
or unweighted effects. Default is weighted = FALSE to mimic previous behavior for backwards compatibility.
* Changed name of `mizr_interaction_comparison_plot()` to `mizr_interact_compare_plot()`
* Changed name of `mizr_normal_probability_plot()` to `mizr_normal_prob_plot()`
* Changed name of `interaction_compare_compute_labels()` to `interact_compare_labels()`
* Changed name of `mizr_main_effects_marginal_plot()` to `mizr_main_effects_mar_plot()`
* Changed name of `main_effects_mar_produce_plot()` to `main_effects_mar_produce_plot` 
* Fixed bugs in `mizr_effect_plot()` computation data frames and added a few test cases to check.
* Added `mizr_tile_table_count_plot()` to produce a tile table of counts rather than averages; this is useful for checking the coverage or the amount of imbalance in a data collection for an experiment
* Changed name of grand mean computer to `compute_grand_mean()` and adjusted computation to handle missing cells.
* Used an updated lintr; this includse using seq_len(length(v)) instead of 1:length(v) because if v has length 0, the former results in not executing the loop while the latter gives us element 0, which is out of bounds. `seq_len` is a function in the base package of R.

## v 2020.01.03

* Added `mizr_effect_plot` and `effects_compute_df` to compute effect sizes (absolute value) for main
effects and two-term interactions.
* added method `two_term_level_effects_df` to get all of the effects for all level combinations for
two distinct factors (`main_effects_hsd_compute_df()`, previously done, already gives all effects
for main effects)
* Updated documentation to add tile_table_plots and effect_plots
* Added parameter `tile_text_size` to `mizr_tile_table_plot` to allow for customizable text size

## v 2019.08.28

Renamed `mizr_main_effects_comparison_plot` to `mizr_interaction_comparison_plot`

Additionally

New and enhanced plots:

* Fixed hsd plot to properly order factors as specified, rather than in alphabetical order
* Added `mizr_table_tile_plot`, which provides a colored heatmap across primary and
secondary factors.
* Improved theming to eliminate unneeded space
* Cleaned up look of Block Plots
* Added nested faceting using the `facet_nested()` method from the `ggnomics` package (https://github.com/teunbrand/ggnomics) to enhance block plots and title plots
* Changed theme for improvements.
* Added the effect mean line for the main effect plots
* Enhanced `mizr_interaction_comparison_plot` to give shape and color cues.
* Added `mizr_scatter_doe_plot`, implementing the DOE Scatter plot with a dataplot style
and enhanced styles.

## v 2019.08.27

New and enhanced plots:

* No longer changes the ggplot theme by default. Instead, provides a `mizr_set_theme()` method
that changes the ggplot default theme, allowing for more customizability.
* Enhanced `mizr_main_effects` Plot to produce standard error bars with a configurable alpha that
can be used for Bonferonni corrections
* Enhanced `mizr_main_effects` To support weighted and unweighted averaging
* Removed `comp_func` parameter in `mizr_main_effects` because the statistics computed only
work if the mean is used.
* Ported over previous Detection Error Tradeoff (DET) Curve Plots to the mtehod `mizr_det_plot`.
DET curves support a primary and a secondary decision cost function, and given the original
mizr data frame with a score column and a key column (and a decision column if actual costs are
desired), computes the necessary data frames to plot.
* Fixed bugs in the `mizr_wide_to_long` and `mizr_long_to_wide`
* Added `mizr_main_effect_hsd_plot` that uses Tukey's Honest Significant Difference (HSD) method
to plot confidence intervals on the difference of events. Added parameter `alpha` to specify 
interval confidence level.
* Added Standard normal Confidence intervals on main effects in the `mizr_main_effects_plot` method
with the option `bar_ci`. Removed option `bar_sdn`. Added parameter `alpha` to specify 
interval confidence level.

## v 2019.07.23

New and Enhanced Plots

* Added boxplot `mizr_box_plot`
* Added `mizr_run_plot` (Plot one of Filliben's Four Plots)
* Added `mizr_lag_plot` (Plot two of Filliben's Four Plots)
* Added `mizr_histogram_plot` (Plot three of Filliben's Four Plots)
* Added `mizr_normal_probability_plot` (Plot four of Filliben's Four Plots)
* Changed x-asis text to be `theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))` to provide enhanced readability of the plots
* Re-added show_points variable to `mizr_box_plot`.

## v 2019.07.22

Changlist includes renaming plot methods and many enhancements, including software design
enhancements and plot enhancements. 

First, here are the methods that are renamed:

* `cr_scatter_plot` has been renamed to `mizr_scatter_plot`
* `cr_treatment_scatter_plot` has been renamed to `mizr_scatter_treatment_plot`
* `cr_effect_scatter_plot` has been renamed to `mizr_scatter_effect_plot`
* `cr_block_plot` has been renamed to `mizr_block_plot`
* `cr_main_effects_plot` has been renamed to `mizr_main_effects_plot`
* `cr_main_effects_comparison_plot` has been renamed to `mizr_main_effects_comparison_plot`
* `cr_m_main_effects_plot` has been renamed to `mizr_main_effects_marginal_plot`

The argument syntax and order may have been changed as well, so please consult the functions
if there are any errors.

Second, here are the enhacments:
* The custom `geom_text_temp` has been renamed to `geom_text_custom` and all of the code has been
placed in the file `geom_text_custom.R` so that the custom geom_text code can be reused across plots
* Each plot now is an external call of two submethods: `yyy_compute_df` and `yyy_produce_plot`, 
where the first produces the computed data frame that is rendered, and the second takes the
pre-computed data frame and produces a plot. This makes it easy to get the data computed by the plot
methods and also separates the implementation.
* The external plots are all in the file `mizr_plots.R`, and the implementations of the 
`yyy_compute_df` and `yyy_produce_plot` are now in separate files.

## v 2019.04.30

* Error bars (standard deviation divided by sqrt(n)) added for main effects plot
* Bug fixed to allow for proper display of pre-ordered factors for main effects plot and main effects comparison plot.

## v 2019.04.09

* Data frame conversion methods `mizr_long_to_wide()` and `mizr_wide_to_long()` added
* Additional documentation describing marginal data frames and use of conversion methods added
* Documentation from master branch deploys to gitlab pages at https://pcf.ipages.nist.gov/mig_analyzer

## v 2019.04.08

* cr_treatment_scatter_plot() now plots points by default rather than text-shaped labels as symbols. Use new argument shape_style = "text" to revert to plotting the old textual symbols
* Code now complies with lintr, and lintr added into CI
* mizr documentation now ported to `vignettes`

## v 0.1.1

First version