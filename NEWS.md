# iraceplot 2.0

 * Requires R >= 4.0 and irace >= 4.0.
 
 * Fix tables in the report not showing any value if there are NAs.
 
 * Handle iteration 0 in `summarise_by_iteration()`.

 * `summarise_by_configuration()` also prints mean rank.

 * `summarise_by_iteration()` tries harder to print unique minimal paths. It also handles non-character instances (like R objects).
 
 * Fix `parallel_coord()` not producing a plot with a single configuration.
 
 
# iraceplot 1.3

 * Requires [irace](https://mlopez-ibanez.github.io/irace/) version >= 4.0 and
   it will not work with earlier versions.
 
 * Fix negative RPD values (Fixed by @j-mezger).

 * Renamed `parallel_coord2()` to `plot_configurations()`. Completely rewrite `parallel_coord()` and `plot_configurations()` to produce nicer tickmarks and fix many bugs.
 

# iraceplot 1.2
 
 * New functions `parameters_tree()`, `parameters_summarise()`, `irace_summarise()`.

 * `plot_experiments_matrix()` now shows rejected configurations with a red `X`.

 * Give a better error if `pandoc` is missing.
 
# iraceplot 1.1

 * New function `ablation_plot()` to plot the result of `irace::ablation()`.

 * New function `summarise_by_configuration()`.
 
 
# iraceplot 1.0

 * Implement all plots that were available in the [irace](https://mlopez-ibanez.github.io/irace/) package and a few
   more.
   
 * First version in CRAN.
 






