# To-do

* Create simplest possible vignette, to analyze just one condition. Basically filter data and then call analyzeOneCondition

* Does the namespace for annotate()  , ggplot2::annotate somehow get screwed up so you have to specify ggplot2::annotate?

*   #to avoid writing the text one time for each data point, would like to cut to one trial per condition, but can't do that without
  #knowing the grouping variable for the facets
  #how do I keep one x from each condition? This is basically like stat_summary
  #See https://stackoverflow.com/questions/15720545/use-stat-summary-to-annotate-plot-with-number-of-observations

  #No, unfortunately stat_summary only gets the numbers, this is the way to do it https://stackoverflow.com/a/46791112/302378
  Have to create separate dataset and geom_text call to add the annotations
  
* I cast warnings as a character to fix a problem but side effect is getting these messages 
"Unequal factor levels: coercing to characterbinding character and factor vector, coercing into character vectorbinding character and factor vector"

*  Add   set.seed(1) # Reproducibility
to everything

* Add inferential stats to analyze_experiment.Rmd vignette

* Pesky fitting errors.  Error in grad.default(ufn, ans$par, ...) : 
  function returns NA at 1.4434927002511e-050.000135401581392880.000100001 distance from x. I have started a test to investigate this in test-fit_model.R

* Need to add Pat's quantitative criterion (or similar) to exclude subjects with low efficacy. Have got "excluded field" from his MATLAB fits in backwards2_E1 that I can compare my test with.

* Add for the nReplicates parameter roxygen a link to the notes on how many fits are needed for stability 

* add a link somewhere, somehow to BackwardsLtrs (Google Drive) E2/compareMATLABtoR.html 

# Stats

For comparing models, for the AB case of having two episodes, Pat used Bayesian Information Criteria (BIC) for the single- and dual-episode models, based on the combined T1 and T2 distribution, to determine when there is evidence of a second attentional episode.  in AB_compare_models.m

For comparing guessing only to mixture, Pat did:

%         % Test for a significant difference in log likelihoods
%         [h,pValue,stat,cValue] = lratiotest(-minNegLogLikelihood,-uniformNegLogLikelihood,nFreeParameters,pCrit);

The mixture model has 3 degrees of freedom I think (e, u, sigma) and the guessing distrbiutino zero.
    

# Questions

#insanely, format.p cannot be found by annotate_fit call from top environment even when it's within the function defined

* Would be ridiculous to use ggplot2:: everytime invoked that ,but is that the way to get rid of "plot_hist_with_fit: no visible binding for global variable ‘SPE’" ?

# Testing

Use RStudio->Build->Test or 

Damn you can't see the vignettes unless you install the proper package from tgz etc or
https://stackoverflow.com/questions/33614660/knitr-rmd-vignettes-do-not-appear-with-vignette?rq=1