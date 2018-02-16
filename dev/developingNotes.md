# To-do

*  Add   set.seed(1) # Reproducibility
to everything

* Work out why likelihood ratio test is so lenient

* Write function for annotating with all the parameters, and p-value

* Add inferential stats to analyze_experiment.Rmd vignette

* Pesky fitting errors.  Error in grad.default(ufn, ans$par, ...) : 
  function returns NA at 1.4434927002511e-050.000135401581392880.000100001 distance from x. I have started a test to investigate this in test-fit_model.R

* Need to add Pat's quantitative criterion (or similar) to exclude subjects with low efficacy. Have got "excluded field" from his MATLAB fits in backwards2_E1 that I can compare my test with.

* Add for the nReplicates parameter roxygen a link to the notes on how many fits are needed for stability 

* add a link somewhere to compareMATLABtoR.html 

# Stats

For comparing models, for the AB case of having two episodes, Pat used Bayesian Information Criteria (BIC) for the single- and dual-episode models, based on the combined T1 and T2 distribution, to determine when there is evidence of a second attentional episode.  in AB_compare_models.m

For comparing guessing only to mixture, Pat did:

%         % Test for a significant difference in log likelihoods
%         [h,pValue,stat,cValue] = lratiotest(-minNegLogLikelihood,-uniformNegLogLikelihood,nFreeParameters,pCrit);

The mixture model has 3 degrees of freedom I think (e, u, sigma) and the guessing distrbiutino zero.
    

# Questions

* Would be ridiculous to use ggplot2:: everytime invoked that ,but is that the way to get rid of "plot_hist_with_fit: no visible binding for global variable ‘SPE’" ?

# Testing

Use RStudio->Build->Test or 

Damn you can't see the vignettes unless you install the proper package from tgz etc or
https://stackoverflow.com/questions/33614660/knitr-rmd-vignettes-do-not-appear-with-vignette?rq=1