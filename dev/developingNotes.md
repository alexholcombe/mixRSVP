# To-do

* Would be ridiculous to use ggplot2:: everytime invoked that ,but is that the way to get rid of "plot_hist_with_fit: no visible binding for global variable ‘SPE’" ?

* Pesky fitting errors.  Error in grad.default(ufn, ans$par, ...) : 
  function returns NA at 1.4434927002511e-050.000135401581392880.000100001 distance from x. I have started a test to investigate this in test-fit_model.R

* Need to add Pat's quantitative criterion (or similar) to exclude subjects with low efficacy. Have got "excluded field" from his MATLAB fits in backwards2_E1 that I can compare my test with.

# Testing

Use RStudio->Build->Test or 

