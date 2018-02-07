# To-do

* Would be ridiculous to use ggplot2:: everytime invoked that ,but is that the way to get rid of "plot_hist_with_fit: no visible binding for global variable ‘SPE’" ?

* Pesky fitting errors.  Error in grad.default(ufn, ans$par, ...) : 
  function returns NA at 1.4434927002511e-050.000135401581392880.000100001 distance from x.

# Testing

test_file('mixtureModeling/tests/test_fitModel.R')
test_dir('mixtureModeling/tests/') # test all files in directory

