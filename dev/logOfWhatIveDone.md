* Did use_r("createGuessingDistribution")
* Did use_roxygen_md()
* Did use_data_raw() - this will not be visible to package users I think. Add data creation scripts there, then use_data to create proper package data format.
* Did use_data(P2E2pilot)
* Did use_package('optimx').  Refer to functions with `optimx::fun()`

# The first practical advantage to using a package is that it’s easy to re-load your code. # You can either run devtools::load_all()
