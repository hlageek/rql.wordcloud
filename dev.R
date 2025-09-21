require(devtools)
devtools::document()
devtools::load_all()
mod_preview()

devtools::document()
devtools::build()
renv::install("/Users/radimhladik/repos/requal_1.3.4.9001.tar.gz")
requal::RequalAPI()

?shiny::sliderInput
