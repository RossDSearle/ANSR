
#' ANSIS Data Disclaimer
#' @author Ross Searle
#' @return character
#' @export

ANSIS_Disclaimer <- function(){

    cat(paste0( crayon::blue(crayon::bold( '\n############################  DISCLAIMER  ############################ \n\n')),

        crayon::blue('Data and other content made accessible via ANSIS is provided by multiple providers, harmonised and made available on an ‘as is’ basis.
        \nIf you use or download any data or other material made accessible by ANSIS you do so at your own risk and you acknowledge that such data may be\nincomplete or not applicable to all situations.\n\n
    To the extent permitted by law, CSIRO excludes all liability to any person for any consequences, including but not limited to all losses, damages,\n costs, expenses and any other compensation, arising directly or indirectly from accessing and using any information or material contained in it.\n\n\n'
    )))
}

#' ANSIS Data System Citation
#' @author Ross Searle
#' @return character
#' @export

ANSIS_Citation <- function(){

  cat('\n\nAustralian Government - Department of Agriculture, Fisheries and Forestry. (2025). Australian National Soil Information System (ANSIS). https://ansis.net/ \n\n')

}

#' ANSIS Funding Ackowledgement
#' @author Ross Searle
#' @return character
#' @export
#'
ANSIS_Ackowledgement <- function(){
  h <- paste0(crayon::blue(crayon::bold( '\n############################  ACKNOWLEDGEMENT  ############################ \n\n')))
  prvs <- apiCatalogueSummary()
  poviderNames <- prvs$name
  p <- paste0(poviderNames, collapse = ', ')
  cat(paste0(h, crayon::blue('ANSIS is supported by funding through the Australian Government National Soil Strategy (Department of Agriculture, Fisheries and Forestry)\nin collaboration with CSIRO and partner organisations.\n\nPartners : ', p, '\n\n')))
}


#' ANSR Package Citation
#' @author Ross Searle
#' @return character
#' @export
#'
ANSR_PackageCitation <- function(){
  suppressWarnings(citation('ANSR'))
#   c$year <- '2026'
#  #c2 <- stringr::str_replace(c, '????', '2026')
# c
}


#' ANSIS Useful Info Resources
#' @author Ross Searle
#' @return character
#' @export
ANSIS_Info <- function(){
  What <- c('ANSIS Website',
            'About',
            'R package',
            'CSIRO')

  Where <- c( 'https://ansis.net/',
             'https://www.agriculture.gov.au/agriculture-land/farm-food-drought/natural-resources/soils/national-soil-action-plan/ansis',
             'https://github.com/RossDSearle/ANSR',
             'https://www.csiro.au/en/research/natural-environment/land/soil/ansis')
data.frame(What, Where)
  }
