afex_options() # see all options

afex_options("return_aov") #get single option

aop <- afex_options() # save current options

\dontrun{
# change options
afex_options(return_aov = "nice")
afex_options("return_aov") #get single option
afex_options(return_aov = "nice", method_mixed = "LRT")
afex_options("method_mixed") #get single option
# do something
}
afex_options(aop) # reset options

