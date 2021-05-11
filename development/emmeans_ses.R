

library("afex")
library("emmeans")
data(md_12.1)
a1 <- aov_ez("id", "rt", md_12.1, within = c("angle", "noise"))

emm1 <- emmeans(a1, "angle")
pairs(emm1)
#  contrast estimate   SE df t.ratio p.value
#  X0 - X4      -108 18.9 18 -5.724  0.0001 
#  X0 - X8      -168 18.9 18 -8.904  <.0001 
#  X4 - X8       -60 18.9 18 -3.180  0.0137 
# 
# Results are averaged over the levels of: noise 
# P value adjustment: tukey method for comparing a family of 3 estimates 


emm2 <- emmeans(a1, "angle", model = "multivariate")
pairs(emm2)
#  contrast estimate   SE df t.ratio p.value
#  X0 - X4      -108 17.4  9 -6.194  0.0004 
#  X0 - X8      -168 20.6  9 -8.159  0.0001 
#  X4 - X8       -60 18.4  9 -3.254  0.0244 
# 
# Results are averaged over the levels of: noise 
# P value adjustment: tukey method for comparing a family of 3 estimates 

sessionInfo()
# R version 4.0.3 (2020-10-10)
# Platform: x86_64-w64-mingw32/x64 (64-bit)
# Running under: Windows 10 x64 (build 19042)
# 
# Matrix products: default
# 
# locale:
# [1] LC_COLLATE=English_United Kingdom.1252 
# [2] LC_CTYPE=English_United Kingdom.1252   
# [3] LC_MONETARY=English_United Kingdom.1252
# [4] LC_NUMERIC=C                           
# [5] LC_TIME=English_United Kingdom.1252    
# 
# attached base packages:
# [1] stats     graphics  grDevices utils     datasets  methods   base     
# 
# other attached packages:
# [1] emmeans_1.5.3 afex_0.28-0   lme4_1.1-26   Matrix_1.3-2 
# 
# loaded via a namespace (and not attached):
#   [1] readxl_1.3.1         backports_1.2.1      jtools_2.1.2        
#   [4] plyr_1.8.6           igraph_1.2.6         splines_4.0.3       
#   [7] crosstalk_1.1.0.1    usethis_2.0.0        ggplot2_3.3.3       
#  [10] TH.data_1.0-10       rstantools_2.1.1     inline_0.3.17       
#  [13] digest_0.6.27        htmltools_0.5.0      rsconnect_0.8.16    
#  [16] lmerTest_3.1-3       fansi_0.4.1          magrittr_2.0.1      
#  [19] memoise_1.1.0        openxlsx_4.2.3       remotes_2.2.0       
#  [22] RcppParallel_5.0.2   matrixStats_0.57.0   xts_0.12.1          
#  [25] sandwich_3.0-0       prettyunits_1.1.1    colorspace_2.0-0    
#  [28] haven_2.3.1          xfun_0.20            dplyr_1.0.2         
#  [31] callr_3.5.1          crayon_1.3.4         jsonlite_1.7.2      
#  [34] survival_3.2-7       zoo_1.8-8            glue_1.4.2          
#  [37] gtable_0.3.0         V8_3.4.0             car_3.0-10          
#  [40] pkgbuild_1.2.0       rstan_2.21.2         abind_1.4-5         
#  [43] scales_1.1.1         mvtnorm_1.1-1        rstatix_0.6.0       
#  [46] miniUI_0.1.1.1       Rcpp_1.0.5           xtable_1.8-4        
#  [49] foreign_0.8-81       stats4_4.0.3         StanHeaders_2.21.0-7
#  [52] DT_0.17              htmlwidgets_1.5.3    threejs_0.3.3       
#  [55] ellipsis_0.3.1       pkgconfig_2.0.3      loo_2.4.1           
#  [58] tidyselect_1.1.0     rlang_0.4.10         reshape2_1.4.4      
#  [61] later_1.1.0.1        munsell_0.5.0        cellranger_1.1.0    
#  [64] tools_4.0.3          cli_2.2.0            generics_0.1.0      
#  [67] devtools_2.3.2       broom_0.7.3          ggridges_0.5.3      
#  [70] evaluate_0.14        stringr_1.4.0        fastmap_1.0.1       
#  [73] yaml_2.2.1           processx_3.4.5       knitr_1.30          
#  [76] fs_1.5.0             zip_2.1.1            pander_0.6.3        
#  [79] purrr_0.3.4          nlme_3.1-151         mime_0.9            
#  [82] rstanarm_2.21.1      compiler_4.0.3       pbkrtest_0.5-0.1    
#  [85] bayesplot_1.8.0      shinythemes_1.1.2    rstudioapi_0.13     
#  [88] curl_4.3             testthat_3.0.1       ggsignif_0.6.0      
#  [91] tibble_3.0.4         statmod_1.4.35       stringi_1.5.3       
#  [94] ps_1.5.0             desc_1.2.0           forcats_0.5.0       
#  [97] lattice_0.20-41      nloptr_1.2.2.2       markdown_1.1        
# [100] shinyjs_2.0.0        vctrs_0.3.6          pillar_1.4.7        
# [103] lifecycle_0.2.0      estimability_1.3     data.table_1.13.6   
# [106] httpuv_1.5.4         R6_2.5.0             promises_1.1.1      
# [109] gridExtra_2.3        rio_0.5.16           sessioninfo_1.1.1   
# [112] codetools_0.2-18     boot_1.3-25          colourpicker_1.1.0  
# [115] MASS_7.3-53          gtools_3.8.2         assertthat_0.2.1    
# [118] pkgload_1.1.0        rprojroot_2.0.2      withr_2.3.0         
# [121] shinystan_2.5.0      multcomp_1.4-15      parallel_4.0.3      
# [124] hms_0.5.3            grid_4.0.3           tidyr_1.1.2         
# [127] coda_0.19-4          minqa_1.2.4          rmarkdown_2.6       
# [130] carData_3.0-4        ggpubr_0.4.0         numDeriv_2016.8-1.1 
# [133] shiny_1.5.0          base64enc_0.1-3      dygraphs_1.1.1.6  


#################################################################
##                         new emmeans                         ##
#################################################################

library("afex")
library("emmeans")
data(md_12.1)
a1 <- aov_ez("id", "rt", md_12.1, within = c("angle", "noise"))

emm1 <- emmeans(a1, "angle")
pairs(emm1)
#  contrast estimate   SE df t.ratio p.value
#  X0 - X4      -108 18.9 18 -5.724  0.0001 
#  X0 - X8      -168 18.9 18 -8.904  <.0001 
#  X4 - X8       -60 18.9 18 -3.180  0.0137 
# 
# Results are averaged over the levels of: noise 
# P value adjustment: tukey method for comparing a family of 3 estimates 


emm2 <- emmeans(a1, "angle", model = "multivariate")
pairs(emm2)
#  contrast estimate   SE df t.ratio p.value
#  X0 - X4      -108 17.4  9 -6.194  0.0004 
#  X0 - X8      -168 20.6  9 -8.159  0.0001 
#  X4 - X8       -60 18.4  9 -3.254  0.0244 
# 
# Results are averaged over the levels of: noise 
# P value adjustment: tukey method for comparing a family of 3 estimates 
