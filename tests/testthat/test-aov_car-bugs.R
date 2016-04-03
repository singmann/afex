
context("ANOVAs: known bugs")

test_that("regex works correctly in aov_car when also having within factors outside the Error term", {
  data(obk.long)
  expect_is(aov_car(value ~ treatment * gender*phase*hour + Error(id/phase*hour), data = obk.long), "afex_aov")
})

test_that("another label bug (May 2014)", {
  data("sk2011.1")
  levels(sk2011.1$inference) <- c("A+:D-", "A+:D+", "A-:D+", "A- : D-")
  expect_is(suppressWarnings(aov_ez("id", "response", sk2011.1, between = "instruction", within = c("type", "inference"), return = "Anova")), "Anova.mlm")  
})

test_that("orig label bug", {
  data(obk.long)
  obk2 <- obk.long
  levels(obk2$phase) <- c("fup test", "post-hans", "pre tenetious")
  expect_is(suppressWarnings(aov_car(value ~ treatment * gender + age + Error(id/phase*hour), data = obk2, factorize=FALSE, return = "Anova")), "Anova.mlm")
})

test_that("ANCOVA check bug (reported by Gang Chen), January 2013", {
  dat <- read.table(header=TRUE, text = "ID Group Gender ROI Value Propdd00 GAS0 MAD0 CPD0
2016 AE M 05_06 1.581 0.543 1.908 0.439999999999998 -0.5335
2016 AE M 07_08 1.521 0.543 1.908 0.439999999999998 -0.5335
2016 AE M 09_10 1.623 0.543 1.908 0.439999999999998 -0.5335
2016 AE M 03_04 1.569 0.543 1.908 0.439999999999998 -0.5335
2016 AE M 11_12 1.719 0.543 1.908 0.439999999999998 -0.5335
2016 AE M 01_02 1.509 0.543 1.908 0.439999999999998 -0.5335
2031 HC F 09_10 1.739 -0.014 0.0480000000000018 -2.347 1.9665
2031 HC F 01_02 1.763 -0.014 0.0480000000000018 -2.347 1.9665
2031 HC F 03_04 1.8 -0.014 0.0480000000000018 -2.347 1.9665
2031 HC F 11_12 1.793 -0.014 0.0480000000000018 -2.347 1.9665
2031 HC F 05_06 1.765 -0.014 0.0480000000000018 -2.347 1.9665
2031 HC F 07_08 1.654 -0.014 0.0480000000000018 -2.347 1.9665
2063 AE F 11_12 1.742 -0.027 2.348 -8.88 -0.0335000000000001
2063 AE F 01_02 1.634 -0.027 2.348 -8.88 -0.0335000000000001
2063 AE F 03_04 1.638 -0.027 2.348 -8.88 -0.0335000000000001
2063 AE F 07_08 1.604 -0.027 2.348 -8.88 -0.0335000000000001
2063 AE F 09_10 1.654 -0.027 2.348 -8.88 -0.0335000000000001
2063 AE F 05_06 1.625 -0.027 2.348 -8.88 -0.0335000000000001
2042 HC M 05_06 1.649 -0.014 2.058 -3.497 -0.8635
2042 HC M 07_08 1.565 -0.014 2.058 -3.497 -0.8635
2042 HC M 09_10 1.765 -0.014 2.058 -3.497 -0.8635
2042 HC M 03_04 1.677 -0.014 2.058 -3.497 -0.8635
2042 HC M 11_12 1.706 -0.014 2.058 -3.497 -0.8635
2042 HC M 01_02 1.618 -0.014 2.058 -3.497 -0.8635
2071 AE M 05_06 1.712 -0.317 -0.802 6.74 1.9665
2071 AE M 07_08 1.64 -0.317 -0.802 6.74 1.9665
2071 AE M 09_10 1.791 -0.317 -0.802 6.74 1.9665
2071 AE M 03_04 1.725 -0.317 -0.802 6.74 1.9665
2071 AE M 11_12 1.782 -0.317 -0.802 6.74 1.9665
2071 AE M 01_02 1.712 -0.317 -0.802 6.74 1.9665
2134 HC M 05_06 1.672 -0.014 0.347999999999999 -5.807 -2.5335
2134 HC M 07_08 1.657 -0.014 0.347999999999999 -5.807 -2.5335
2134 HC M 09_10 1.791 -0.014 0.347999999999999 -5.807 -2.5335
2134 HC M 03_04 1.633 -0.014 0.347999999999999 -5.807 -2.5335
2134 HC M 11_12 1.859 -0.014 0.347999999999999 -5.807 -2.5335
2134 HC M 01_02 1.653 -0.014 0.347999999999999 -5.807 -2.5335
2009 AE F 09_10 1.672 -0.027 1.058 3.36 11.1365
2009 AE F 03_04 1.723 -0.027 1.058 3.36 11.1365
2009 AE F 05_06 1.676 -0.027 1.058 3.36 11.1365
2009 AE F 07_08 1.622 -0.027 1.058 3.36 11.1365
2009 AE F 01_02 1.633 -0.027 1.058 3.36 11.1365
2009 AE F 11_12 1.853 -0.027 1.058 3.36 11.1365
2132 HC M 05_06 1.758 -0.014 -1.082 -2.857 -0.0335000000000001
2132 HC M 07_08 1.623 -0.014 -1.082 -2.857 -0.0335000000000001
2132 HC M 09_10 1.843 -0.014 -1.082 -2.857 -0.0335000000000001
2132 HC M 03_04 1.773 -0.014 -1.082 -2.857 -0.0335000000000001
2132 HC M 11_12 1.806 -0.014 -1.082 -2.857 -0.0335000000000001
2132 HC M 01_02 1.708 -0.014 -1.082 -2.857 -0.0335000000000001
2127 HC F 11_12 1.824 -0.014 0.628 6.223 -0.5335
2127 HC F 09_10 1.871 -0.014 0.628 6.223 -0.5335
2127 HC F 01_02 1.687 -0.014 0.628 6.223 -0.5335
2127 HC F 03_04 1.699 -0.014 0.628 6.223 -0.5335
2127 HC F 07_08 1.646 -0.014 0.628 6.223 -0.5335
2127 HC F 05_06 1.738 -0.014 0.628 6.223 -0.5335
2081 AE M 09_10 1.807 -0.027 -2.082 2.43 -1.5335
2081 AE M 11_12 1.917 -0.027 -2.082 2.43 -1.5335
2081 AE M 03_04 1.767 -0.027 -2.082 2.43 -1.5335
2081 AE M 05_06 1.776 -0.027 -2.082 2.43 -1.5335
2081 AE M 07_08 1.733 -0.027 -2.082 2.43 -1.5335
2081 AE M 01_02 1.775 -0.027 -2.082 2.43 -1.5335
2086 AE F 11_12 1.768 -0.457 -1.082 -1.76 6.9665
2086 AE F 09_10 1.769 -0.457 -1.082 -1.76 6.9665
2086 AE F 01_02 1.752 -0.457 -1.082 -1.76 6.9665
2086 AE F 03_04 1.769 -0.457 -1.082 -1.76 6.9665
2086 AE F 05_06 1.751 -0.457 -1.082 -1.76 6.9665
2086 AE F 07_08 1.728 -0.457 -1.082 -1.76 6.9665
2033 HC M 05_06 1.804 0.126 2.768 7.083 -2.2035
2033 HC M 07_08 1.784 0.126 2.768 7.083 -2.2035
2033 HC M 09_10 1.948 0.126 2.768 7.083 -2.2035
2033 HC M 03_04 1.821 0.126 2.768 7.083 -2.2035
2033 HC M 11_12 2.143 0.126 2.768 7.083 -2.2035
2033 HC M 01_02 1.824 0.126 2.768 7.083 -2.2035
2007 AE M 07_08 1.554 -0.027 0.488 -6.05 -0.5335
2007 AE M 05_06 1.643 -0.027 0.488 -6.05 -0.5335
2007 AE M 09_10 1.674 -0.027 0.488 -6.05 -0.5335
2007 AE M 03_04 1.593 -0.027 0.488 -6.05 -0.5335
2007 AE M 11_12 1.726 -0.027 0.488 -6.05 -0.5335
2007 AE M 01_02 1.517 -0.027 0.488 -6.05 -0.5335
6062 HC M 05_06 1.911 -0.014 -3.802 4.093 -3.5335
6062 HC M 07_08 1.887 -0.014 -3.802 4.093 -3.5335
6062 HC M 09_10 1.951 -0.014 -3.802 4.093 -3.5335
6062 HC M 03_04 1.798 -0.014 -3.802 4.093 -3.5335
6062 HC M 11_12 1.953 -0.014 -3.802 4.093 -3.5335
6062 HC M 01_02 1.772 -0.014 -3.802 4.093 -3.5335
2072 AE M 05_06 1.667 0.253 1.908 0.289999999999999 -1.0335
2072 AE M 07_08 1.587 0.253 1.908 0.289999999999999 -1.0335
2072 AE M 09_10 1.739 0.253 1.908 0.289999999999999 -1.0335
2072 AE M 03_04 1.638 0.253 1.908 0.289999999999999 -1.0335
2072 AE M 11_12 1.784 0.253 1.908 0.289999999999999 -1.0335
2072 AE M 01_02 1.662 0.253 1.908 0.289999999999999 -1.0335
2008 HC F 05_06 1.623 -0.014 -1.372 -2.317 2.1365
2008 HC F 07_08 1.6 -0.014 -1.372 -2.317 2.1365
2008 HC F 09_10 1.688 -0.014 -1.372 -2.317 2.1365
2008 HC F 03_04 1.624 -0.014 -1.372 -2.317 2.1365
2008 HC F 11_12 1.772 -0.014 -1.372 -2.317 2.1365
2008 HC F 01_02 1.656 -0.014 -1.372 -2.317 2.1365
2070 AE F 05_06 1.657 0.113 -1.372 -0.140000000000001 -5.5335
2070 AE F 07_08 1.579 0.113 -1.372 -0.140000000000001 -5.5335
2070 AE F 09_10 1.75 0.113 -1.372 -0.140000000000001 -5.5335
2070 AE F 03_04 1.808 0.113 -1.372 -0.140000000000001 -5.5335
2070 AE F 11_12 1.777 0.113 -1.372 -0.140000000000001 -5.5335
2070 AE F 01_02 1.702 0.113 -1.372 -0.140000000000001 -5.5335
2064 AE F 11_12 1.781 -0.027 -5.512 3.57 -3.5335
2064 AE F 09_10 1.724 -0.027 -5.512 3.57 -3.5335
2064 AE F 01_02 1.631 -0.027 -5.512 3.57 -3.5335
2064 AE F 03_04 1.607 -0.027 -5.512 3.57 -3.5335
2064 AE F 05_06 1.577 -0.027 -5.512 3.57 -3.5335
2064 AE F 07_08 1.546 -0.027 -5.512 3.57 -3.5335
2039 HC M 09_10 1.879 -0.014 2.628 -1.867 -5.5335
2039 HC M 11_12 1.918 -0.014 2.628 -1.867 -5.5335
2039 HC M 03_04 1.794 -0.014 2.628 -1.867 -5.5335
2039 HC M 05_06 1.787 -0.014 2.628 -1.867 -5.5335
2039 HC M 07_08 1.687 -0.014 2.628 -1.867 -5.5335
2039 HC M 01_02 1.774 -0.014 2.628 -1.867 -5.5335
2117 HC F 09_10 1.712 -0.014 0.917999999999999 1.293 3.7965
2117 HC F 11_12 1.75 -0.014 0.917999999999999 1.293 3.7965
2117 HC F 03_04 1.717 -0.014 0.917999999999999 1.293 3.7965
2117 HC F 07_08 1.587 -0.014 0.917999999999999 1.293 3.7965
2117 HC F 05_06 1.667 -0.014 0.917999999999999 1.293 3.7965
2117 HC F 01_02 1.663 -0.014 0.917999999999999 1.293 3.7965
")
  dat$ID <- as.factor(dat$ID)
  fm <- suppressWarnings(aov_car(Value ~ Propdd00 + Group + Gender + GAS0 + MAD0 + CPD0 + Error(ID/ROI), data=dat, factorize=FALSE, return = "Anova"))
  fm0 <- suppressWarnings(aov_car(Value ~ MAD0 + CPD0 + Error(ID/ROI), data=dat, factorize=FALSE, return='afex_aov'))
  expect_is(fm, "Anova.mlm")
  expect_is(fm0, "afex_aov")
})


test_that("ANOVA: ids in multiple between.subjects conditions", {
  species<- c("a","b","c","c","b","c","b","b","a","b","c","c","a","a","b","b","a","a","b","c")
  habitat<-  c("x","x","x","y","y","y","x","x","y","z","y","y","z","z","x","x","y","y","z","z")
  mvt.rate<-c(6,5,7,8,9,4,3,5,6,9,3,6,6,7,8,9,5,6,7,8)
  ind<-as.factor(c(1,2,3,4,1,2,3,4,1,2,3,4,1,2,3,4,1,2,3,4))
  data1<-data.frame(species, habitat, mvt.rate, ind)
  # should give an error
  expect_error(aov_ez("ind", "mvt.rate", data1, within = "habitat", between = "species"), "Following ids are in more than one between subjects condition:")
})

test_that("empty factors are not causing aov.cat to choke", {
  data(sleepstudy) #Example data in lme4
  sleepstudy$Days<-factor(sleepstudy$Days)
  #Works with all factors
  expect_is(aov_ez("Subject","Reaction",sleepstudy, within="Days", return = "Anova"), "Anova.mlm")
  #If you remove a factor it fails...
  expect_is(aov_ez("Subject","Reaction",sleepstudy[sleepstudy$Days!=9,], within="Days", return = "Anova"), "Anova.mlm")
})

test_that("factors have more than one level", {
  data(obk.long)
  expect_error(aov_car(value ~ treatment+ Error(id/phase), data = obk.long[ obk.long$treatment == "control",]), "one level only.")
  expect_error(aov_car(value ~ treatment+ Error(id/phase), data = obk.long[ obk.long$phase == "pre",]), "one level only.")
})


test_that("variable names longer", {
  data(obk.long)
  obk.long$gender2 <- obk.long$treatment
  orig <- aov_car(value ~ treatment * gender + age + Error(id/phase*hour), data = obk.long, factorize=FALSE, observed = "gender")
  v1 <- aov_car(value ~ gender2 * gender + age + Error(id/phase*hour), data = obk.long, factorize=FALSE, observed = "gender")
  v2 <- aov_car(value ~ gender2 * gender + age + Error(id/phase*hour), data = obk.long, factorize=FALSE, observed = "gender2")
  expect_equivalent(orig$anova_table, v1$anova_table)
  expect_identical(nice(orig)[,-1], nice(v1)[,-1])
  expect_identical(nice(orig)[,c("df", "MSE", "F", "p.value")], nice(v2)[,c("df", "MSE", "F", "p.value")])
  expect_equivalent(orig$anova_table[,c("num Df", "den Df", "MSE", "F", "Pr(>F)")], v2$anova_table[c("num Df", "den Df", "MSE", "F", "Pr(>F)")])
})

test_that("works with dplyr data.frames (see https://github.com/singmann/afex/issues/6):", {
  if (getRversion() >= "3.1.2") {
    require(dplyr)
    data(md_12.1)
    md2 <- tbl_df(md_12.1)
    expect_is(aov_ez("id", "rt", md2, within = c("angle", "noise"), anova_table=list(correction = "none", es = "none")), "afex_aov") 
  }
})
