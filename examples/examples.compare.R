
with(sleep, compare.2.vectors(extra[group == 1], extra[group == 2]))

# gives:
## $parametric
##    test test.statistic test.value test.df       p
## 1     t              t     -1.861   18.00 0.07919
## 2 Welch              t     -1.861   17.78 0.07939
## 
## $nonparametric
##              test test.statistic test.value test.df       p
## 1 stats::Wilcoxon              W     25.500      NA 0.06933
## 2     permutation              Z     -1.751      NA 0.08154
## 3  coin::Wilcoxon              Z     -1.854      NA 0.06487
## 4          median              Z      1.744      NA 0.17867

# compare with:
with(sleep, compare.2.vectors(extra[group == 1], extra[group == 2], alternative = "less"))

with(sleep, compare.2.vectors(extra[group == 1], extra[group == 2], alternative = "greater"))

# doesn't make much sense as the data is not paired, but whatever:
with(sleep, compare.2.vectors(extra[group == 1], extra[group == 2], paired = TRUE))

# from ?t.test:
compare.2.vectors(1:10,y=c(7:20, 200))

