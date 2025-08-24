#' Data 16.4 from Maxwell & Delaney
#'
#' Data from a hypothetical inductive reasoning study.
#'
#'
#' Description from pp. 841:
#'
#' Suppose an educational psychologist has developed an intervention to teach inductive reasoning skills to school children. She decides to test the efficacy of her intervention by conducting a randomized design. Three classrooms of students are randomly assigned to the treatment condition, and 3 other classrooms are assigned to the control.
#'
#' Table 16.4 shows hypothetical data collected from 29 children who participated in the study assessing the effectiveness of the intervention to increase inductive reasoning skills. We want to call your attention to several aspects of the data. First, the 15 children with condition values of 0 received the control, whereas the 14 children with condition values of 1 received the treatment. Second, 4 of the children in the control condition were students in control Classroom 1, 6 of them were students in control Classroom 2, and 5 were students in control Classroom 3. Along similar lines, 3 of the children in the treatment condition were students in treatment Classroom 1, 5 were students in treatment Classroom 2, and 6 were students in treatment Classroom 3. It is essential to understand that there are a total of six classrooms here; we have coded classroom from 1 to 3 for control as well as treatment, because we will indicate to PROC MIXED that classroom is nested under treatment. Third, scores on the dependent variable appear in the rightmost column under the variable label "induct."
#'
#' Note that it would make a lot more sense to change the labeling of room from 1 to 3 nested within cond to 1 to 6. However, I keep this in line with the original. The random effects term in the call to mixed is therefore a little bit uncommon.#'
#'
#' @docType data
#' @keywords dataset
#' @name md_16.4
#' @usage md_16.4
#' @format A data.frame with 24 rows and 3 variables.
#' @source Maxwell, S. E., & Delaney, H. D. (2004). Designing experiments and analyzing data: a model-comparisons perspective. Mahwah, N.J.: Lawrence Erlbaum Associates. p. 574
#'
#' @encoding UTF-8
#'
#' @examples
#' # data for next examples (Maxwell & Delaney, Table 16.4)
#' data(md_16.4)
#' str(md_16.4)
#'
#' ### replicate results from Table 16.6 (Maxwell & Delaney, 2004, p. 845)
#' # p-values (almost) hold:
#' (mixed2 <- mixed(induct ~ cond + (1|room:cond), md_16.4))
#' # (1|room:cond) is needed because room is nested within cond.
#'
#'
NULL
