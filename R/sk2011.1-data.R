#' Data from Singmann & Klauer (2011, Experiment 1)
#'
#' Singmann and Klauer (2011) were interested in whether or not conditional reasoning can be explained by a single process or whether multiple processes are necessary to explain it. To provide evidence for multiple processes we aimed to establish a double dissociation of two variables: instruction type and problem type. Instruction type was manipulated between-subjects, one group of participants received  deductive instructions (i.e., to treat the premises as given and only draw necessary conclusions) and a second group of participants received probabilistic instructions (i.e., to reason as in an everyday situation; we called this "inductive instruction" in the manuscript). Problem type consisted of two different orthogonally crossed variables that were manipulated within-subjects, validity of the problem (formally valid or formally invalid) and plausibility of the problem (inferences which were consisted with the background knowledge versus problems that were inconsistent with the background knowledge). The critical comparison across the two conditions was among problems which were valid and implausible with problems that were invalid and plausible. For example, the next problem was invalid and plausible:
#'
#' If a person is wet, then the person fell into a swimming pool.  \cr
#' A person fell into a swimming pool.  \cr
#' How valid is the conclusion/How likely is it that the person is wet?
#'
#' For those problems we predicted that under deductive instructions responses should be lower (as the conclusion does not necessarily follow from the premises) as under probabilistic instructions. For the valid but implausible problem, an example is presented next, we predicted the opposite pattern:
#'
#' If a person is wet, then the person fell into a swimming pool.    \cr
#' A person is wet.  \cr
#' How valid is the conclusion/How likely is it that the person fell into a swimming pool?
#'
#' Our study also included valid and plausible and invalid and implausible problems. 
#'
#' Note that the factor `plausibility` is not present in the original manuscript, there it is a results of a combination of other factors.
#'
#'
#' @docType data
#' @keywords dataset
#' @name sk2011.1
#' @usage sk2011.1
#' @format A data.frame with 640 rows and 9 variables.
#' @source Singmann, H., & Klauer, K. C. (2011). Deductive and inductive conditional inferences: Two modes of reasoning. Thinking & Reasoning, 17(3), 247-281. doi:10.1080/13546783.2011.572718

#'
#' @encoding UTF-8
#'
#' @examples
#' data(sk2011.1)
#' 
#' # Table 1 (p. 264):
#' aov_ez("id", "response", sk2011.1[ sk2011.1$what == "affirmation",], 
#'        within = c("inference", "type"), between = "instruction", 
#'        args.return=(es = "pes"))
#' aov_ez("id", "response", sk2011.1[ sk2011.1$what == "denial",], 
#'        within = c("inference", "type"), between = "instruction", 
#'        args.return=(es = "pes"))
#' 
#'  
NULL



