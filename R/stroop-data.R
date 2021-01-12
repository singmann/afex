#' Stroop data from Lin et al. (2020, Psych. Science)
#'
#' 
#' Lin, Saunders, Friese, Evans, and Inzlicht (2020) investigated ego depletion.
#' An initial high-demand task was followed by a Stroop task. The data of the
#' Stroop task from all 4 of their studies is included here.
#'
#' Their abstract: People feel tired or depleted after exerting mental effort.
#' But even preregistered studies often fail to find effects of exerting effort
#' on behavioral performance in the laboratory or elucidate the underlying
#' psychology. We tested a new paradigm in four preregistered within-subjects
#' studies (N = 686). An initial high-demand task reliably elicited very strong
#' effort phenomenology compared with a low-demand task. Afterward, participants
#' completed a Stroop task. We used drift-diffusion modeling to obtain the
#' boundary (response caution) and drift-rate (information-processing speed)
#' parameters. Bayesian analyses indicated that the high-demand manipulation
#' reduced boundary but not drift rate. Increased effort sensations further
#' predicted reduced boundary. However, our demand manipulation did not affect
#' subsequent inhibition, as assessed with traditional Stroop behavioral
#' measures and additional diffusion-model analyses for conflict tasks. Thus,
#' effort exertion reduced response caution rather than inhibitory control,
#' suggesting that after exerting effort, people disengage and become
#' uninterested in exerting further effort.
#' 
#'
#'
#' @docType data
#' @keywords dataset
#' @name stroop
#' @usage stroop
#' @format A data frame with 246600 rows and 7 variables:
#' \describe{
#'   \item{pno}{participant id (preceded by study id), factor with 685 levels}
#'   \item{condition}{experimental condition (control/low demand, deplete/high demand), factor with 2 levels}
#'   \item{study}{study number (1, 2, 3, 4), factor with 4 levels}
#'   \item{trialnum}{trial number}
#'   \item{congruency}{Stroop congruency (congruent, incongruent), factor with 2 levels}
#'   \item{acc}{accuracy (0: error, 1: correct)}
#'   \item{rt}{reaction time (seconds)}
#' }
#' @source Lin, H., Saunders, B., Friese, M., Evans, N. J., & Inzlicht, M.
#'   (2020). Strong Effort Manipulations Reduce Response Caution: A
#'   Preregistered Reinvention of the Ego-Depletion Paradigm. *Psychological
#'   Science*, \doi{10.1177/0956797620904990}
#'
#' @encoding UTF-8
#'
"stroop"



