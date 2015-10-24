#' Data 12.1 from Maxwell & Delaney
#'
#' Hypothetical Reaction Time Data for 2 x 3 Perceptual Experiment: Example data for chapter 12 of Maaxwell and Delaney (2004, Table 12.1, p. 574) in long format. Has two within.subjects factors: angle and noise.
#' 
#' Description from pp. 573:
#' 
#' Suppose that a perceptual psychologist studying the visual system was interested in determining the
#' extent to which interfering visual stimuli slow the ability to recognize letters. Subjects are
#' brought into a laboratory and seated in front of a tachistoscope. Subjects are told that they will 
#' see either the letter T or the letter I displayed on the screen. In some trials, the letter appears 
#' by itself, but in other trials, the target letter is embedded in a group of other letters. This 
#' variation in the display constitutes the first factor, which is referred to as noise. The noise 
#' factor has two levels?absent and present. The other factor varied by the experimenter is where in 
#' the display the target letter appears. This factor, which is called angle, has three levels. The 
#' target letter is either shown at the center of the screen (i.e., 0° off-center, where the subject 
#' has been instructed to fixate), 4° off-center or 8° off-center (in each case, the deviation from the 
#' center varies randomly between left and right). Table 12.1 presents hypothetical data for 10 
#' subjects. As usual, the sample size is kept small to make the calculations easier to follow. The 
#' dependent measure is reaction time (latency), measured in milliseconds (ms), required by a subject 
#' to identify the correct target letter. Notice that each subject has six scores, one for each 
#' combination of the 2 x 3 design. In an actual perceptual experiment, each of these six scores would 
#' itself be the mean score for that subject across a number of trials in the particular condition. 
#' Although "trials" could be used as a third within-subjects factor in such a situation, more 
#' typically trials are simply averaged over to obtain a more stable measure of the individual's 
#' performance in each condition.
#'
#' @docType data
#' @keywords dataset
#' @name md_12.1
#' @usage md_12.1
#' @format A data.frame with 60 rows and 4 variables.
#' @source Maxwell, S. E., & Delaney, H. D. (2004). Designing experiments and analyzing data: a model-comparisons perspective. Mahwah, N.J.: Lawrence Erlbaum Associates. p. 574

#'
#' @encoding UTF-8
#'
#' @examples
#' data(md_12.1)
#' 
#' # Table 12.5 (p. 578):
#' aov_ez("id", "rt", md_12.1, within = c("angle", "noise"), 
#'        args.return=list(correction = "none", es = "none"))
#' 
#' 
NULL
