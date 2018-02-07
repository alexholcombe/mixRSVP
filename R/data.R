#' Dual-RSVP data, with letters having random orientation on each frame
#'
#' A dataset of what letter three different undergrads reported
#' on each of 400 different dual-stream RSVP trials.
#'
#' @format A data frame with 1200 rows and 23 variables:
#' \describe{
#'   \item{target}{which target, the one in the left stream (1) or the right stream (2), this row pertains to}
#'   \item{SPE}{the serial position error}
#'   \item{allLetterOrientations}{}
#'   \item{allRTs}{}
#'   \item{allResponseOrientations}{}
#'   \item{allTargetOrientations}{}
#'   \item{condition}{}
#'   \item{letterSeq}{}
#'   \item{resp}{}
#'   \item{respSP}{}
#'   \item{subject}{}
#'   \item{targetSP}{}
#'   \item{trial}{}
#'   ...
#' }
#' @source Alex's GoogleDrive folder, "/Users/alexh/Google\ Drive/Backwards\ paper/secondPaper/E2/Data/Raw", first 3 Ss run.
"P2E2pilot"

#' Dual-RSVP performance, with letters having random orientation on each frame
#'
#' A dataset of what letter three different undergrads reported
#' on each of 400 different dual-stream RSVP trials.
#'
#' @format A data frame with 1200 rows and 23 variables:
#' \describe{
#'   \item{target}{which target, the one in the left stream (1) or the right stream (2), this row pertains to}
#'   \item{SPE}{the serial position error}
#'   \item{allLetterOrientations}{}
#'   \item{allRTs}{}
#'   \item{allResponseOrientations}{}
#'   \item{allTargetOrientations}{}
#'   \item{condition}{}
#'   \item{letterSeq}{}
#'   \item{resp}{}
#'   \item{respSP}{}
#'   \item{subject}{}
#'   \item{targetSP}{}
#'   \item{trial}{}
#'   ...
#' }
#' @source Alex's GoogleDrive folder, "/Users/alexh/Google\ Drive/Backwards\ paper/secondPaper/E2/Data/Raw", first 3 Ss run.
"P2E2pilot"





#' Dual-RSVP data, with letters having random orientation on each trial
#'
#' A dataset of what letter thirty different undergrads (subject) reported
#' in 200 different dual-stream RSVP trials.
#'
#' @format A data frame with 1200 rows and 10 variables:
#' \describe{
#'   \item{target}{which target, the one in the left stream (1) or the right stream (2), this row pertains to}
#'   \item{SPE}{the serial position error}
#'   \item{allRTs}{response time}
#'   \item{condition}{1 is upright, 2 is inverted?}
#'   \item{letterSeq}{the (ordinal value of) each of the 24 letters presented in this stream}
#'   \item{resp}{the (ordinal value) of the letter reported}
#'   \item{respSP}{the serial position of the letter reported}
#'   \item{subject}{two-letter subject code}
#'   \item{targetSP}{the target serial position}
#'   \item{trial}{trial number}
#'   ...
#' }
#' @source Alex's GoogleDrive folder, "/Users/alexh/Google\ Drive/Backwards\ paper/secondPaper/E1/RawData/Data"
#' which is read in by backwardsLtrsLoadRawData.R which you can find here https://github.com/alexholcombe/MixtureModelRSVP/tree/master/importingData
"backwards2_E1"
