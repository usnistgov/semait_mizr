% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mizr_plots.R
\name{mizr_det_plot}
\alias{mizr_det_plot}
\title{Produce a Detection Error Tradeoff (DET) Curve Plot}
\usage{
mizr_det_plot(
  df,
  score_col,
  key_col,
  key_values = c(0, 1),
  dec_col = NULL,
  cp_miss = 1,
  cp_fa = 1,
  pp_hkv = 0.5,
  cs_miss = 1,
  cs_fa = 1,
  ps_hkv = 0.9,
  sys_name = ""
)
}
\arguments{
\item{df}{the data frame of scores and decisions to produce the DET curve for}

\item{score_col}{the column containing the system scores.}

\item{key_col}{the column of the data frame that contains the key (ground truth)}

\item{key_values}{a two-element vector with (low_key_value, high_key_value), where
the high_key_value is interpreted as the target or the positive trait.}

\item{dec_col}{the column containing the system's actual decisions. Set to NULL if there are
no decisions provided. Defaults to NULL.}

\item{cp_miss}{the cost of a miss for the primary cost function. Defaults to 1.}

\item{cp_fa}{the cost of a false alarm for the primary cost function. Defaults to 1.}

\item{pp_hkv}{The prior probability of the high_key_value for the primary cost function,
which is the prior probability of
the target class or positive class. Defaults to 0.5, meaning that with the defaults misses
and false alarms are equally weighted.}

\item{cs_miss}{the cost of a miss for the secondary cost function. Defaults to 1.}

\item{cs_fa}{the cost of a false alarm for the secondary cost function. Defaults to 1.}

\item{ps_hkv}{The prior probability of the high_key_value for the secondary cost function,
which is the prior probability of
the target class or positive class. Defaults to 0.9, meaning that with the defaults misses
and false alarms are qually weighted.}

\item{sys_name}{the name of the system. Defaults to "".}
}
\value{
mizr_plot a plot object
}
\description{
This produces a Detection Error Tradeoff (DET) Curve following the work of:
}
\details{
Martin, Alvin F., George R. Doddington, Terri Kamm, Mark Ordowski, and Mark A. Przybocki.
1997. “The DET Curve in Assessment of Detection Task Performance.”
In Fifth European Conference on Speech Communication and Technology, EUROSPEECH 1997.
Rhodes, Greece. http://www.isca-speech.org/archive/eurospeech_1997/e97_1895.html.

This produces a DET curve for a single system, and has options to support actual decisions
in addition to minimum scores, and allows for parameterizable costs.
Parameterization is supported both in costs of a miss
and false alarm as well as the prior probability of a target, or "high_key_value".

The DET curve supports two decision cost functions: a primary cost function (p) and a
secondary cost function (s). This allows for scenarios such as a forensic and investigative
analysis that provides a system's performance according to two cost functions
}
