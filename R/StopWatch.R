#' @import R6

StopWatch <-
  R6Class(
    classname = "StopWatch",
    class = TRUE,
    cloneable = FALSE,
    public = list(
      Start = function(treatment) {
        private[paste0(treatment, "Start")] <- Sys.time()
      },
      Stop = function(treatment) {
        private[paste0(treatment, "Stop")] <- Sys.time()
      },
      Reset = function(treatment) {
        private[paste0(treatment, c("Start", "Stop"))]  <- numeric(0L)
      }, 
      GetData = function() {
        data.frame(
          ID = UUIDgenerate(n = 2L),
          Initials = private$.initials,
          Group = c("Control", "Stroop"),
          Sex = private$.sex,
          Value = c(as.numeric(self$ControlDuration, units = "secs"), 
                    as.numeric(self$StroopDuration, units = "secs"))
        )
      }
    ),
    active = list(
      StroopDuration = function(value) {
        private$.StroopStop - private$.StroopStart
      },
      ControlDuration = function(value) {
        private$.ControlStop - private$.ControlStart
      }
    ),
    private = list(
      .initials = character(0L),
      .StroopStart = numeric(0L),
      .StroopStop = numeric(0L),
      .ControlStart = numeric(0L),
      .ControlStop = numeric(0L)
    )
  )
