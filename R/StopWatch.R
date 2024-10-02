#' @import R6

StopWatch <-
  R6Class(
    classname = "StopWatch",
    class = TRUE,
    cloneable = FALSE,
    public = list(
      Start = function(treatment) {
        private[[paste0(".", treatment, "Start")]] <- Sys.time()
      },
      Stop = function(treatment) {
        private[[paste0(".", treatment, "Stop")]] <- Sys.time()
        
        if (!length(private$.order)) {
            if(treatment == "Control") {
              private$.order <- c("Control" = 1, "Stroop" = 2)
            } else {
              private$.order <- c("Control" = 2, "Stroop" = 1)
            }
        }
      },
      SetInitials = function(value) {
        private$.initials <- value
      },
      SetGroup = function(value) {
        private$.group <- value
      },
      Reset = function(fields = c(
        "initials",
        "group",
        "order",
        "StroopStart",
        "StroopStop",
        "ControlStart",
        "ControlStop"
      )) {
        lapply(fields, function(field) {
          private[[paste0(".", field)]] <- NULL
        })
      },
      GetData = function() {
        data.frame(
          ID = rep(UUIDgenerate(n = 1L), 2),
          Date = as.character(Sys.Date()),
          Initials = private$.initials,
          Group = private$.group,
          Order = private$.order,
          Test = c("Control", "Stroop"),
          Value = c(
            as.numeric(self$ControlDuration, units = "secs"),
            as.numeric(self$StroopDuration, units = "secs")
          )
        )
      }
    ),
    active = list(
      StroopDuration = function(value) {
        private$.StroopStop - private$.StroopStart
      },
      ControlDuration = function(value) {
        private$.ControlStop - private$.ControlStart
      },
      Time1 = function(value) {
        self[[paste0(names(private$.order[private$.order == 1]), "Duration")]]
      },
      Time2 = function(value) {
        self[[paste0(names(private$.order[private$.order == 2]), "Duration")]]
      }
    ),
    private = list(
      .initials = character(0L),
      .group = "Group A",
      .order = numeric(0L),
      .StroopStart = numeric(0L),
      .StroopStop = numeric(0L),
      .ControlStart = numeric(0L),
      .ControlStop = numeric(0L)
    )
  )
