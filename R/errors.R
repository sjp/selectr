# All user-facing selectr failures share one condition hierarchy so
# callers can catch the family without matching on message text, and
# narrow to a specific shape when they need one:
#
#   selectr_parse_error       malformed CSS (fields: selector, pos)
#   selectr_translation_error valid but unsupported CSS (fields:
#                              selector, feature)
#   selectr_argument_error    a bad R-level argument (no extra fields)
#
# Every one of them also inherits "selectr_error", "error", "condition".
selectr_abort <- function(message, class, ...) {
    stop(errorCondition(message, class = c(class, "selectr_error"), ...,
                        call = NULL))
}
