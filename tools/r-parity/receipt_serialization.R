# Shared numerical serialization policy for candidate-bound R receipts.

RECEIPT_SIGNIFICANT_DIGITS <- 13L
RECEIPT_ZERO_THRESHOLD <- 1e-14

canonicalize_receipt_numbers <- function(value) {
  if (is.data.frame(value)) {
    for (name in names(value)) {
      value[[name]] <- canonicalize_receipt_numbers(value[[name]])
    }
    return(value)
  }
  if (is.list(value)) {
    return(lapply(value, canonicalize_receipt_numbers))
  }
  if (is.double(value)) {
    value <- signif(value, digits = RECEIPT_SIGNIFICANT_DIGITS)
    value[is.finite(value) & abs(value) < RECEIPT_ZERO_THRESHOLD] <- 0
  }
  value
}

receipt_format_number <- function(value) {
  value <- canonicalize_receipt_numbers(as.numeric(value))
  rendered <- sprintf(paste0("%.", RECEIPT_SIGNIFICANT_DIGITS, "g"), value)
  rendered[value == 0] <- "0.0"
  rendered
}

receipt_serialization_convention <- function() {
  paste0(
    RECEIPT_SIGNIFICANT_DIGITS,
    " significant decimal digits; finite absolute values below ",
    format(RECEIPT_ZERO_THRESHOLD, scientific = TRUE),
    " serialize as zero"
  )
}
