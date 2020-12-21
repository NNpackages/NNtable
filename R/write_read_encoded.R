
#' Write a file readeble by EOT
#'
#' @param text \code{character} where each element represent a line in the final
#'   output
#' @param file \code{character} indicating the full name of the output
#' @param enc \code{character} indicating the encoding to be used. The EOT tool
#'   expects this to be in CP1252
#'
#' @return The encoded string is returned invisible
#' @export
write_encoded <- function(text, file, enc = "CP1252") {

  # Encode as UTF-8 so that we know where we come from
  utf8 <- enc2utf8(text)

  # Re-encode as wanted. The EOT tool expects CP1252
  encoded <- iconv(utf8, "UTF-8", enc)

  # Create a connection writing in native encoding
  # This means that R wont translate into native encoding
  con <- file(file, open = "w+", encoding = "native.enc")

  # Ensure that the connection is closed even if writelines failes
  on.exit(close(con))

  # Write to the connection with 'useBytes = TRUE',
  # This means that R wont translate to the native encoding
  writeLines(encoded, con = con, useBytes = TRUE)
  return(invisible(encoded))
}

#' Read an encoded txt output
#'
#' @param file \code{character} indicating the file to read
#' @param enc \code{character} indicating the encoding to be used. The EOT tool
#'   expects this to be in CP1252
#'
#' @return character with the read lines
#' @export
read_encoded <- function(file, enc = "CP1252") {
  readLines(file, encoding = "CP1252")
}
