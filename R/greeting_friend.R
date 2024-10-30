#' Generate an animal with R version info
#'
#' @param animal should be either 'octopus' or 'bear' to define what animal
#' will be generated.
#' @param include_angry Should the angry expression be included as an option?
#' @param color_funs Optional, a list of `crayon::*` functions to use
#' for the octopus' coloring (see examples).
#' @param startup Defines whether or not R version information will be included.
#'
#' @export
#'
#' @importFrom glue glue
#' @importFrom crayon white cyan blue magenta green
#'
#' @examples
#' # Disable the angry expression w/ `include_angry = FALSE`:
#' greeting_animal(animal = "octopus", include_angry = FALSE)
#'
#' # Override the default color options w/ `color_funs`:
#' greeting_animal(animal = "octopus", color_funs = list(crayon::yellow, crayon::magenta))
#' greeting_animal(animal = "bear", color_funs = list(crayon::green))
#'
#' # Disable the R version information w/ `startup = FALSE`:
#' greeting_animal(animal = "bear", startup = FALSE)
#'
greeting_friend <- function(animal = NULL, include_angry = TRUE, color_funs = NULL,
                            startup = TRUE) {

  if (is.null(animal)) {
    animal <- sample(c("octopus", "bear", "dog", "fish"), 1)
  }
  # Initialize art, split by newlines ---------------------------------------
  if (animal == "octopus") {
    animal_ascii <- "                        ___\n                     .-'   `'.\n                    /         \\\n                    |         ;\n                    |         |           ___.--,\n           _.._     |0) ~ (0) |    _.---'`__.-( (_.\n    __.--'`_.. '.__.\\    '--. \\_.-' ,.--'`     `\"\"`\n   ( ,.--'`   ',__ /./;   ;, '.__.'`    __\n   _`) )  .---.__.' / |   |\\   \\__..--\"\"  \"\"\"--.,_\n  `---' .'.''-._.-'`_./  /\\ '.  \\ _.-~~~````~~~-._`-.__.'\n        | |  .' _.-' |  |  \\  \\  '.               `~---`\n         \\ \\/ .'     \\  \\   '. '-._)\n          \\/ /        \\  \\    `=.__`~-.\n          / /\\         `) )    / / `\"\".`\\\n    , _.-'.'\\ \\        / /    ( (     / /\n     `--~`   ) )    .-'.'      '.'.  | (\n            (/`    ( (`          ) )  '-;\n             `      '-;         (-'"
  } else if (animal == "bear") {
    animal_ascii <- "     .--.              .--.\n    : (\\ \". _......_ .\" /) :\n     '.    `        `    .'\n      /'    _        _   `\\\\\n     /     (0)      (0)    \\\\\n    |        /      \\       |\n    |      /'        `\\     |\n     \\    | .  .==.  . |   /\n      '._  \\.' \\__/ './ _.'\n       /  ``'._-''-_.'``  \\\\"
  } else if (animal == "dog") {
    animal_ascii <- "\n                             ;\\\n                            |\' \\\n         _                  ; : ;\n        / \`-.              /: : |\n       |  ,-.\`-.          ,\': : |\n       \\  :  \`. \`.       ,\'-. : |\n        \\ ;    ;  \`-.__,\'    \`-.|\n         \\ ;   ;  :::  ,::\'\`:.  \`.\n          \\ \`-. :  \`    :.    \`.  \\\n           \\   \    ,   ;    ,:    (\\\n            \\   :., :.    ,\'o)): \` \`-.\n           ,/,\' ;\' ,::\"\'\`.\`---\'   \`.  \`-._\n         ,/  :  ; \'\"      \`;\'           ,--\`.\n        ;/   :; ;             ,:\'     (   ,:)\n          ,.,:.    ; ,:.,  ,-._ \`.     \\\"\"\'/\n         \'::\'     \`:\'\`  ,\'(  \\`._____.-\'\"\'\n             ;,   ;  \`.  \`. \`._\`-.  \\\n             ;:.  ;:       \`-._\`-.\\  \\`.\n              \'\`:. :        |\' \`. \`\\ ) \\\n                 \` ;:       |    \`--\\__,\'\n                   \'\`      ,\'\n                        ,-\'\n"
  } else if (animal == "fish") {
    animal_ascii <- "\n~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n        .                \`         /\n                          .    ,../...       .\n           .               .  /       \`\\  /  .\n     \\    .        o         < \'  )     =<\n     /\\  .                    \\ \\      /  \\   .  __\n   >=)\'>                       \`\'\\\'\"\'\"\'         /o \\/\n     \\/ .    /         o              /,        \\__/\\    .:/\n     /   .  /--\\ /         /         <\')=<     .      ,,///;,   ,;/\n           <o)  =<      . / \\         \\\`         .   o:::::::;;///\n            \\__/ \\       <\')_=<                     >::::::::;;\\\\\\\n             \\            \\_/            .            \'\'\\\\\\\\\\\'\' \';\\\n    (                      \\              .   __\n     )                                       <\'_><          (\n    (          (                ,/..          \`              )\n     )     (    )             <\')   \`=<                )    (\n    (       )  (               \`\`\\\`\`\`                 (      )\n_____)_____(____)______________________________________)____(__________      \n"
  } else {
    stop("animal not defined")
  }

  if(startup) {
    # Add R version and nickname if startup is true
    version_string <- paste0(version$version.string, " -- ", "\"", version$nickname, "\"")
    animal_ascii <- paste(version_string, animal_ascii, sep = "\n")
  }

  animal_ascii <- strsplit(animal_ascii, "\\n")[[1]]


  # Adjustments to ASCII art ------------------------------------------------

  # wink
  if (stats::runif(1) < 1/5) {
    animal_ascii <- sub("(?<=\\()0", "-", animal_ascii, perl = TRUE)
    animal_ascii <- sub("~", " ", animal_ascii)

    # blink
    if (stats::runif(1) < 1/3) {
      animal_ascii <- sub("0", "-", animal_ascii)

      # sleep
      if (stats::runif(1) < 1/2) {
        animal_ascii[2] <- sub("^                  ", "{crayon::white('              Z  Z')}", animal_ascii[2])
        animal_ascii[3] <- sub("^                  ", "{crayon::white('             Z   Z')}", animal_ascii[3])
        animal_ascii[4] <- sub("^                  ", "{crayon::white('            Z Z   ')}", animal_ascii[4])
        animal_ascii[5] <- sub("^                  ", "{crayon::white('             Z  Z ')}", animal_ascii[5])
      }
    }

  } else {

    # angry
    if (include_angry & stats::runif(1) < 1/5) {
      animal_ascii <- gsub("0", '{crayon::bold(crayon::red("0"))}', animal_ascii)
      animal_ascii <- gsub("~", "v", animal_ascii)

    }

  }

  # glue::glue() for various expressions
  animal_ascii <- vapply(animal_ascii, glue::glue, FUN.VALUE = character(1))


  # Assign colors -----------------------------------------------------------

  color_options <- c(
    "solid",
    "stripe",
    "rainbow"
  )

  if (is.null(color_funs)) {

    # default options for color_funs:
    color_funs <- list(
      crayon::cyan,
      crayon::blue,
      crayon::magenta,
      crayon::green
    )

  } else {

    # need to check user-supplied color_funs
    if (!is.list(color_funs) || any(!vapply(color_funs, is.function, logical(1)))) {
      stop("color_funs should be a list of functions from the crayon package \n  e.g. color_funs = list(crayon::blue, crayon::black, crayon::magenta)")
    }

  }


  # If there are more than 1 options of color_funs available,
  # choose pattern from color_options
  if (length(color_funs) == 1) {

    color_funs <- color_funs[[1]]

  } else {

    color_funs <-switch(
      sample(color_options, 1),
      "solid" = color_funs[[sample(1:length(color_funs), 1)]],
      "stripe" = color_funs[sample(1:length(color_funs), 2)],
      "rainbow" = color_funs
    )

  }


  # function to apply to each element ("row") of animal_ascii
  color_ascii <- function(string) {

    if (length(color_funs) == 1) {
      color_fun <- color_funs
    } else {
      color_fun <- color_funs[[sample(1:length(color_funs), 1)]]
    }

    color_fun(string)
  }

  # Collapse back to vector of length 1, w/ newlines
  animal_ascii <- lapply(animal_ascii, color_ascii) |>
    paste(collapse = "\n")



  # cat, returning string invisibly -----------------------------------------
  cat(animal_ascii, "\n", "\n")
  invisible(animal_ascii)

}








