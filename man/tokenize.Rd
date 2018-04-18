\name{tokenize}
\alias{tokenize}
\title{
Tokenization and transliteration of character strings based on an orthography profile
}
\description{
To process strings it is often very useful to tokenise them into graphemes (i.e. functional units of the orthography), and possibly replace those graphemes by other symbols to harmonize the orthographic representation of different orthographic representations (`transcription/transliteration'). As a quick and easy way to specify, save, and document the decisions taken for the tokenization, we propose using an orthography profile.

This function is the main function to produce, test and apply orthography profiles.
}
\usage{
tokenize(strings, 
  profile = NULL, transliterate = NULL,
  method = "global", ordering = c("size", "context", "reverse"),
  sep = " ", sep.replace = NULL, missing = "\u2047", normalize = "NFC",
  regex = FALSE, silent = FALSE,
  file.out = NULL)
}
\arguments{
  \item{strings}{
  Vector of strings to the tokenized. It is also possibly to pass a filename, which will then simply be read as \code{scan(strings, sep = "\n", what = "character")}.
}
  \item{profile}{
  Orthography profile specifying the graphemes for the tokenization, and possibly any replacements of the available graphemes. Can be a reference to a file or an R object. If NULL then the orthography profile will be created on the fly using the defaults of \code{write.profile}.
}
  \item{transliterate}{
  Default \code{NULL}, meaning no transliteration is to be performed. Alternatively, specify the name of the column in the orthography profile that should be used for replacement.
}
  \item{method}{
  Method to be used for parsing the strings into graphemes. Currently two options are implemented: \code{global} and \code{linear}. See Details for further explanation.
}
  \item{ordering}{
  Method for ordering. Currently three different methods are implemented, which can be combined (see Details below): \code{size}, \code{context}, \code{reverse} and \code{frequency}. Use \code{NULL} to prevent ordering and use the top to bottom order as specified in the orthography profile.
}
  \item{sep}{
  Separator to be inserted between graphemes. Defaults to space. This function assumes that the separator specified here does not occur in the data. If it does, unexpected things might happen. Consider removing the chosen seperator from your strings first, e.g. by using \code{\link[base]{gsub}} or use the option \code{sep.replace}.
}
  \item{sep.replace}{
  Sometimes, the chosen separator (see above) occurs in the strings to be parsed. This is technically not a problem, but the result might show unexpected sequences. When \code{sep.replace} is specified, this marking is inserted in the string at those places where the \code{sep} marker occurs. Typical usage in linguistics would be \code{sep = " ", sep.replace = "#"} adding spaces between graphemes and replacing spaces in the input string by hashes in the output string.
  }
  \item{missing}{
  Character to be inserted at transliteration when no transliteration is specified. Defaults to DOUBLE QUESTION MARK at U+2047. Change this when this character appears in the input string.
}
  \item{normalize}{
  Which normalization to use before tokenization, defaults to "NFC". Other option is "NFD". Any other input will result in no normalisation being performed.
}
  \item{regex}{
  Logical: when \code{regex = FALSE} internally the matching of graphemes is done exact, i.e. without using regular expressions. When \code{regex = TRUE} ICU-style regular expression (see \code{\link[stringi]{stringi-search-regex}}) are used for all content in the profile (including the Grapheme-column!), so any reserved characters have to be escaped in the orthography profile. Specifically, add a slash "\" before any occurrence of the characters \code{ [](){}|+*.-!?ˆ$\ } in your profile (except of course when these characters are used in their regular expression meaning).
  
Note that this parameter also influences whether contexts should be considered in the tokenization (internally, contextual searching uses regular expressions). By default, when \code{regex = FALSE}, context is ignored. If \code{regex = TRUE} then the function checks whether there are columns called \code{Left} (for the left context) and \code{Right} (for the right context), and optionally a column called \code{Class} (for the specification of grapheme-classes) in the orthography profile. These are hard-coded column-names, so please adapt your orthography profile accordingly. The columns \code{Left} and \code{Right} allow for regular expression to specify context.
}
%  \item{case.insensitive}{
%  By default, there is no case matching, so "A" and "a" are different graphemes. The option \code{case.incensitive = TRUE} applies a simple ICU "u_toupper" as made available in the \code{stringi} package. This should give the desired results in most cases, though for special orthographies the case mapping might need to be specified explicitly. In that case, consider writing a profile specifying the case mapping.
%}
  \item{silent}{
  Logical: by default missing characters in the strings are reported with a warning. use \code{silent = TRUE} to supress these warnings.
  }
  \item{file.out}{
  Filename for results to be written. No suffix should be specified, as various different files with different suffixes are produced (see Details below). When \code{file.out} is specified, then the data is written to disk AND the R dataframe is returned invisibly.
}
}
\details{
Given a set of graphemes, there are at least two different methods to tokenize strings. The first is called \code{global} here: this approach takes the first grapheme, matches this grapheme globally at all places in the string, and then turns to the next string. The other approach is called \code{linear} here: this approach walks through the string from left to right. At the first character it looks through all graphemes whether there is any match, and then walks further to the end of the match and starts again. In some special cases these two methods can lead to different results (see Examples).

The ordering or the lines in the ortography profile is of crucial importance, and different orderings will lead to radically different results. To simply use the top to bottom ordering as specified in the profile, use \code{order = NULL}. Currently, there are four different ordering strategies implemented: \code{size}, \code{context}, \code{reverse} and \code{frequency}. By specifying more than one in a vector, these orderings are used to break ties, e.g. the default specification \code{c("size", "context", "reverse")} will first order by size, and for those with the same size, it will order by whether any context is specifed (with context coming first). For lines that are still tied (i.e. the have the same size and all either have or have no context) the order will be reversed in comparison to the order as attested in the profile. Reversing order can be useful, because hand-written profiles tend to put general rules before specific rules, which mostly should be applied in reverse order.

\itemize{
\item \code{size}: order the lines in the profile by the size of the grapheme, largest first. Size is measured by number of Unicode characters after normalization as specified in the option \code{normalize}. For example, \code{é} has a size of 1 with \code{normalize = "NFC"}, but a size of 2 with \code{normalize = "NFD"}.

\item \code{context:} order the lines by whether they have any context specified, lines with context coming first. Note that this only works when the option \code{regex = TRUE} is also chosen.

\item \code{reverse:} order the lines from bottom to top.

\item \code{frequency}: order the lines by the frequency with which they match in the specified strings before tokenization, least frequent coming first. This frequency of course depends crucially on the available strings, so it will lead to different orderings when applied to different data. Also note that this frequency is (necessarily) measured before graphemes are identified, so these ordering frequencies are not the same as the final frequencies shown in the outpur. Frequency of course also strongly differs on whether context is used for the matching through \code{regex = TRUE}.
}
}
\value{
Without specificatino of \code{file.out}, the function \code{tokenize} will return a list of four:
\item{strings}{a dataframe with the original and the tokenized/transliterated strings}
\item{profile}{a dataframe with the graphemes with added frequencies. The dataframe is ordered according to the order that resulted from the specifications in \code{ordering}.}
\item{errors}{a dataframe with all original strings that contain unmatched parts.}
\item{missing}{a dataframe with the graphemes that are missing from the original orthography profilr, as indicated in the errors. Note that the report of missing characters does currently not lead to correct results for transliterated strings.}

When \code{file.out} is specified, these four tables will be written to three different tab-separated files (with header lines): \code{file_strings.tsv} for the strings, \code{file_profile.tsv} for the orthrography profile, \code{file_errors.tsv} for the strings that have unidentifyable parts, and \code{file_missing.tsv} for the graphemes that seem to be missing. When there is nothing missing, then no file for the missing strings is produced.
}
\references{
Moran & Cysouw (forthcoming)
}
\note{
When \code{regex = TRUE}, regular expressions are acceptable in the columns `Grapheme`, 'Left' and 'Right'. Backreferences in the transliteration column are not possible (yet). When regular expressions are allowed, all literal uses of special regex-characters have to be escaped! Any literal occurrence of the following characters has then to be preceded by a backslash \code{ \ }.

\itemize{
\item - (U+002D, HYPHEN-MINUS)
\item ! (U+0021, EXCLAMATION MARK)
\item ? (U+003F, QUESTION MARK)
\item . (U+002E, FULL STOP)
\item ( (U+0028, LEFT PARENTHESIS)
\item ) (U+0029, RIGHT PARENTHESIS)
\item \[ (U+005B, LEFT SQUARE BRACKET)
\item \] (U+005D, RIGHT SQUARE BRACKET)
\item \{ (U+007B, LEFT CURLY BRACKET)
\item \} (U+007D, RIGHT CURLY BRACKET)
\item | (007C, VERTICAL LINE)
\item * (U+002A, ASTERISK)
\item \ (U+005C, REVERSE SOLIDUS)
\item ˆ (U+005E, CIRCUMFLEX ACCENT)
\item + (U+002B, PLUS SIGN)
\item $ (U+0024, DOLLAR SIGN)
}

Note that overlapping matching does not (yet) work with regular expressions. That means that for example "aa" is only found once in "aaa". In some special cases this might lead to problems that might have to be explicitly specified in the profile, e.g. a grapheme "aa" with a left context "a". See examples below. This problem arises because overlap is only available in literal searches \code{\link[stringi]{stri_opts_fixed}}, but the current function uses regex-searching, which does not catch overlap \code{\link[stringi]{stri_opts_regex}}.
}
\note{
There is a bash-executable distributed with this package (based on the \code{docopt} package) that let you use this function directly in a bash-terminal. The easiest way to use this executable is to softlink the executable to some directory in your bash PATH, for example \code{/usr/local/bin} or simply \code{~/bin}. To softlink the function \code{tokenize} to this directory, use something like the following in your bash terminal:

\code{ln -is `Rscript -e 'cat(system.file("exec/tokenize", package="qlcData"))'` ~/bin}

From within R your can also use the following (again, optionally changing the linked-to directory from \code{~/bin} to anything more suitable on your system):

\code{file.symlink(system.file("exec/tokenize", package="qlcData"), "~/bin")}

}
\author{
Michael Cysouw <cysouw@mac.com>
}
\seealso{
See also \code{\link{write.profile}} for preparing a skeleton orthography profile.
}
\examples{
# simple example with interesting warning and error reporting
# the string might look like "AABB" but it isn't...
(string <- "\u0041\u0410\u0042\u0412")
tokenize(string,c("A","B"))

# make an ad-hoc orthography profile
profile <- cbind(
    Grapheme = c("a","ä","n","ng","ch","sch"), 
    Trans = c("a","e","n","N","x","sh"))
# tokenization
tokenize(c("nana", "änngschä", "ach"), profile)
# with replacements and a warning
tokenize(c("Naná", "änngschä", "ach"), profile, transliterate = "Trans")

# different results of ordering
tokenize("aaa", c("a","aa"), order = NULL)
tokenize("aaa", c("a","aa"), order = "size")

# regexmatching does not catch overlap, which can lead to wrong results
# the second example results in a warning instead of just parsing "ab bb"
# this should occur only rarely in natural language
tokenize("abbb", profile = c("ab","bb"), order = NULL)
tokenize("abbb", profile = c("ab","bb"), order = NULL, regex = TRUE)

# different parsing methods can lead to different results
# note that in natural language this is VERY unlikely to happen
tokenize("abc", c("bc","ab","a","c"), order = NULL, method = "global")$strings
tokenize("abc", c("bc","ab","a","c"), order = NULL, method = "linear")$strings
}

