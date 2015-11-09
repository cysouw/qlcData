#!/usr/bin/env Rscript

# =================
# Copyright 2015 Michael Cysouw <cysouw@mac.com>
#
# This file is free software: you may copy, redistribute and/or modify it
# under the terms of the GNU General Public License as published by the
# Free Software Foundation, either version 3 of the License, or (at your
# option) any later version.
#
# This file is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
# =================

# ============
# dependencies
# ============

require(docopt, quietly = TRUE)
require(qlcData, quietly = TRUE)

# =====
# usage
# =====

DOC <- "
USAGE: 
  tokenize [-hlrd -s SEP -t TRANS <STRINGS> <PROFILE>]

DESCRIPTION:
  Using the function tokenize() from the R-package qlcData for tokenization and
  transliteration of character strings based on an orthography profile. Details
  http://www.rdocumentation.org/packages/qlcMatrix/functions/sim.strings.html

  STRINGS can be piped through. When no PROFILE is specified, default Unicode
  tokenization is performed. Note that the R-code allows for even more options,
  not all options are made available here for ease of use.
  Errors are likewise (not yet) returned

OPTIONS:
  -h, --help      Showing this help text
  -l, --linear    Use linear method instead of default global method
  -r, --regex     Use regex matching, including contexts in matching graphemes
  -d, --NFD       Use NFD normalization instead of default NFC
  -s SEP          Separator to be inserted after tokenization
                    defaults to space [default: S]
  -t TRANS        Column in profile to use for transliteration [default: NULL]
"

# ==============
# docopt parsing
# ==============

attach(docopt::docopt(DOC))

# for piping data
if (length(STRINGS) == 0) {
	STRINGS <- scan(file("stdin") , sep = "\n" , quiet = TRUE , what = "character")
	closeAllConnections() 
}

if (length(PROFILE) == 0) {
	PROFILE <- NULL
}

# catch options
if (linear) { method <- "linear" } else { method <- "global" }
if (regex) { regex <- TRUE } else { regex <- FALSE }
if (NFD) { normalize <- "NFD" } else { normalize <- "NFC"}
if (t == "NULL") { t <- NULL}

# space cannot be passed as argument in bash
if (s == "S") {s <- " "}

# ======
# R code
# ======

result <- qlcData::tokenize(STRINGS, PROFILE
						, method = method
						, regex = regex
						, normalize = normalize
						, sep = s
						, transliterate = t
						)

if (is.null(t)) {
	result <- result$strings$tokenized
} else {
	result <- result$strings$transliterated
}

# =======================
# Return output to stdout
# =======================

write.table(result
	, file = ""
	, sep = "\t"
	, row.names = FALSE
	, col.names = FALSE
	, quote = FALSE
	)
	