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
require(qlcData, quietly = TRUE) # currently needs github version, not the one from CRAN

# =====
# usage
# =====

DOC <- "
USAGE:
  pass_align [-h -s SEPARATOR -i IN_GAP -o OUT_GAP] <ORIGINALS> [<ALIGNMENTS>]

DESCRIPTION:
  Using the function pass_align() from the R-package qlcData
  to transfer alignments from an alignment string to an original string. 
  Not yet on CRAN, so install package from https://github.com/cysouw/qlcData

  Originals and Alignments should be files with each string on a new line.
  Alignments can be piped through from another executable.

OPTIONS:
  -h, --help      Showing this help text
  -s SEPARATOR    Separator, defaults to nothing, 
                    use 'S' to get space [default: ]
  -i IN_GAP       Gap symbol in the alignments [default: -]
  -o OUT_GAP      Gap symbol in the output, useful when the gap symbol
                    from the alignments actually occurs as a character
                    in the originals [default: -]

EXAMPLES:
  $$$ pass_align.R test X---XXX
  
  Results in: 't---est'
"

# ==============
# docopt parsing
# ==============

attach(docopt::docopt(DOC))

# for piping data
if (length(ALIGNMENTS) == 0) {
	ALIGNMENTS <- scan(file("stdin") , sep = "\n" , quiet = TRUE , what = "character")
	closeAllConnections() 
}

# space cannot be passed as argument in bash
if (s == "S") {s <- " "}

# ======
# R code
# ======

result <- qlcData::pass_align(ORIGINALS, ALIGNMENTS, sep = s, in.gap = i, out.gap = o)

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
	