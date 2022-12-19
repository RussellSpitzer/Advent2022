package com.russell.advent.util

object Ranges {
  /** Finds the left most contiguous range in a set of ranges
   **/
  def findContiguous(unorderedRanges: Seq[Range.Inclusive]): Seq[Range.Inclusive] = {
    val ranges = unorderedRanges.sortBy(_.start)
    ranges.foldLeft(Seq(): Seq[Range.Inclusive]) { (combinedRanges, range: Range.Inclusive) =>
      if (combinedRanges.isEmpty) {
        combinedRanges :+ range
      } else {
        val lastCombined = combinedRanges.last
        if (lastCombined.contains(range.start) && lastCombined.end < range.end) {
          combinedRanges.dropRight(1) :+ (lastCombined.start to range.end)
        } else {
          combinedRanges
        }
      }
    }
  }
}
