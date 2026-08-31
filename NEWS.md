# quickcode 1.1.0

This is the first entry in `NEWS.md` for quickcode. It documents the state of
the package as of version 1.1.0, currently on CRAN.

## Highlights

* Comprehensive set of `not.*` validation helpers (`not.null`, `not.na`,
  `not.numeric`, `not.integer`, `not.logical`, `not.vector`, `not.data`,
  `not.environment`, `not.duplicated`, `not.image`, `not.inherits`,
  `not.exists`) alongside companions such as `has.error()`.
* PHP-style array/data operations: `vector_push()`, `vector_pop()`,
  `list_push()`, `data_push()`, `data_pop()`, `data_pop_filter()`,
  `data_rep()`, `rows.rep()`, `cols.rep()`, `data_shuffle()`,
  `list_shuffle()`, `vector_shuffle()`, `switch_rows()`, `switch_cols()`,
  `sample_by_column()`, `mutate_filter()`, and `add_key()` (alias `indexed()`).
* Custom operators: `%nin%` (alias `%!in%`) for "not in", `%or%` as a
  nullish-coalescing operator, `%eo%` as an error-coalescing operator, and
  `%match%` for percentage string similarity.
* `init()` for one-line initialization of multiple variables/objects, and
  `newSuperVar()` for variables that stay accessible and mutable across
  scopes, with optional locking, class enforcement, and edit-count limits.
* Environment management helpers: `refresh()`, `clean()`, `libraryAll()`,
  `summarize.envobj()`, and `lastwd()`.
* Statistics and outlier detection: `zscore()`, `zscore_outlier()`,
  `zscore_outlier2()`, `detect_outlier()`, `detect_outlier2()`,
  `iqr_outlier()`, plus distribution fit-checks
  (`is.normal`, `is.lognormal`, `is.uniform`, `is.poisson`, `is.gamma`,
  `is.weibull`, `is.cauchy`, `is.logistic`),
  `getDistribution()`, `geo.mean()`, `geo.sd()`, `geo.cv()`, `mode.calc()`,
  `na.cumsum()`, `normalize.vector()`, `sub.range()`, `in.range()`,
  `unique_len()`, `pairDist()`, and `compHist()`.
* Modeling utilities: `cat_to_num()`, `cat_to_num2()`, `from_tensor_slices()`,
  `multihead_att()`, `learn_rate_scheduler()`, and `make_dosing_df()`.
* String and text helpers: `bionic_txt()`, `randString()`, `strsplit.bool()`,
  `strsplit.num()`, `percent_match()`, `ndecimal()`, `as.boolean()`,
  `yesNoBool()`, `extract_comment()`, `remove_comment()`,
  `remove_content_in_quotes()`, `getDate()`, and `extract_IP()`.
* Date helpers: `date1to3()`, `date3to1()`, `getWeekSeq()`, `is.leap()`,
  `fAddDate()`, `is.increasing()`, and `is.decreasing()`.
* File and filesystem helpers: `duplicate()`, `ai.duplicate()`,
  `trim.file()`, `sort_file_type()`, `sort_length()`, `read.csv.print()`,
  `read.table.print()`, and `insertInText()`.
* RStudio addins for interactive use: header/section comment insertion
  (`add.header`, `header.rmd`, `add.sect.comment`), an environment-reset
  snippet (`add.snippet.clear`), and prompt-guided file duplication
  (`ai.duplicate`).
* Color and shape utilities: `rcolorconst()`, `mix.color()`,
  `mix.cols.btw()`, `setDisAlpha()`, `unsetDisAlpha()`, and
  `create_shape()`.
* Package/repo utilities: `find_packages()`, `archivedPkg()`,
  `rDecomPkg()`, `getGitRepoStart()`, `getGitRepoChange()`, and
  `track_func()` for tracking function call frequency, timing, and usage
  patterns.
* Math and misc helpers: `plus()`, `minus()`, `inc()`, `number()`,
  `const()`, `math.mm()`, `math.qt()`, `error.out()` (`%eo%`), `or()`
  (`%or%`), `chain_sep()` (`%.%`), and `chain_func()`.
* Four vignettes covering the general introduction, appending dates to
  filenames, the nullish-coalescing operator, and function-usage tracking.

## Notes

* Package authors: Obinna Obianom (maintainer) and Brice Richard.
* License: MIT.
* Future releases should record their changes as new sections above this
  one, following standard R package `NEWS.md` conventions (most recent
  version first).
