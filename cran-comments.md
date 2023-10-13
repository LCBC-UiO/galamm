## Submission note

This is a resubmission aimed at fixing current issues. There are three types of 
issues, which are described under each header below:

### 1. Memory issues (clang-ASAN, gcc-ASAN, valgrind)

I was able to reproduce the error using debug mode locally with 'R -d lldb'. 
It was caused by attempting to access a vector element with a an index '-2', 
which clearly does not work. This case has now been fixed, and the fix is 
described in more detail here: https://github.com/LCBC-UiO/galamm/issues/165.

### 2. Errors on r-oldrelease

Failed because of the following test, where 'fam' is some model family, e.g.,
'fam <- binomial()' or 'fam <- gaussian()'. I tested 'is.na(fam$dispersion)', 
but in R4.2.3 these 'fam' would not have an element named 'dispersion', and
hence the test failed. In R4.3.1 'fam' does have a 'dispersion' element, and
hence the test passed here. I have rewritten the code so it does not rely on
this element, and verified it using devtools::test_win_oldrelease(). The fix
is described in more detail here: https://github.com/LCBC-UiO/galamm/issues/167.

### 3. Errors on M1 Mac

On M1 Mac, 10 tests are skipped and 1 test fails. The failing test happens
because the expected deviance value differs from the computed deviance value
at 3rd decimal (885.7339 vs 885.7341). I have thus reduced the tolerance of this
test, and also confirmed that the version now being submitted works with the 
R Mac Builder (test outputs provided below).


## Test environments

* Windows, r-release, r-oldrelease, r-devel
* Local Apple Silicon M1 in debug mode (R -d lldb) on r-devel.
* Local Apple Silicon M1, on R4.3.1 and R4.2.3.
* R-CMD-check via GitHub Actions on windows-latest, macOS-latest, ubuntu-20.04 (release), and ubuntu-20.04 (devel).

„
## R CMD check results

0 errors | 0 warnings | 1 note

* checking CRAN incoming feasibility ... NOTE
Maintainer: '�ystein S�rensen <oystein.sorensen@psykologi.uio.no>'

Possibly mis-spelled words in DESCRIPTION:
  Hesketh (17:52)
  Jeon (17:38)
  Rabe (17:47)
  Sorensen (11:33)
  al (11:45)
  et (11:42)
  nonlinearly (14:5)

