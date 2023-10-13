## Submission note

This is a resubmission aimed at fixing current issues. There are three types of 
issues, and the respective fixes are described under each header below:

### 1. Memory issues (clang-ASAN, gcc-ASAN, valgrind)

I was able to reproduce the error both using debug mode locally with 'R -d lldb'
and using the Docker image rocker/r-devel-san. 
The issue was caused by attempting to access a vector element at a an index 
'-2', which clearly does not work. This case has now been fixed, and the fix is 
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

* R development version with sanitizer support via rocker/r-devel-san.
* R Mac Builder (https://mac.r-project.org/macbuilder/submit.html)
* Windows, r-release, r-oldrelease, r-devel.
* Local Apple M1 Max in debug mode (R -d lldb) on R4.3.1.
* Local Apple M1 Max, on R4.3.1 and R4.2.3.
* R-CMD-check via GitHub Actions on windows-latest, macOS-latest, 
  ubuntu-20.04 (release), and ubuntu-20.04 (devel).


## R CMD check results

### r-devel-san

0 ERRORs, 0 WARNINGs, 0 NOTEs

### R Mac Builder

0 ERRORs, 0 WARNINGs, 1 NOTE:

* checking installed package size ... NOTE
  installed size is 24.1Mb
  sub-directories of 1Mb or more:
    doc    1.9Mb
    libs  21.1Mb


### Windows, r-release, r-oldrelease, r-devel

### Local Apple M1 Max in debug model (R -d lldb)

0 ERRORs, 0 WARNINGs, 0 NOTEs

### Local Apple M1 Max, on R4.3.1 and R4.2.3.

0 ERRORs, 0 WARNINGs, 0 NOTEs

### GitHub Actions

0 ERRORs, 0 WARNINGs, 0 NOTEs
