## Submission note

This is a fix of a WARNING on r-devel-linux-x86_64-fedora-gcc. In addition, some
fixes to summary and print functions.

I have (1) reproduced the original WARNING and (2) verified that this fix removes it.

## Test environments

* Local Apple M1 Mac, on R4.5.0
* Local gcc15 Ubuntu exactly reproducing r-devel-linux-x86_64-fedora-gcc on which there were warnings.
* R-CMD-check via GitHub Actions on windows-latest, macOS-latest, 
  ubuntu-20.04 (release), and ubuntu-20.04 (devel).

## R CMD check results

There were 0 ERRORs, 0 WARNINGs, 0 NOTEs

