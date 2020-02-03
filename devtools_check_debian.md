> devtools::check()
Updating geocacheR documentation
Loading geocacheR
Writing NAMESPACE
── Building ─────────────────────────────────────────────────────────────────────────────────────────────────────── geocacheR ──
Setting env vars:
● CFLAGS    : -Wall -pedantic -fdiagnostics-color=always
● CXXFLAGS  : -Wall -pedantic -fdiagnostics-color=always
● CXX11FLAGS: -Wall -pedantic -fdiagnostics-color=always
────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────
✓  checking for file ‘/home/mark/Github/geocacheR/DESCRIPTION’
─  preparing ‘geocacheR’:
✓  checking DESCRIPTION meta-information ... OK
─  checking for LF line-endings in source and make files and shell scripts
─  checking for empty or unneeded directories
─  building ‘geocacheR_0.1.0.tar.gz’
   
── Checking ─────────────────────────────────────────────────────────────────────────────────────────────────────── geocacheR ──
Setting env vars:
● _R_CHECK_CRAN_INCOMING_REMOTE_: FALSE
● _R_CHECK_CRAN_INCOMING_       : FALSE
● _R_CHECK_FORCE_SUGGESTS_      : FALSE
── R CMD check ─────────────────────────────────────────────────────────────────────────────────────────────────────────────────
─  using log directory ‘/tmp/RtmpAzKqWO/geocacheR.Rcheck’
─  using R version 3.6.2 (2019-12-12)
─  using platform: x86_64-pc-linux-gnu (64-bit)
─  using session charset: UTF-8
─  using options ‘--no-manual --as-cran’
✓  checking for file ‘geocacheR/DESCRIPTION’ ...
─  checking extension type ... Package
─  this is package ‘geocacheR’ version ‘0.1.0’
─  package encoding: UTF-8
✓  checking package namespace information ... OK
✓  checking package dependencies (830ms)
✓  checking if this is a source package ...
✓  checking if there is a namespace
✓  checking for executable files ...
✓  checking for hidden files and directories ...
✓  checking for portable file names ...
✓  checking for sufficient/correct file permissions
✓  checking serialization versions ...
✓  checking whether package ‘geocacheR’ can be installed (3.4s)
✓  checking installed package size ... OK
✓  checking package directory
✓  checking for future file timestamps (631ms)
✓  checking DESCRIPTION meta-information ... OK
✓  checking top-level files
✓  checking for left-over files
✓  checking index information
✓  checking package subdirectories ... 
✓  checking R files for non-ASCII characters ...
✓  checking R files for syntax errors ...
✓  checking whether the package can be loaded (639ms)
✓  checking whether the package can be loaded with stated dependencies (604ms)
✓  checking whether the package can be unloaded cleanly (602ms)
✓  checking whether the namespace can be loaded with stated dependencies (585ms)
✓  checking whether the namespace can be unloaded cleanly (648ms)
✓  checking loading without being on the library search path (696ms)
✓  checking dependencies in R code (642ms)
✓  checking S3 generic/method consistency (1.3s)
✓  checking replacement functions (613ms)
✓  checking foreign function calls (646ms)
✓  checking R code for possible problems (4s)
✓  checking Rd files ...
✓  checking Rd metadata ...
✓  checking Rd line widths ...
✓  checking Rd cross-references ...
✓  checking for missing documentation entries (647ms)
✓  checking for code/documentation mismatches (2s)
✓  checking Rd \usage sections (1.5s)
✓  checking Rd contents ... 
✓  checking for unstated dependencies in examples ... 
✓  checking examples (2s)OK
✓  checking for unstated dependencies in ‘tests’ ...
─  checking tests ...
✓  Running ‘testthat.R’ (1.8s)
✓  checking for detritus in the temp directory (1.8s)
   
   
── R CMD check results ──────────────────────────────────────────────────────────────────────────────────── geocacheR 0.1.0 ────
Duration: 26.7s

0 errors ✓ | 0 warnings ✓ | 0 notes ✓
> 