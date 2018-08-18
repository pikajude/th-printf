## 0.6.0 (2018-08-18)

Backported new backpack-based code to pre GHC-8.4 versions.

* Rename of public modules
* Parser rewrite
* th-printf now prints a warning when given an erroneous format string
* Several printf behaviors have been updated to comply with spec:
  * `x`, `u`, etc. specifiers now only apply to positive integers
  * Length specifiers are allowed
* Generated testsuite covers more cases
