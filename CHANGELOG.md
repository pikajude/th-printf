## 0.7.0 (2019-09-11)

Clarifying 0.6.0's note: this package doesn't use backpack

* th-printf can now use different types of builder as long as they implement
  Buildable. As a result, th-printf can now produce lazy Text as well as String.
* Dropped support for GHC < 8.2. It may still build with 8.0 but I'm no longer
  testing this library with that compiler version.

## 0.6.0 (2018-08-18)

Backported new backpack-based code to pre GHC-8.4 versions.

* Rename of public modules
* Parser rewrite
* th-printf now prints a warning when given an erroneous format string
* Several printf behaviors have been updated to comply with spec:
  * `x`, `u`, etc. specifiers now only apply to positive integers
  * Length specifiers are allowed
* Generated testsuite covers more cases
