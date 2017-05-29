# patchdep
Patchdep: Analyze dependencies between patches

Patchdep analyzes a set of patches for dependencies. Two patches have a dependency when they touch the same lines of code in any of the files in the code base.
With patch dep, you can extract the minimum set of patches you need in order to merge a given patch, or a given set of patches.

```
Usage: patchdep <options>
  -patchlevel 	Patch level
  -patchset 	file with the list of available patches in working order
  -iwant 	file with the list of patches you want
  -dontwant 	file with the list of patches you want
  -dot 	output dependencies as a dot file
  -help  Display this list of options
  --help  Display this list of options
```
