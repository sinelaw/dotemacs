Dan's Emacs 24.4+ configuration
===============================

This is a rebooted version of my configuration.

Some stuff require build for this to be used, so:

```
make
```

A few things to consider:

* To use the native Emacs scrollbar I recompile Fedora's Emacs
  package to disable GTK's own scrollbars.
* Should not use any Emacs extension from Fedora itself but only
  the base package so that no externous dependencies add up.
* Not using the git-submodule - all additions are built-in taking
  from respective upstream git revisions. I prefer a workflow with
  git that is not encumbered by having foreign cloned repos under
  my tree.
* Originally I used vmalloc's configuration but instead I prefer
  to keep things much more minimal by including and loading only the
  packages I need.
* Does not depend on MELPA or auto-updates. I'm old fashioned, and
  like to keep a stable configuration.

So if you ever want to test-run this, you would only need emacs
package from Fedora, and this repo under .emacs.d. That's it - no
strange magic (except when expecting things to be available 
under $PATH).
