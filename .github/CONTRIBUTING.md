# Contributing to WRF-Hydro®
All bug reports, bug fixes, code, and documentation contributions are welcome and encouraged.
However, guidelines need to be established and followed to ensure that community contributions can
be effectively incorporated.

Please read the following guidelines carefully before contributing.

# Guidelines

**Failure to adhere to the following guidelines may result in your contributions being permanently
rejected if they are unable to be merged.**

All contributions must be made through the
[NCAR/wrf_hydro_nwm_public](https://github.com/NCAR/wrf_hydro_nwm_public) GitHub page.
Contributions submitted via email or help desk will not be processed.

## Bug reports, feature requests, and documentation suggestions
All bug reports, feature requests, and documentation suggestions should be submitted via [GitHub
issues](https://github.com/NCAR/wrf_hydro_nwm_public/issues). Before submitting your issue, please
search for a few keywords related to your issue in the open and closed issues to discover if your
issue has already been logged and/or resolved. An issue template has been provided to assist with
submitting bug reports. For more information on using GitHub issues, please see
https://help.github.com/articles/about-issues/.

### Bug reports
Please use the supplied issue template when submitting bug reports. This will help ensure that your
bug is described in sufficient detail. You can read more on how to construct an
appropriate bug report here: https://stackoverflow.com/help/mcve

### Feature requests
Please follow similar guidelines when suggesting new features as you would for reporting bugs. Be
specific and detail the added value of your suggested feature.

### Documentation suggestions
All technical documentation for the WRF-Hydro model will eventually be formatted as markdown, which
will greatly simplify updates and changes in the future. In the meantime, please reference the
document title, page number and quote the line or section for suggesting changes to the
documentation.

## Code contributions
All code development contributions will be made via [forks](https://help.github.com/articles/about-forks/)
and [pull requests](https://help.github.com/articles/about-pull-requests/). If you are
unfamiliar with GitHub, forks, or pull requests, see [collaborating with issues and pull
requests](https://help.github.com/categories/collaborating-with-issues-and-pull-requests/) for guidance.

### WRF-Hydro Fortran code style standards
Please see our Fortran [code style guidelines](CODESTYLE.md)

### Universal guidelines

* All code contributions must be made through GitHub following specific guidelines listed below.
* All contributions must be made in an open, and public way via the GitHub repository. There will
  be no embargo period for code contributions.
* All contributions must be able to be merged without conflict. It is the responsibility of the
  contributor to resolve any merge conflicts **prior** to submitting a pull request.
* All contributions must pass relevant automated testing procedures. These tests are executed
  automatically when you submit your pull request.
* Be respectful when giving and receiving feedback on contributions or issues (see our community [code of conduct](../CODE_OF_CONDUCT.md).

### Pull requests

#### Contributor responsibility
Pull requests will only be accepted if they follow the best practices described below. Not
following best practices described below is grounds for rejection of pull requests and may result
in excessive manual work for contributors. It is the contributors’ responsibility to know and
follow best git practices.

#### Commits: practices & messages
Learning to use commits wisely and “atomically” is the foundation for success. As users gain
familiarity with basics of how git works and the merging practices we are using. The next level of
sophistication is interaction with the git log. Reading logs is greatly facilitated by writing good
logs, or “commit messages”. The following webpage gives a crash course on how to write good commit
messages: https://chris.beams.io/posts/git-commit/

#### Frequency
Pull requests should happen with high enough frequency to minimize the scope of the code review.
Commits which do not changes answers (pass regression testing) MUST be kept separate from commits
which do change answers even when the work is on the same feature or bug. The portion of the code
which changes the answer must be isolated as much as possible into its own pull request.

### Types of code contributions
#### Bug fixes
All bug fixes must address a specific [GitHub
issue](https://github.com/NCAR/wrf_hydro_nwm_public/issues). Only issues labeled with ["community
dev"](https://github.com/NCAR/wrf_hydro_nwm_public/labels/community%20dev) are open to community
contributions. The majority of issues will be open to community contributions, but some cases may
require more careful handling. Issues without the "community dev" label are either still being
investigated, are of broad scope, or involve changes to code that is actively being developed internally.

All code changes to address issues must be submitted using a pull request, and the corresponding
issue number must be referenced in the pull request. For more information on referencing issues,
see [issues and keywords](https://help.github.com/articles/closing-issues-using-keywords/).

#### New features
Code contributions for minor model enhancements or new features follow a similar pattern to bug
fixes. The proposed feature must be submitted as a GitHub issue and all discussions regarding the
new feature should reside in the issue thread. Additionally, work should not begin on the new
feature until the issue has been tagged with "community dev".

Depending on the scope of the new feature, a WRF-Hydro core contributor may suggest you create a
[branch](https://help.github.com/articles/about-branches/) to hold all development for the new
feature.

#### Research development
Larger-scale research development, e.g. model coupling, must be coordinated with a WRF-Hydro team
member prior to beginning work. This type of development may impact other community projects and
requires more careful governance to help steer the direction of model development and maximize
harmony among the various NCAR, NOAA, and WRF-Hydro community research efforts.

### Checklist for new feature contributions
#### You should have the following to submit a new feature contribution to the WRF-Hydro code base:
1. A WRF-Hydro team point of contact (POC). Early in your development process, it is always a good idea
to reach out to the WRF-Hydro team and find a good POC for your new feature. This person will help
keep you informed of new developments as they may relate to your feature, advise you on which branches
to keep current with (if other than main), and provide pointers on requirements.
2. New feature code integrated in with the latest WRF-Hydro main branch (or other as advised by your
WRF-Hydro POC). This merged code should exist in your own fork, but can be in your own main branch or
a separate feature branch.
   * *TIP #1*: It is always a good strategy to pull the latest WRF-Hydro main code into your branch
fairly often throughout your development cycle. Leaving months of code changes to merge at the end of
your development cycle can result in lots of merge conflicts, time-consuming manual edits, and higher
potential for making errors.
   * *TIP #2*: New code should follow the
[WRF-Hydro code contribution guidelines](https://github.com/NCAR/wrf_hydro_nwm_public/blob/main/.github/CONTRIBUTING.md).
3. Documentation for your new feature. This should include (1) a simple summary in the release notes
(added to [NEWS.md](https://github.com/NCAR/wrf_hydro_nwm_public/blob/main/NEWS.md)), and (2) a
detailed description of the new feature suitable to be included within the
[WRF-Hydro Technical Description](https://ral.ucar.edu/sites/default/files/public/projects/wrf_hydro/technical-description-user-guide/wrf-hydro-v5.1.1-technical-description.pdf),
included as a new markdown document in the
[Doc](https://github.com/NCAR/wrf_hydro_nwm_public/tree/main/trunk/NDHMS/Doc)
folder.
4. An updated
[standalone test case](https://ral.ucar.edu/projects/wrf_hydro/testcases) demonstrating how
your new feature works, based on one of the existing WRF-Hydro test cases (e.g., Croton). If you cannot
adapt an existing test case and need to create something new, please consult with your POC. The test
case should include:
   * Domain and parameter files
   * Namelists
   * Example forcings if needed (idealized forcing use is OK)
   * Test case documentation
5. Testing - you must clearly demonstrate that your new feature:
   * Does no harm to the existing code (e.g., produces identical solutions EXCEPT when the new feature is
activated by a namelist or parameter setting).
   * Fixes a documented issue (reference existing issue in the
[WRF-Hydro GitHub issue tracking](https://github.com/NCAR/wrf_hydro_nwm_public/issues) - this could also be a feature request).
   * If model performance is improved, for example through more efficient IO, memory utilization, or other
software-specific improvements, it must be demonstrable and measurable.
   * *TIP*: All pull requests go through automated CI testing, but for a new feature it is important to do
thorough testing ahead of time so you have a good sense of the model impacts and can explain/justify them.
Do not wait until automated CI testing to test your new feature.
6. A pull request from a branch on your fork to the WRF-Hydro main branch (or other as advised by your
WRF-Hydro POC). Make sure to also follow the specific
[WRF-Hydro pull request guidelines](https://github.com/NCAR/wrf_hydro_nwm_public/blob/main/.github/PULL_REQUEST_TEMPLATE.md).
