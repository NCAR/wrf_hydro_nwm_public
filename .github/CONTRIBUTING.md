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
bug is described in sufficient detail. Additionally, you can read more on how to construct an
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
requests](https://help.github.com/categories/collaborating-with-issues-and-pull-requests/) for a
thorough guide to collaborating on GitHub.

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
* Be respectful when giving and receiving feedback on contributions or issues.

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
issue](https://github.com/NCAR/wrf_hydro_nwm_public/issues). Only issues labeled with "Community
dev" are open to community contributions. The majority of issues will be open to community
contributions, but some cases may require more careful handling. Issues without the "Community dev"
label are either still being investigated, are of broad scope, or involve changes to code that is
actively being developed internally.

All code changes to address issues must be submitted using a pull request, and the corresponding
issue number must be referenced in the pull request. For more information on referencing issues,
see [issues and keywords](https://help.github.com/articles/closing-issues-using-keywords/).

#### New features
Code contributions for minor model enhancements or new features follow a similar pattern to bug
fixes. The proposed feature must be submitted as a GitHub issue and all discussions regarding the
new feature should reside in the issue thread. Additionally, work should not begin on the new
feature until the issue has been tagged with "Community dev".

Depending on the scope of the new feature, a WRF-Hydro core contributor may suggest you create a
[branch](https://help.github.com/articles/about-branches/) to hold all development for the new
feature.

#### Research development
Larger-scale research development, e.g. model coupling, must be coordinated with a WRF-Hydro team
member prior to beginning work. This type of development may impact other community projects and
requires more careful governance to help steer the direction of model development and maximize
harmony among the various NCAR, NOAA, and WRF-Hydro community research efforts.
