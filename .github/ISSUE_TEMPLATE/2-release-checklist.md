---
name: Release checklist
about: Checklist for preparing a release
title: "Release vX.Y.Z"
labels: release
---

## Before Release
- [ ] Choose release version
- [ ] Create list of new feature for release
- [ ] Add new features
  - [ ] Add new feature `foo`
  - [ ] Add new feature `bar`
- [ ] Update Tests
  - [ ] Standalone testcases: do parameters, namelists, domains, need updates?
  - [ ] Regression tests: are new Github Actions needed?
- [ ] Update documentation
  - [ ] Add documentation for feature `foo`
  - [ ] Add documentation for feature `bar`
  - [ ] Check for broken links
  - [ ] Update other user guides
- [ ] Bump WRF-Hydro Version
  - [ ] WRF-Hydro Repo
    - [ ] readthedocs `docs/userguide/conf.py`: version and release string
    - [ ] readthedocs `docs/userguide/index.rest`: two places of 'Version X.Y' string
    - [ ] readthedocs `docs/userguide/appendices.rest`: link to Croton tarball
    - [ ] readthedocs `docs/userguide/meta.rest`: version string in two places
    - [ ] `tests/README.md`: Croton tarball link in two places
    - [ ] `tests/ctests/setup_cmake_testcase.sh`: version value
    - [ ] `src/.version`
  - [ ] WRF-Hydro Docker
    - [ ] `dev/release/Docker`: VERSION variable
    - [ ] `dev/release/Makefile`: tag version
    - [ ] `training/Croton/entrypoint.sh`: version number in three places
    - [ ] `.github/workflows/docker-push.yml`: release tag
- [ ] Create `Release Notes`
- [ ] Update `NEWS.md`

## During Release
- [ ] Create release branch
- [ ] Tag release
- [ ] Push tag
- [ ] Draft GitHub release
- [ ] Add artifacts to release

## After release
- [ ] Verify new [Zenodo DOI](https://zenodo.org/records/15040873)
- [ ] Update Docker images
- [ ] Create Docker image of new release
- [ ] Training lesson updates
