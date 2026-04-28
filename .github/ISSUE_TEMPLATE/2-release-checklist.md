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
