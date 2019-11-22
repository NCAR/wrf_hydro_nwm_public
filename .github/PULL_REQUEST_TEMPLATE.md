Use this template to give a detailed message describing the change you want to make to the code.

The title of this pull request should be a brief "purpose" for this change.

--- Delete this line and those above before hitting "Create pull request" ---

TYPE: choose one of [bug fix, enhancement, new feature, feature removed, no impact, text only]

KEYWORDS: approximately 3 to 6 words (more is always better) related to your commit, separated by commas

SOURCE: Developer's name and affiliation 

DESCRIPTION OF CHANGES: One or more paragraphs describing problem, solution, and required changes.

ISSUE: The GitHub Issue that this PR addresses. For issue number 123, it would be:
```
Fixes #123
```

TESTS CONDUCTED: Explicitly state if regression, integration, or unit tests were run before submitting (do not include the CI tests automatically run by GitHub)

NOTES: Optional, as appropriate. Delete if not used. Included only once for new features requiring several merge cycles. Changes to default behavior are also note worthy.


### Checklist
Merging the PR depends on following checklist being completed. Add `X` between each of the square 
brackets if they are completed in the PR itself. If a bullet is not relevant to you, please comment 
on why below the bullet.

 - [ ] Closes issue #xxxx (An issue must exist or be created to be closed. The issue describes and documents the problem and general solution, the PR describes the technical details of the solution.) 
 - [ ] Tests added (unit tests and/or regression/integration tests)
 - [ ] Backwards compatible
 - [ ] Requires new files? If so, how to generate them.
 - [ ] Fully documented
 - [ ] Short description in the Development section of `NEWS.md`
