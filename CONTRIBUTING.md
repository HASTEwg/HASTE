# Contributing to HATS
We welcome all kinds of input! Feel free to:
- Report a bugs
- Discuss the state of the code
- Submit fixes
- Propose new features

## Writing Code for HATS
We're open to ANY contributions to improve the project, but expect *energetic* discussion to justify code not following these guidlines!
- HATS is written exclusively in modern Fortran (specifically Fortran 2008). We are not equipped to support a multi-language project at this time.
- Avoid the use of language extensions. Where extensions are used, also provide standard-compliant code via conditional compilation (by a "C-style" preprocessor).
- Strive to be self-documenting! Thoroughly comment your code (describe the procedure throughout the procedure) and use descriptive procedure/variable names.
- Avoid use of variables with the SAVE attribute (this includes module variables and some other data-sharing constructs). These can introduce challenges when adapting the code for massivley parallel architectures. Similarly, avoid use of recursive procedures.
- **NO deleted or depreciated language features.** This includes GOTO, ASSIGN, integer FORMAT, and PAUSE among others. Please use modern and standard-compliant code in these instances.
- **NO implicit typing.** All variable types must be explicitly declared, and mixed-type arithmetic should be coded with explicit type-conversions.
- Use consistent style. (No tabs, 4-space indentation)

## Making Contributions
Pull requests are the best way to propose changes to the codebase. We actively welcome your pull requests:
- Fork the repository and create your branch from master.
- If you've added code that should be tested, add tests.
- Make sure your code lints.
- Issue a pull request.

## Any contributions you make will be under the GNU General Public License version 3 software license
In short, when you submit code changes, your submissions are understood to be under the same GPL-3.0 that covers the project. Feel free to contact the maintainers if that's a concern.

## Report bugs using Github's issues
Bugs may be submitted as issues on GitHub. A good issue tends to have:
- A quick summary and/or background
  - What you expected would happen
  - What actually happens
- Steps to reproduce
- Sample code if applicable, or links to relevant source lines
- Notes (possibly including why you think this might be happening, or stuff you tried that didn't work).
Be specific! You found the bug, you're submitting the bug: I hate to break it to you, but that makes you the world's expert on the bug at the moment.
