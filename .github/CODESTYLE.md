# WRF-Hydro Fortran Conventions Guide

# Fortran standard
New code contributions should be written to at least
[Fortran 2003 standard](https://gcc.gnu.org/wiki/GFortranStandards#Fortran_2003).

# Source Code Formatting
* Max line length 100 characters. All wrapped code statements shall at least be indented one
  additional level beyond the previous level.
* Do NOT use tab characters.
* Indentation: 3 spaces (no tabs) for all control below the subroutine/function level (indenting
  less often saves a lot of space). Tightly nested do loops can be exceptions. Put comments at same
  indentation level as the code which it is commenting.


```fortran
module my_module
integer :: local_var
!==================================================================================================
subroutine foo
implicit none
!no indent until control units, line breaks between units

do i=1,2     ! white space in-line not crucial for readability as in long subroutine calls
   count=i
end do ! i=1,2

do i=1,2
   do j=i,(i*2)
      count = i + j
   end do ! j=i,(i*2)
end do ! i=1,2

do i=1,2
do j=i,(i*2)    ! control structure is compact, easy to perceive without indentations
      count = i + j
end do ! j=i,(i*2)
end do ! i=1,2

end subroutine foo

end module my_module
```

* Generally use lower case except for where emphasis is needed.
* Camel case, vs underscores
    * firstLowerCamelCase: use most generally
    * FirstUpperCamelCase: Classes
    * lowecase_underscore_separated: use for mouldes, subroutines, functions
    * UPPERCASE_UNDERSCORE_SEPARATED: constants

* Line up related pieces of syntax (readability when there are repeated elements, ability to count
  the position of arguments), including end-of-line characters.

```fortran
# WRONG
foo=function(apple, banana, corn, &
      durian, eggplant, fig)

# CORRECT
foo=function(apple,  banana,   corn, &
             durian, eggplant, fig    )
```

* Place the same number of arguments on each line except the last for countability/position
  matching.

```fortran
# WRONG
foo=function(a,b,c = 1)
# CORRECT
foo = function(a, b, c=1)
```

* Horizontal white space: enhances readability, esp w respect to function/subroutine arguments.
  Whitespace helps identify separate things.
* Vertical white space:
  * No vertical white space: Used to group closely-related lines of code.
  * Single vertical white space: Separate less-related lines of code. After control structures.
  * Two vertical white spaces: only used to emphasize separation between functions, subroutines and
    occasionally large code blocks.
* Control structures identified against their opening statement.
  * #ifdef:  When nested.
  * If, do, while, case: Best practice: All the time. Required: when spanning more than one page of
    vertical space.

```fortran
#ifdef HYDRO_D
#ifdef OBSCURE_THING
…
#endif /* HYDRO_D */
#endif /* OBSCURE_THING */


if(a .eq. 100) then
…..
endif ! if(a .eq. 100)

or

if(a .eq. 100) then  !! a .eq. 100 block
…
endif  !! a .eq. 100 block
```

* Comment blocks above every unit (module, subtroutine, function), with the following form. This
  should be the NCO standard template.

```fortran
!===================================================================================================
! Subroutine Name:
!   subroutine read_nudging_last_obs
! Author(s)/Contact(s):
!   James L McCreight <jamesmcc><ucar><edu>
! Abstract:
!   does some fancy thing
! History Log:
!   02/03/16 -Created, JLM.
! Usage:
! Parameters:
! Input Files:
! Output Files:
! Condition codes:
! User controllable options: None.
! Notes: Needs better error handling...
```


* Allocate/deallocate statements do NOT span multiple lines. This is so grep can reveal all
  allocation/deallocation pairs.

```fortran
# WRONG
allocate(foo, &
         bar)
# CORRECT
allocate(foo)
allocate(bar)
allocate(foo, bar)
```

# Naming
## Source file names
* The name of the source file or script shall represent its purpose. All of the functions in a file
  shall have a common purpose.
* Source file names SHALL NOT include spaces nor colons (‘:’) nor any other special
  characters.
* End in the `.Fxx` extension, with `xx` corresponding to the fortran standard, e.g. `.F90`
* Filenames shall use the `lowercase_underscore_separated.Fxx` convention.

## Output file names
* Source file names SHALL NOT include spaces nor colons (`:`) nor any other special
  characters.

## Variable names
* Variable names shall convey their intended use to other developers who did not author the code.
  This eliminates need for inline comments that describe variables.
  * DO NOT strive to much for shortening of variable names, prefer meaningful name: local
    shortening is often possible with the associate construct.
* Comments/descriptions on variables:
  * All variables in argument lists will be described in the definition using FORD syntax.
  * Derived type arguments only need documented in bulk, their individual components only need
    defined where the derived type is defined.
  * Variable descriptions will not be redundant with the variable name
  * Descriptions will describe the physical quantity
  * Descriptions will include units.
* Variable names shall contain a noun.
* Exception: when widely accepted equations have simple variable names. (e.g. `F=m*a`)
* Loop counters should typically have names.
* Loop counters should NOT be single letters. Simply doubling the letter makes it easier to search
  for the use (and possibly replace).
* Conventions for special kinds of variables:
  * Accumulation variables: “acc” prefix, e.g.
  * MPI global variables
  * prognostic vs diagnostic variables
  * logical variables must be named to reflect the state they are used to convey, most with the
    verb to be, e.g. :
    * `lib_is_initialized` vs `lib_init`
    * `obj_has_parent` vs `obj_parent`

## Function/Subroutine Names
* Function names shall contain at least one verb, e.g. `get_name`, `parse_control_string`.
* Function names shall convey their intended use to other developers who did not author the code.
* Follow the `lowercase_underscore_separated` convention.

## Constant names
* Name all constants: Numbers should not be used in the code except in variable definitions.
* All constants are defined as parameters.
* Exceptions: should be rare and clearly typed (float, double, etc).
* Constants follow the `UPPERCASE_UNDERSCORE SEPARATED` convention for naming.
* Constant names shall describe what the contained value represents within context, e.g.
  `NO_DATA_VALUE`, `PLANCK_LENGTH`, `HIGH_TEMPERATURE_THRESHOLD`, etc.

# Documenting in the source
* Code documentation should be a primary source of overall documentation. By following simple
  practices, such documentation can be extracted to live separately if needed. But keeping it close
  as possible to the source maintains consistency and improves communication among developers: all
  you need is the source to understand the code (not other random documents)
* Write code that documents itself. Try to make code as clear as possible (“self-documenting”) to
  avoid use of comments and redundancy between the two which needs synchronized or is confusingly
  out of sync. Writing self-documenting code includes using variable names with obvious meaning and
  documenting ambiguities in variable names when they exist. Minimize commenting “what” (repeating
  the code with comments) but do it when it is necessary for interpretation.
* Document why (not what). Algorithmic choices are often the hardest thing to perceive, not
  function calls on variables.
* Indent comments to the indentation level of the code which is being commented.
* Comments shall be written in English with good spelling, punctuation, and grammar.
* Documentation shall be placed in the code and [FORD](https://github.com/cmacmackin/ford) will be
  used to generate documentation.
* TODO: Comments used to remind developers of future or unfinished actions in source code shall
  begin with “TODO FML” (first middle last initials, as available), and describe the action to be
  taken. TODO comments shall also list a specific date or event by which the TODO action will be
  completed.

```fortran
! TODO JLM : Rename the variable foo to discharge. Target Date: 12/25/20

! TODO JLM : Rename the variable foo to discharge.
! TODO JLM : Target Date: 12/25/20
```

* DEPRECATED: Flagging deprecated code sections shall be done in the following way for automated
  removal at end-of-version code release.

```fortran
!DEPRECATED >>>
!code
!code
!<<< DEPRECATED
```

# File header comments
* Each source file shall contain one file header comment.
* File header comments shall contain the following:
  * A concise description of the collective purpose of file contents.
      * Exception: When a file contains only one class or function definition, this description
        shall simply state the file is an implementation or definition file for class/function
        `<name>`. The class or function description will be written into the class or function
        header comment.
* Organization name: National Center for Atmospheric Research
* Current maintainer
* Author names. Multiple authors should be listed when more than one developer has worked on a
  source file over time.

```fortran
! module_super_foo.F
! Purpose: This module file contains the derived type and
!          methods for the super_foo class.
! National Center for Atmospheric Research
! Responsible: James L McCreight <jamesmcc>ucar<edu>
! Authors: James McCreight, Logan Karsten, Wei Yu
```

# Module/Subroutines/Function Usage
* Always use modules.
* Intent: all arguments should be given an intent (in,out, inout),
* Each argument is defined on its own line. Document details in- or below- line using
  [FORD](https://github.com/cmacmackin/ford/wiki/Writing-Documentation) conventions.
  * FORD will ignore a normal comments preceded with a single exclamation mark (!) However,
    comments with two exclamation marks (!!) are interpreted as documentation and will be captured
    for inclusion in the FORD output. By default, FORD documentation comes after whatever it is
    that you are documenting, either at the end of the line or on a subsequent line.
  * For longer blocks of documentation, it can be inconvenient to continually type the "docmark" =
    ‘!!’. For such situations, the docmark_alt (set to * by default) may be used in the first line
    of the documentation comment. Any immediately following lines containing only a comment will
    then be included in the block of documentation, without needing the "docmark".

    Example:
```fortran
subroutine feed_pets(cats, dogs, food, angry)
    !! Feeds your cats and dogs, if enough food is available. If not enough
    !! food is available, some of your pets will get angry.

    ! Arguments
    integer, intent(in)  :: cats
    !! The number of cats to keep track of.
    integer, intent(in)  :: dogs
    !! The number of dogs to keep track of.
    real, intent(inout)  :: food
    !! The amount of pet food (in kilograms) which you have on hand.
    integer, intent(out) :: angry
    !! The number of pets angry because they weren't fed.

    !...
    return
end subroutine feed_pets
```

* Avoid “side effects” (return values are the only thing modified: files are not created,
  module/global variables are not changed, etc). Use pure functions?
* `implicit none` for all program units
* Use `use, only:` as much as possible.
* Restrict number of passed variables per line in the function/subroutine definition/call to 4 max.
  Line up variables vertically for ease of reading and counting. Match the call layout to the
  definition layout.
* Have a local variables section separate from passed variables

```fortran
# WRONG
some_function(a,b,c,d,e,f,g,h &
,i,j,k,l)
real :: a,b,c,d,e,f
Ingeter :: localVar
Real, intent(in) :: h,i,j,k,l

call some_function(a,b,c,d &
,e,f &
g,h,i,j,k,l)

# CORRECT
some_function(a, b, c, d &
             ,e, f, g, h &
             ,i, j, k, l )
Use module_great_stuff: only, shiny_object, rad_tool
Implicit none
real, intent(in)  :: a !! acceleration   (m/s2)
real, intent(in)  :: b !! brownian coeff (-)
real, intent(out) :: c !! celerity       (m/s)
… (etc) …
real, intent(in)  :: l !! lousy variable (kgmsK)

!! Local variables
integer :: localVar

call some_function(a, b, c, d &
                  ,e, f, g, h &
                  ,i, j, k, l )
```

# Variable Scoping and Definition
* Minimize global data.
* Global data should be limited to constants as much as possible.
* Define and use global type parameters.
* Make all derived type components private.

# Run-time error messages
* Must indicate the function/routine in which they occur.
* Should be informative.


# Compiler warning messages
* Compilers generally issue two types of messages: warnings and errors. Extent of compiler warning
  messages can typically be tuned with a compiler option. Compiler warnings normally do not stop
  the compile process, but can still result in run-time problems. Compiler errors do stop the
  compile process, forcing the developer to fix the problem and recompile. STANDARD:
* When available, compiler options should be set to produce the maximum number of warning messages.
* Compiler and linker warnings shall be treated as errors and fixed.

# Other general guidelines
* Code should always be written with cleanliness and clarity in mind. Algorithm implementations
  should not be unnecessarily complicated without significant performance gains over simpler
  alternatives.
* Structured programming - do NOT use GOTO
* Related resources
  * http://research.metoffice.gov.uk/research/nwp/numerical/fortran90/f90_standards.html
  * https://github.com/szaghi/zen-of-fortran#standard
  * http://www.fortran.com/Fortran_Style.pdf
