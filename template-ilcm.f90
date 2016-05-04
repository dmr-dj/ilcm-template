!-----|--1----+----2----+----3----+----4----+----5----+----6----+----7----+----8----+----9----+----0----+----1----+----2----+----3-|
!
!>      VUA and IPSL/LSCE by the iLOVECLIM / LUDUS coding group / Within the LUDUS code environement
!
!       LICENSING TERMS:
!>      \copyright
!!      This file is part of [insert sub-component name here, in following Foobar]
!!      Foobar is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License
!!      as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
!!
!!      Foobar is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty
!!      of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details.
!!
!!      You should have received a copy of the GNU General Public License along with Foobar.
!!      If not, see <http://www.gnu.org/licenses/>.
!
!-----|--1----+----2----+----3----+----4----+----5----+----6----+----7----+----8----+----9----+----0----+----1----+----2----+----3-|

!dmr -- Adding the choice of components through the pre-processing options
#include 'choixcomposantes.h'
!dmr -- Adding the choice of components through the pre-processing options

!-----|--1----+----2----+----3----+----4----+----5----+----6----+----7----+----8----+----9----+----0----+----1----+----2----+----3-|
!      MODULE: [Module_Name here]
!
!>     @author  The First Author (tfa)
!>     @author  The Second Author (tsa)
!
!>     @brief This module [Foo_mod] is handling the creation of a particuliar type of fried noodles ...
!
!>     @date Creation date: November, 41st, 2999
!>     @date Last modification: SLastChangedDate$
!>     @author Last modified by : tfa, tsa
!
!>     @version This is svn version: $LastChangedRevision$
!
!      DESCRIPTION
!>     Here add the long_description of the module ...
!!      more blablas ...
!!      more blablas ...
!>      Compute formula: \f$ \frac{d\lambda}{dt} , \frac{d\phi}{dt},  \frac{dz}{dt} \f$
!!
!!      REFERENCES: papers or other documents to be cited...(including link when possible)    
!
!       REVISION HISTORY:
!       YYYY-MM-DD - Initial Version
!       TODO_YYYY-MM-DD - TODO_describe_appropriate_changes to be done or discussed - TODO_name
!-----|--1----+----2----+----3----+----4----+----5----+----6----+----7----+----8----+----9----+----0----+----1----+----2----+----3-|

      module Foo_mod

       use AnotherModule_mod, only: some_variable
       use AnotherModule_mod, only: some_otherfunction

       implicit none
       private

       public :: someFunction
       public :: Foo

       ! type foo contains data structure for foo data:
       ! foo1  : definition of variable foo1 [units]
       ! foo2  : definition of variable foo2 [units]
       type Foo

       end type Foo
       ! 

       ! NOTE_avoid_public_variables_if_possible
       ! IF NOT POSSIBLE... don't forget to include definition and units!

      contains

!-----|--1----+----2----+----3----+----4----+----5----+----6----+----7----+----8----+----9----+----0----+----1----+----2----+----3-|
!      ROUTINE: [Routine or Function Name Here]
!
!>     @brief This subroutine / function is adding a wonderful knew unused functionality
!
!      DESCRIPTION:
!
!>     Here add the long_description of the module ...
!!      more blablas ...
!!      more blablas ...
!>      Compute formula: \f$ \frac{d\lambda}{dt} , \frac{d\phi}{dt},  \frac{dz}{dt} \f$
!
!!      REFERENCES: papers or other documents to be cited...(including link when possible)    
!
!       REVISION HISTORY:
!       YYYY-MM-DD - Initial Version
!       TODO_YYYY-MM-DD - TODO_describe_appropriate_changes to be done or discussed - TODO_name
!-----|--1----+----2----+----3----+----4----+----5----+----6----+----7----+----8----+----9----+----0----+----1----+----2----+----3-|

      function someFunction(inParam, outParam, inOutParam) result(returnValue)

       use AnotherModule_mod, only: some_variable          ! brief_description [units]
       use AnotherModule_mod, only: some_otherfunction     ! brief_description [units]

!-----|--1----+----2----+----3----+----4----+----5----+----6----+----7----+----8----+----9----+----0----+----1----+----2----+----3-|
! tfa   By reference variables ...
!>    @param[in]  inParam  The Beaufitul Parameter that does all the input [units] !!
!>    @param[out] outParam The Beaufitul Parameter that does all the output [units]!!
!>    @return returnValue
!-----|--1----+----2----+----3----+----4----+----5----+----6----+----7----+----8----+----9----+----0----+----1----+----2----+----3-|

       real, intent(in)    :: inParam     
       real, intent(inout) :: inOutParam  
       real, intent(out)   :: outParam

!-----|--1----+----2----+----3----+----4----+----5----+----6----+----7----+----8----+----9----+----0----+----1----+----2----+----3-|
! tfa  Local variables ...
!-----|--1----+----2----+----3----+----4----+----5----+----6----+----7----+----8----+----9----+----0----+----1----+----2----+----3-|

       real                :: returnValue   !> @var Variable description [units]
       real                :: someVariable  !> @var Variable veryveryveryveryveryveryveryveryveryveryveryveryveryveryveryveryvery
                                            ! veryveryveryveryveryveryveryveryveryvery long_description [units]


!-----|--1----+----2----+----3----+----4----+----5----+----6----+----7----+----8----+----9----+----0----+----1----+----2----+----3-|
!       Main code of the function starts here
!-----|--1----+----2----+----3----+----4----+----5----+----6----+----7----+----8----+----9----+----0----+----1----+----2----+----3-|

!>    @bug Description of the stupid sticky bug that we know exist there but is not corrected yet!

      end function someFunction

!-----|--1----+----2----+----3----+----4----+----5----+----6----+----7----+----8----+----9----+----0----+----1----+----2----+----3-|
!      End of the function someFunction here
!-----|--1----+----2----+----3----+----4----+----5----+----6----+----7----+----8----+----9----+----0----+----1----+----2----+----3-|

!-----|--1----+----2----+----3----+----4----+----5----+----6----+----7----+----8----+----9----+----0----+----1----+----2----+----3-|
!      End of the module Foo_mod here
!-----|--1----+----2----+----3----+----4----+----5----+----6----+----7----+----8----+----9----+----0----+----1----+----2----+----3-|

end module Foo_mod

!-----|--1----+----2----+----3----+----4----+----5----+----6----+----7----+----8----+----9----+----0----+----1----+----2----+----3-|
!      The End of All Things (op. cit.)
!-----|--1----+----2----+----3----+----4----+----5----+----6----+----7----+----8----+----9----+----0----+----1----+----2----+----3-|
