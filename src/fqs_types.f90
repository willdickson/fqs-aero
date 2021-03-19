module fqs_types 

    use, intrinsic :: iso_fortran_env, only : sp=>real32, dp=>real64, ip=>int32

    implicit none

    integer, parameter :: wp = dp 

    public wp, sp, dp, ip

end module fqs_types 
