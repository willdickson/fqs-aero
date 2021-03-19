module fqs_euler_angle

    use fqs_types, only: wp

    implicit none

    private

    ! ---------------------------------------------------------------------------
    !
    ! Euler angle convention - similar to NASA standard airplane, but with y and 
    ! z axes swapped to conform with x3d. (in order of application)
    !     1.0 rotation about y-axis
    !     2.0 rotation about z-axis
    !     3.0 rotation about x-axis
    !
    ! ---------------------------------------------------------------------------

    type, public :: euler_t
        real(wp) :: heading  = 0.0_wp 
        real(wp) :: attitude = 0.0_wp
        real(wp) :: bank     = 0.0_wp
    end type euler_t

    interface euler_t
        procedure :: new_from_values
    end interface euler_t

contains

    elemental function new_from_values(heading, attitude, bank) result(angles)
        real(wp), intent(in) :: heading, attitude, bank
        type(euler_t)        :: angles
        angles % heading = heading
        angles % attitude = attitude
        angles % bank = bank
    end function new_from_values

end module fqs_euler_angle
