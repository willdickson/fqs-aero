module fqs_axis_angle

    use fqs_types,  only: wp
    use fqs_vector, only: vect_t

    implicit none

    type, public :: axis_angle_t
        type(vect_t) :: axis
        real(wp)     :: angle  = 0.0_wp 
    end type axis_angle_t

    interface axis_angle_t
        procedure :: new_from_values
    end interface axis_angle_t

contains

    elemental function new_from_values(axis, angle) result(axis_angle)
        type(vect_t), intent(in) :: axis
        real(wp),     intent(in) :: angle
        type(axis_angle_t)       :: axis_angle
        axis_angle % axis = axis
        axis_angle % angle = angle
    end function new_from_values


end module fqs_axis_angle
