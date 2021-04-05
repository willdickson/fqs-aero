module fqs_wing_frame

    use fqs_types,   only: wp
    use fqs_vector,  only: vect_t
    use fqs_vector,  only: e1, e2, e3

    implicit none

    type, public :: wing_frame_t
        type(vect_t) :: u_axis  = e3
        type(vect_t) :: u_chord = e2
        type(vect_t) :: u_norm  = e1
    contains
        procedure :: add => add_vect
        procedure :: sub => sub_vect
    end type wing_frame_t

contains

    elemental function add_vect(self,v) result(new_frame)
        class(wing_frame_t), intent(in) :: self
        type(vect_t),        intent(in) :: v
        type(wing_frame_t)              :: new_frame
        new_frame % u_axis  = (self % u_axis)  + v
        new_frame % u_chord = (self % u_chord) + v
        new_frame % u_norm  = (self % u_norm)  + v
    end function add_vect


    elemental function sub_vect(self,v) result(new_frame)
        class(wing_frame_t), intent(in) :: self
        type(vect_t),        intent(in) :: v
        type(wing_frame_t)              :: new_frame
        new_frame % u_axis  = (self % u_axis)  - v
        new_frame % u_chord = (self % u_chord) - v
        new_frame % u_norm  = (self % u_norm)  - v
    end function sub_vect

end module fqs_wing_frame
