module fqs_rotations

    use fqs_types,       only: wp
    use fqs_vector,      only: vect_t
    use fqs_quaternion,  only: quat_t
    use fqs_euler_angle, only: euler_t
    use fqs_axis_angle,  only: axis_angle_t
    use fqs_wing_frame,  only: wing_frame_t
    use fqs_conversions, only: quat_from_vect
    use fqs_conversions, only: vect_from_quat
    use fqs_conversions, only: quat_from_euler
    use fqs_conversions, only: euler_from_quat
    use fqs_conversions, only: quat_from_axis_angle
    use fqs_conversions, only: quat_from_euler

    implicit none

    private

    public :: rotate

    interface rotate
        procedure :: rotate_euler_by_axis_angle
        procedure :: rotate_vect_by_axis_angle
        procedure :: rotate_vect_by_euler
        procedure :: rotate_wing_frame_by_euler
        procedure :: rotate_wing_frame_by_axis_angle
    end interface rotate

contains 


    elemental function rotate_euler_by_axis_angle(euler, axis_angle) result(euler_new)
        ! Rotates euler angles using and axis angle rotatation
        type(euler_t),      intent(in) :: euler
        type(axis_angle_t), intent(in) :: axis_angle
        type(euler_t)                  :: euler_new

        ! Local variables 
        type(quat_t)               :: q_euler
        type(quat_t)               :: q_euler_new
        type(quat_t)               :: q_rot
        type(quat_t)               :: q_rot_inv

        q_euler = quat_from_euler(euler)
        q_rot = quat_from_axis_angle(axis_angle) 
        q_rot_inv = q_rot%inv()
        q_euler_new = q_rot * (q_euler * q_rot_inv)
        euler_new = euler_from_quat(q_euler_new)
    end function rotate_euler_by_axis_angle


    elemental function rotate_vect_by_axis_angle(v, axis_angle) result(w)
        ! Rotates a vector using an axis and angle rotation
        type(vect_t),       intent(in) :: v
        type(axis_angle_t), intent(in) :: axis_angle
        type(vect_t)                   :: w 

        ! Local variables
        type(quat_t)               :: q_v
        type(quat_t)               :: q_w
        type(quat_t)               :: q_rot
        type(quat_t)               :: q_rot_inv

        q_v = quat_from_vect(v)
        q_rot = quat_from_axis_angle(axis_angle) 
        q_rot_inv = q_rot%inv()
        q_w = q_rot * (q_v * q_rot_inv)
        w = vect_from_quat(q_w)
    end function rotate_vect_by_axis_angle


    elemental function rotate_vect_by_euler(v, euler) result(w)
        ! Rotates a vector using euler euler
        type(vect_t),  intent(in)  :: v
        type(euler_t), intent(in)  :: euler 
        type(vect_t)               :: w

        ! Local variables
        type(quat_t)               :: q_v
        type(quat_t)               :: q_w
        type(quat_t)               :: q_rot
        type(quat_t)               :: q_rot_inv

        q_v = quat_from_vect(v)
        q_rot = quat_from_euler(euler) 
        q_rot_inv = q_rot%inv()
        q_w = q_rot * (q_v * q_rot_inv)
        w = vect_from_quat(q_w)
    end function rotate_vect_by_euler


    elemental function rotate_wing_frame_by_euler(frame, euler) result(frame_rot)
        ! Rotates a wing referene frame using euler angle
        type(wing_frame_t), intent(in) :: frame
        type(euler_t),      intent(in) :: euler
        type(wing_frame_t)             :: frame_rot
        frame_rot % u_axis  = rotate_vect_by_euler(frame % u_axis,  euler)
        frame_rot % u_chord = rotate_vect_by_euler(frame % u_chord, euler)
        frame_rot % u_norm  = rotate_vect_by_euler(frame % u_norm,  euler)
    end function rotate_wing_frame_by_euler


    elemental function rotate_wing_frame_by_axis_angle(frame, axis_angle) result(frame_rot)
        ! Rotates a wing referene frame using an axis and angle rotation
        type(wing_frame_t), intent(in) :: frame
        type(axis_angle_t), intent(in) :: axis_angle 
        type(wing_frame_t)             :: frame_rot
        frame_rot % u_axis  = rotate_vect_by_axis_angle(frame % u_axis,  axis_angle)
        frame_rot % u_chord = rotate_vect_by_axis_angle(frame % u_chord, axis_angle)
        frame_rot % u_norm  = rotate_vect_by_axis_angle(frame % u_norm,  axis_angle)
    end function rotate_wing_frame_by_axis_angle



end module fqs_rotations
