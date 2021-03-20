module fqs_rotations

    use fqs_types,       only: wp
    use fqs_vector,      only: vect_t
    use fqs_quaternion,  only: quat_t
    use fqs_euler_angle, only: euler_t
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
    end interface rotate


contains 


    elemental function rotate_euler_by_axis_angle(euler, axis, angle) result(euler_new)
        ! Rotates euler angles using and axis angle rotatation
        type(euler_t), intent(in) :: euler
        type(vect_t),  intent(in) :: axis
        real(wp),      intent(in) :: angle
        type(euler_t)             :: euler_new

        ! Local variables 
        type(quat_t)               :: q_euler
        type(quat_t)               :: q_euler_new
        type(quat_t)               :: q_rot
        type(quat_t)               :: q_rot_inv

        q_euler = quat_from_euler(euler)
        q_rot = quat_from_axis_angle(axis,angle) 
        q_rot_inv = q_rot%inv()
        q_euler_new = q_rot * (q_euler * q_rot_inv)
        euler_new = euler_from_quat(q_euler_new)
    end function rotate_euler_by_axis_angle


    elemental function rotate_vect_by_axis_angle(v, axis, angle) result(w)
        ! Rotates a vector using an axis and angle rotation
        type(vect_t), intent(in)   :: v
        type(vect_t), intent(in)   :: axis
        real(wp),     intent(in)   :: angle
        type(vect_t)               :: w 

        ! Local variables
        type(quat_t)               :: q_v
        type(quat_t)               :: q_w
        type(quat_t)               :: q_rot
        type(quat_t)               :: q_rot_inv

        q_v = quat_from_vect(v)
        q_rot = quat_from_axis_angle(axis,angle) 
        q_rot_inv = q_rot%inv()
        q_w = q_rot * (q_v * q_rot_inv)
        w = vect_from_quat(q_w)
    end function rotate_vect_by_axis_angle


    elemental function rotate_vect_by_euler(v, angles) result(w)
        ! Rotates a vector using euler angles
        type(vect_t),  intent(in)  :: v
        type(euler_t), intent(in)  :: angles 
        type(vect_t)               :: w

        ! Local variables
        type(quat_t)               :: q_v
        type(quat_t)               :: q_w
        type(quat_t)               :: q_rot
        type(quat_t)               :: q_rot_inv

        q_v = quat_from_vect(v)
        q_rot = quat_from_euler(angles) 
        q_rot_inv = q_rot%inv()
        q_w = q_rot * (q_v * q_rot_inv)
        w = vect_from_quat(q_w)
    end function rotate_vect_by_euler


end module fqs_rotations
