module fqs_conversions 

    use fqs_types,        only: wp
    use fqs_vector,       only: vect_t
    use fqs_quaternion,   only: quat_t
    use fqs_euler_angle,  only: euler_t
    use fqs_axis_angle,   only: axis_angle_t
    use fqs_constants,    only: pi 
    use fqs_utility,      only: check_division

    implicit none
    private

    public :: quat_from_vect
    public :: quat_from_euler
    public :: quat_from_axis_angle
    public :: vect_from_quat
    public :: axis_angle_from_quat
    public :: axis_angle_from_euler
    public :: euler_from_quat

contains


    elemental function quat_from_vect(v) result(q)
        type(vect_t), intent(in) :: v
        type(quat_t)             :: q
        q % w = 0
        q % x = v % x
        q % y = v % y
        q % z = v % z
    end function quat_from_vect


    elemental function vect_from_quat(q) result(v)
        type(quat_t), intent(in) :: q
        type(vect_t)             :: v
        v % x = q % x 
        v % y = q % y 
        v % z = q % z 
    end function vect_from_quat


    elemental function quat_from_euler(angles) result(q)
        type(euler_t), intent(in) :: angles
        type(quat_t)              :: q

        ! Local variables
        real(wp)                  :: ch, ca, cb
        real(wp)                  :: sh, sa, sb
        ch = cos(0.5_wp * (angles % heading))
        ca = cos(0.5_wp * (angles % attitude))
        cb = cos(0.5_wp * (angles % bank))
        sh = sin(0.5_wp * (angles % heading))
        sa = sin(0.5_wp * (angles % attitude))
        sb = sin(0.5_wp * (angles % bank))
        q % w = (ch * ca * cb) - (sh * sa * sb)
        q % x = (sh * sa * cb) + (ch * ca * sb)
        q % y = (sh * ca * cb) + (ch * sa * sb)
        q % z = (ch * sa * cb) - (sh * ca * sb)
    end function quat_from_euler


    elemental function quat_from_axis_angle(axis_angle) result(q)
        type(axis_angle_t), intent(in) :: axis_angle
        type(quat_t)                   :: q

        q % w = cos( 0.5_wp * (axis_angle % angle) )
        q % x = (axis_angle % axis % x) * sin(0.5_wp * (axis_angle % angle))
        q % y = (axis_angle % axis % y) * sin(0.5_wp * (axis_angle % angle))
        q % z = (axis_angle % axis % z) * sin(0.5_wp * (axis_angle % angle))
    end function quat_from_axis_angle


    elemental function axis_angle_from_quat(q) result(axis_angle)
        type(quat_t), intent(in)  :: q
        type(axis_angle_t)        :: axis_angle

        ! Local variables
        real(wp)     :: norm
        real(wp)     :: denom
        type(quat_t) :: qnorm

        norm = q % norm()
        if ( check_division(q, norm) ) then
            qnorm = q / norm
            axis_angle % angle = 2.0_wp*acos(qnorm % w)
            axis_angle % axis = vect_from_quat(qnorm)
            denom = sqrt(1.0_wp - (qnorm % w)*(qnorm % w))
            if ( check_division(axis_angle % axis, denom) ) then
                axis_angle % axis = ( (axis_angle % axis) ) / denom
            else
                axis_angle % axis = vect_t(1.0_wp, 0.0_wp, 0.0_wp)
            end if
        else
            axis_angle % angle = 0.0_wp
            axis_angle % axis = vect_t(1.0_wp, 0.0_wp, 0.0_wp)
        end if
    end function axis_angle_from_quat


    elemental function axis_angle_from_euler(euler) result(axis_angle)
        type(euler_t), intent(in) :: euler
        type(axis_angle_t)        :: axis_angle

        ! Local variables
        type(quat_t) :: q

        q = quat_from_euler(euler)
        axis_angle = axis_angle_from_quat(q)
    end function axis_angle_from_euler


    elemental function euler_from_quat(q,thresh_input) result(angles)
        type(quat_t), intent(in)       :: q
        real(wp), intent(in), optional :: thresh_input
        type(euler_t)                  :: angles

        ! Local variables
        real(wp), parameter            :: thresh_default = 0.4999_wp
        real(wp)                       :: thresh
        real(wp)                       :: test
        real(wp)                       :: sum2
        real(wp)                       :: qww 
        real(wp)                       :: qwx 
        real(wp)                       :: qwy 
        real(wp)                       :: qwz 
        real(wp)                       :: qxx 
        real(wp)                       :: qxy 
        real(wp)                       :: qxz 
        real(wp)                       :: qyy 
        real(wp)                       :: qyz 
        real(wp)                       :: qzz
        
        if (.not. present(thresh_input)) then
            thresh = thresh_default
        end if

        ! Compute products
        qww = (q % w) * (q % w)
        qwx = (q % w) * (q % x)
        qwy = (q % w) * (q % y)
        qwz = (q % w) * (q % z)
        qxx = (q % x) * (q % x)
        qxy = (q % x) * (q % y)
        qxz = (q % x) * (q % z)
        qyy = (q % y) * (q % y)
        qyz = (q % y) * (q % z)
        qzz = (q % z) * (q % z)

        ! Values for checking for sigularity at north or south pole
        sum2 = qww + qxx + qyy + qzz
        test = qxy + qwz

        if ( abs(test) > ( thresh * sum2 ) ) then
            ! Sigularity at north/south poles
            angles % heading  = 2.0_wp * sign( 1.0_wp, test ) * atan2( q % x, q % w )
            angles % attitude = 0.5_wp * sign( 1.0_wp, test ) * pi
            angles % bank     = 0.0_wp
        else
            angles % heading  = atan2( 2.0_wp * (qwy - qxz), qxx - qyy - qzz + qww )
            angles % attitude = asin(( 2.0_wp * test) / sum2)
            angles % bank     = atan2( 2.0_wp * (qwx - qyz), -qxx + qyy - qzz + qww ) 
        end if
    end function euler_from_quat

end module fqs_conversions
