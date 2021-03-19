module fqs_conversions 

    use fqs_types,        only: wp
    use fqs_vector,       only: vect_t
    use fqs_quaternion,   only: quat_t
    use fqs_euler_angle,  only: euler_t
    use fqs_constants,    only: pi 

    implicit none
    private

    public :: quat_from_vect
    public :: vect_from_quat
    public :: quat_from_euler
    public :: quat_from_axis_angle
    public :: euler_from_quat
    public :: test_convert
    
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
        real(wp), parameter       :: half = 0.5_wp
        real(wp)                  :: ch, ca, cb
        real(wp)                  :: sh, sa, sb
        ch = cos(half * (angles % heading))
        ca = cos(half * (angles % attitude))
        cb = cos(half * (angles % bank))
        sh = sin(half * (angles % heading))
        sa = sin(half * (angles % attitude))
        sb = sin(half * (angles % bank))
        q % w = (ch * ca * cb) - (sh * sa * sb)
        q % x = (sh * sa * cb) + (ch * ca * sb)
        q % y = (sh * ca * cb) + (ch * sa * sb)
        q % z = (ch * sa * cb) - (sh * ca * sb)
    end function quat_from_euler


    elemental function quat_from_axis_angle(axis, angle) result(q)
        type(vect_t), intent(in)  :: axis
        real(wp),     intent(in)  :: angle 
        type(quat_t)              :: q
        real(wp), parameter       :: half  = 0.5_wp
        q % w = cos( half * angle )
        q % x = (axis % x) * sin(half * angle)
        q % y = (axis % y) * sin(half * angle)
        q % z = (axis % z) * sin(half * angle)
    end function quat_from_axis_angle


    elemental function euler_from_quat(q,thresh_input) result(angles)
        type(quat_t), intent(in)       :: q
        real(wp), intent(in), optional :: thresh_input
        real(wp), parameter            :: thresh_default = 0.4999_wp
        type(euler_t)                  :: angles
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
            angles % attitude = asin( ( 2.0_wp*test ) / sum2)
            angles % bank     = atan2( 2.0_wp * (qwx - qyz), -qxx + qyy - qzz + qww ) 
        end if
    end function euler_from_quat

    subroutine test_convert()
        type(quat_t)  :: q
        type(quat_t)  :: p
        type(euler_t) :: angles
        type(euler_t) :: angles_2
        type(vect_t)  :: axis
        real(wp)      :: theta = 0.5_wp * pi

        angles = euler_t(pi/2.0,0.0,0.0)
        axis = vect_t(0.0, 1.0, 0.0)

        q = quat_from_euler(angles)
        p = quat_from_axis_angle(axis,theta)

        angles_2 = euler_from_quat(q)

        print *, q
        print *, p
        print *, angles
        print *, angles_2
    end subroutine test_convert

end module fqs_conversions
