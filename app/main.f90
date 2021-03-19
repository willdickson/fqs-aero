program main

    use fqs,  only: wp 
    use fqs,  only: pi
    use fqs,  only: quat_t
    use fqs,  only: vect_t
    use fqs,  only: euler_t
    use fqs,  only: linspace_fcn
    use fqs,  only: rotate

    implicit none

    integer, parameter        :: num_pts = 10
    type(vect_t), allocatable :: v(:)
    type(vect_t), allocatable :: w(:)
    type(vect_t), allocatable :: u(:)
    type(vect_t)              :: axis
    real(wp)                  :: angle 
    type(euler_t)             :: euler 
    integer :: i

    allocate(v(num_pts))
    allocate(w(num_pts))
    allocate(u(num_pts))

    angle = -pi/2.0_wp
    axis = vect_t(0.0_wp, 1.0_wp, 0.0_wp)

    euler % heading = angle

    v % x = linspace_fcn(0.0_wp, 10.0_wp, num_pts)
    w = rotate(v,axis,angle)
    u = rotate(v,euler)

    print *, ' '
    do i = 1,size(v)
        print *, 'v', v(i)
        print *, 'w', w(i)
        print *, 'u', u(i)
        print *, ' '
    end do

end program main
