program main

    use fqs, only: wp 
    use fqs, only: pi
    use fqs, only: quat_t
    use fqs, only: vect_t
    use fqs, only: euler_t
    use fqs, only: axis_angle_t
    use fqs, only: linspace_fcn
    use fqs, only: rotate

    implicit none

    integer, parameter        :: num_pts = 10
    type(vect_t), allocatable :: v(:)
    type(vect_t), allocatable :: w(:)
    type(vect_t), allocatable :: u(:)
    type(axis_angle_t)        :: axis_angle
    type(euler_t)             :: euler 
    integer :: i

    allocate(v(num_pts))
    allocate(w(num_pts))
    allocate(u(num_pts))

    axis_angle % angle = -0.5_wp*pi
    axis_angle % axis = vect_t(0.0_wp, 1.0_wp, 0.0_wp)

    euler % heading = 0.0 
    print *, euler
    euler = rotate(euler, axis_angle)
    print *, euler

    v % x = linspace_fcn(0.0_wp, 10.0_wp, num_pts)
    w = rotate(v,axis_angle)
    !u = rotate(v,euler)

    print *, ' '
    do i = 1,size(v)
        print *, 'v', v(i)
        print *, 'w', w(i)
        !print *, 'u', u(i)
        print *, ' '
    end do

end program main
