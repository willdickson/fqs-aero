program main

    use h5fortran,     only: hdf5_file, HSIZE_T
    use pyplot_module, only:  pyplot
    use fqs,           only: wp 
    use fqs,           only: pi
    use fqs,           only: quat_t
    use fqs,           only: vect_t
    use fqs,           only: euler_t
    use fqs,           only: axis_angle_t
    use fqs,           only: linspace_fcn
    use fqs,           only: rotate

    implicit none


    integer,  parameter           :: num_pts = 500 
    real(wp), parameter           :: period = 5.0_wp 
    type(axis_angle_t)            :: axis_angle

    real(wp),      allocatable    :: t(:)
    real(wp),      allocatable    :: tmp(:)
    type(euler_t), allocatable    :: euler(:)
    type(euler_t), allocatable    :: euler_rot(:)
    type(pyplot)                  :: plt

    allocate(t(num_pts))
    allocate(tmp(num_pts))
    allocate(euler(num_pts))

    t = linspace_fcn(0.0_wp, 10.0_wp, num_pts)

    axis_angle % angle = -45.0_wp*pi/180_wp
    axis_angle % axis = vect_t(1.0_wp, 0.0_wp, 1.0_wp)
    
    euler % heading = 0.5_wp*pi*cos(2.0*pi*t/period) 
    euler_rot = rotate(euler, axis_angle)

    call plt % initialize(grid=.true.,xlabel='t (sec)',ylabel='heading (deg)')

    tmp = euler % heading
    call plt % add_plot(t, tmp, label='chord le',linestyle=':b',markersize=5,linewidth=2)

    tmp = euler_rot % heading
    call plt % add_plot(t, tmp, label='chord le',linestyle='b',markersize=5,linewidth=2)

    tmp = euler % attitude
    call plt % add_plot(t, tmp, label='chord le',linestyle=':r',markersize=5,linewidth=2)

    tmp = euler_rot % attitude
    call plt % add_plot(t, tmp, label='chord le',linestyle='r',markersize=5,linewidth=2)

    call plt % showfig()

end program main
