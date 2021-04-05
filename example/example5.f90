program main

    use fqs,  only: wp
    use fqs,  only: vect_t
    use fqs,  only: euler_t
    use fqs,  only: rad2deg
    use fqs,  only: linspace_sub
    use fqs,  only: fake_kine
    use fqs,  only: fake_kine_param_t
    use fqs,  only: fake_param_type_1
    use fqs,  only: fake_param_type_2
    use fqs,  only: fake_param_type_3
    use fqs,  only: fake_param_type_4
    use fqs,  only: load_dataset
    use fqs,  only: save_dataset
    use fqs,  only: rotate
    use fqs,  only: vect_to_array
    use fqs,  only: wing_frame_t
    use pyplot_module, only: pyplot

    implicit none

    integer, parameter            :: num_time_pts = 200
    character(len=:), allocatable :: filename
    character(len=:), allocatable :: dataname
    character(len=:), allocatable :: outfile
    real(wp), allocatable         :: elem_pos(:)
    real(wp), allocatable         :: le_pos(:)
    real(wp), allocatable         :: te_pos(:)
    integer                       :: num_elem
    integer                       :: ierr
    real(wp)                      :: t0 
    real(wp)                      :: tn 
    type(fake_kine_param_t)       :: param
    type(wing_frame_t)            :: wing_frame(num_time_pts)
    real(wp)                      :: t(num_time_pts)
    type(euler_t)                 :: euler(num_time_pts)
    type(vect_t), allocatable     :: ax_vect(:,:) 
    type(vect_t), allocatable     :: le_vect(:,:) 
    type(vect_t), allocatable     :: te_vect(:,:) 
    real(wp), allocatable         :: ax_array(:,:,:) 
    real(wp), allocatable         :: le_array(:,:,:) 
    real(wp), allocatable         :: te_array(:,:,:) 
    real(wp)                      :: t_cpu_start
    real(wp)                      :: t_cpu_finish
    type(pyplot)                  :: plt
    real(wp)                      :: phi(num_time_pts)
    real(wp)                      :: alpha(num_time_pts)
    real(wp)                      :: theta(num_time_pts)
    integer                       :: i,j

    filename = '/home/wbd/work/programming/python/fly_aero_data/wing_data/fly_param.hdf5'

    ! Load kinematics parameters
    param = fake_param_type_4()
    t0 = 0.0_wp
    tn = 1.0_wp*param % period

    ! Load datasets
    dataname = '/wing/left/blade_element/position'
    call load_dataset(filename, dataname,  elem_pos, ierr)
    if ( ierr /= 0) error stop ': unable to load dataset: '//dataname

    dataname = '/wing/left/blade_element/chord_leading'
    call load_dataset(filename, dataname,  le_pos, ierr)
    if ( ierr /= 0) error stop ': unable to load dataset: '//dataname

    dataname = '/wing/left/blade_element/chord_trailing'
    call load_dataset(filename, dataname,  te_pos, ierr)
    if ( ierr /= 0) error stop ': unable to load dataset: '//dataname

    num_elem = size(elem_pos)
    allocate(ax_vect(num_time_pts, num_elem))
    allocate(le_vect(num_time_pts, num_elem))
    allocate(te_vect(num_time_pts, num_elem))

    allocate(ax_array(num_time_pts, num_elem, 3))
    allocate(le_array(num_time_pts, num_elem, 3))
    allocate(te_array(num_time_pts, num_elem, 3))

    call cpu_time(t_cpu_start)

    ! Get wing kinematics
    call linspace_sub(t0, tn, t)
    call fake_kine(t, euler, param=param)
    euler % bank = -(euler % bank)

    ! Rotate wing frame through kinematics
    wing_frame = rotate(wing_frame, euler)

    do i=1,num_elem
        ax_vect(:,i) = (wing_frame % u_axis)*elem_pos(i)
        le_vect(:,i) = (wing_frame % u_axis)*elem_pos(i) + (wing_frame % u_chord)*le_pos(i)
        te_vect(:,i) = (wing_frame % u_axis)*elem_pos(i) + (wing_frame % u_chord)*te_pos(i)
    end do

    ax_array = vect_to_array(ax_vect)
    le_array = vect_to_array(le_vect) 
    te_array = vect_to_array(te_vect)

    call cpu_time(t_cpu_finish)
    print *, 'dt = ', (t_cpu_finish - t_cpu_start)

    if (.false.) then 
        phi   = rad2deg(euler % heading)
        alpha = rad2deg(euler % attitude)
        theta = rad2deg(euler % bank)

        call plt % initialize(grid=.true.,xlabel='t (sec)',ylabel='(deg)')
        call plt % add_plot(t,phi,label='phi',linestyle='b',markersize=5,linewidth=2)
        call plt % add_plot(t,alpha,label='alpha',linestyle='g',markersize=5,linewidth=2)
        call plt % add_plot(t,theta,label='alpha',linestyle='r',markersize=5,linewidth=2)
        call plt % showfig()
    end if

    outfile = 'test3.hdf5'
    call save_dataset(outfile, '/ax_array', ax_array, ierr, status='new', action='w')
    call save_dataset(outfile, '/le_array', le_array, ierr, status='old', action='rw')
    call save_dataset(outfile, '/te_array', te_array, ierr, status='old', action='rw')

end program main
