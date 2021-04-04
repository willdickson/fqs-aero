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
    use pyplot_module, only: pyplot

    implicit none

    integer, parameter            :: num_time_pts = 200
    character(len=:), allocatable :: filename
    character(len=:), allocatable :: dataname
    real(wp), allocatable         :: elem_pos(:)
    real(wp), allocatable         :: le_pos(:)
    real(wp), allocatable         :: te_pos(:)
    real(wp), allocatable         :: phi(:)
    real(wp), allocatable         :: alpha(:)
    real(wp), allocatable         :: theta(:)
    type(pyplot)                  :: plt
    real(wp), allocatable         :: t(:)
    type(euler_t), allocatable    :: euler(:,:)
    type(vect_t), allocatable     :: ax_vects(:,:)
    type(vect_t), allocatable     :: le_vects(:,:)
    type(vect_t), allocatable     :: te_vects(:,:)
    real(wp), allocatable         :: ax_array(:,:,:)
    real(wp), allocatable         :: le_array(:,:,:)
    real(wp), allocatable         :: te_array(:,:,:)
    real(wp)                      :: t0 
    real(wp)                      :: tn 
    integer                       :: num_be
    integer                       :: i,j
    integer                       :: ierr
    type(fake_kine_param_t)       :: param
    real(wp)                      :: t_cpu_start
    real(wp)                      :: t_cpu_finish

    allocate(phi(num_time_pts))
    allocate(alpha(num_time_pts))
    allocate(theta(num_time_pts))
    allocate(t(num_time_pts))

    param = fake_param_type_4()
    t0 = 0.0_wp
    tn = 1.0_wp*param % period

    filename = '/home/wbd/work/programming/python/fly_aero_data/wing_data/fly_param.hdf5'

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

    ! Create wing position vectors
    num_be = size(elem_pos)
    allocate(ax_vects(num_time_pts,num_be))
    allocate(le_vects(num_time_pts,num_be))
    allocate(te_vects(num_time_pts,num_be))
    allocate(euler(num_time_pts,num_be))
    allocate(ax_array(num_time_pts,num_be,3))
    allocate(le_array(num_time_pts,num_be,3))
    allocate(te_array(num_time_pts,num_be,3))

    ! Get wing kinematics
    call linspace_sub(t0, tn, t)
    call fake_kine(t, euler(:,1), param=param)
    euler(:,2:) = spread(euler(:,1), 2, num_be-1)

    do i=1,num_time_pts
        ax_vects(i,:) % z = elem_pos
        le_vects(i,:) % z = elem_pos
        te_vects(i,:) % z = elem_pos
        le_vects(i,:) % y = le_pos 
        te_vects(i,:) % y = te_pos 
    end do

    call cpu_time(t_cpu_start)

    ax_vects = rotate(ax_vects, euler)
    le_vects = rotate(ax_vects, euler)
    te_vects = rotate(ax_vects, euler)

    ax_array = vect_to_array(ax_vects)
    le_array = vect_to_array(le_vects) 
    te_array = vect_to_array(te_vects)
    
    call cpu_time(t_cpu_finish)
    print *, (t_cpu_finish - t_cpu_start)

    if (.false.) then
        phi = rad2deg(euler(:,1) % heading)
        alpha = rad2deg(euler(:,1) % attitude)
        theta = rad2deg(euler(:,1) % bank)

        call plt % initialize(grid=.true.,xlabel='t (sec)',ylabel='(deg)')
        call plt % add_plot(t,phi,label='phi',linestyle='b',markersize=5,linewidth=2)
        call plt % add_plot(t,alpha,label='alpha',linestyle='g',markersize=5,linewidth=2)
        call plt % add_plot(t,theta,label='alpha',linestyle='r',markersize=5,linewidth=2)
        call plt % showfig()
    end if

    call save_dataset('test1.hdf5', '/ax_array', ax_array, ierr, status='new', action='w')
    call save_dataset('test1.hdf5', '/le_array', le_array, ierr, status='old', action='rw')
    call save_dataset('test1.hdf5', '/te_array', te_array, ierr, status='old', action='rw')

contains

end program main 
