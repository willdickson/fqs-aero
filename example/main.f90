program main

    use h5fortran,     only: hdf5_file, HSIZE_T
    use pyplot_module, only:  pyplot
    use fqs,           only: wp 
    use fqs,           only: pi
    use fqs,           only: quat_t
    use fqs,           only: vect_t
    use fqs,           only: euler_t
    use fqs,           only: linspace_fcn
    use fqs,           only: rotate

    implicit none


    integer,  parameter           :: num_pts = 500 
    real(wp), parameter           :: period = 5.0_wp 
    type(vect_t)                  :: axis
    real(wp)                      :: angle 
    real(wp), allocatable         :: t(:)
    type(euler_t), allocatable    :: euler(:)
    type(euler_t), allocatable    :: euler_rot(:)

    type(pyplot)                  :: plt
    integer                       :: i

    allocate(t(num_pts))
    allocate(euler(num_pts))

    t = linspace_fcn(0.0_wp, 10.0_wp, num_pts)

    angle = -45.0_wp*pi/180_wp
    print *, angle
    axis = vect_t(1.0_wp, 0.0_wp, 1.0_wp)

    euler % heading = 0.5_wp*pi*cos(2.0*pi*t/period) 
    euler_rot = rotate(euler, axis, angle)

    do i=1,num_pts
        print *, euler(i) % heading, euler_rot(i) % heading
    end do


    call plt % initialize(grid=.true.,xlabel='t (sec)',ylabel='heading (deg)')
    call plt % add_plot(t, euler % heading, label='chord le',linestyle=':b',markersize=5,linewidth=2)
    call plt % add_plot(t, euler_rot % heading, label='chord le',linestyle='b',markersize=5,linewidth=2)
    call plt % add_plot(t, euler % attitude, label='chord le',linestyle=':r',markersize=5,linewidth=2)
    call plt % add_plot(t, euler_rot % attitude, label='chord le',linestyle='r',markersize=5,linewidth=2)
    call plt % showfig()

    ! ----------------------------------------------------------
    !character(len=:), allocatable :: filename
    !character(len=:), allocatable :: dataname
    !real(wp), allocatable         :: be_position(:)
    !real(wp), allocatable         :: chord_le(:)
    !real(wp), allocatable         :: chord_te(:)
    !integer                       :: ierr

    !filename = '/home/wbd/work/programming/python/fly_aero_data/wing_data/fly_param.hdf5'

    !dataname = '/wing/left/blade_element/position'
    !call load_real_1d_dataset(filename, dataname,  be_position, ierr)
    !if ( ierr /= 0) error stop ': unable to load dataset: '//dataname

    !dataname = '/wing/left/blade_element/chord_leading'
    !call load_real_1d_dataset(filename, dataname,  chord_le, ierr)
    !if ( ierr /= 0) error stop ': unable to load dataset: '//dataname

    !dataname = '/wing/left/blade_element/chord_trailing'
    !call load_real_1d_dataset(filename, dataname,  chord_te, ierr)
    !if ( ierr /= 0) error stop ': unable to load dataset: '//dataname

    !call plt % initialize(grid=.true.,xlabel='pos',ylabel='chord',axis_equal=.true.)
    !call plt % add_plot(be_position,chord_le,label='chord le',linestyle='b',markersize=5,linewidth=2)
    !call plt % add_plot(be_position,chord_te,label='chord te',linestyle='b',markersize=5,linewidth=2)
    !call plt % showfig()


    ! ---------------------------------------------------------
    !integer, parameter        :: num_pts = 10
    !type(vect_t), allocatable :: v(:)
    !type(vect_t), allocatable :: w(:)
    !type(vect_t), allocatable :: u(:)
    !type(vect_t)              :: axis
    !real(wp)                  :: angle 
    !type(euler_t)             :: euler 
    !integer :: i

    !allocate(v(num_pts))
    !allocate(w(num_pts))
    !allocate(u(num_pts))

    !angle = -0.5_wp*pi
    !axis = vect_t(0.0_wp, 1.0_wp, 0.0_wp)

    !euler % heading = 0.0 
    !print *, euler
    !euler = rotate(euler, axis, angle)
    !print *, euler

    !v % x = linspace_fcn(0.0_wp, 10.0_wp, num_pts)
    !w = rotate(v,axis,angle)
    !u = rotate(v,euler)

    !print *, ' '
    !do i = 1,size(v)
    !    print *, 'v', v(i)
    !    print *, 'w', w(i)
    !    print *, 'u', u(i)
    !    print *, ' '
    !end do

    ! -----------------------------------------------------------

contains

    subroutine load_real_1d_dataset(filename, dataname, dataset, ierr)
        character(*), intent(in)             :: filename
        character(*), intent(in)             :: dataname
        real(wp), intent(inout), allocatable :: dataset(:)
        integer, intent(inout)               :: ierr

        ! locad variables
        type(hdf5_file)               :: h5file
        logical                       :: exists
        integer                       :: ndims
        integer(HSIZE_T), allocatable :: dims(:)

        call h5file % initialize(filename, ierr, status='old', action='r') 
        if ( ierr /= 0 ) return 

        exists = h5file % exists(dataname)
        if (.not. exists) then
            ! Maybe set some type of error code?
            ierr = 1  
            return
        end if

        ndims = h5file % ndims(dataname)
        if (ndims /= 1) then
            ! Maybe set some type of error code?
            ierr = 1
            return
        end if
        
        call h5file % shape(dataname, dims, ierr)
        if ( ierr /= 0 ) return 

        if (allocated(dataset)) then
            deallocate(dataset)
        endif
        allocate(dataset(dims(1)), stat=ierr)
        if (ierr /= 0 ) return

        call h5file % read(dataname, dataset, ierr)
        if ( ierr /=0 ) return 

        call h5file % finalize(ierr)
        if (ierr /=0 ) return 
    end subroutine load_real_1d_dataset


    subroutine load_real_scalar_dataset(filename, dataname, dataset, ierr)
        character(*), intent(in)             :: filename
        character(*), intent(in)             :: dataname
        real(wp), intent(inout)              :: dataset
        integer, intent(inout)               :: ierr

        ! locad variables
        type(hdf5_file) :: h5file
        logical         :: exists
        integer         :: ndims

        call h5file % initialize(filename, ierr, status='old', action='r') 
        if ( ierr /= 0 ) return 

        exists = h5file % exists(dataname)
        if (.not. exists) then
            ! Maybe set some type of error code?
            ierr = 1  
            return
        end if

        ndims = h5file % ndims(dataname)
        if ( ndims /= 0 ) then 
            ierr = 1
            return 
        end if

        call h5file % read(dataname, dataset, ierr)
        if ( ierr /=0 ) return 

        call h5file % finalize(ierr)
        if (ierr /=0 ) return 
    end subroutine load_real_scalar_dataset

end program main
