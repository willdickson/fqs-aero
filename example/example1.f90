program main

    use h5fortran,     only: hdf5_file, HSIZE_T
    use pyplot_module, only: pyplot
    use fqs,           only: wp 

    implicit none

    character(len=:), allocatable :: filename
    character(len=:), allocatable :: dataname
    real(wp), allocatable         :: be_position(:)
    real(wp), allocatable         :: chord_le(:)
    real(wp), allocatable         :: chord_te(:)
    integer                       :: ierr
    type(pyplot)                  :: plt

    filename = '/home/wbd/work/programming/python/fly_aero_data/wing_data/fly_param.hdf5'

    dataname = '/wing/left/blade_element/position'
    call load_real_1d_dataset(filename, dataname,  be_position, ierr)
    if ( ierr /= 0) error stop ': unable to load dataset: '//dataname

    dataname = '/wing/left/blade_element/chord_leading'
    call load_real_1d_dataset(filename, dataname,  chord_le, ierr)
    if ( ierr /= 0) error stop ': unable to load dataset: '//dataname

    dataname = '/wing/left/blade_element/chord_trailing'
    call load_real_1d_dataset(filename, dataname,  chord_te, ierr)
    if ( ierr /= 0) error stop ': unable to load dataset: '//dataname

    call plt % initialize(grid=.true.,xlabel='pos',ylabel='chord',axis_equal=.true.)
    call plt % add_plot(be_position,chord_le,label='chord le',linestyle='b',markersize=5,linewidth=2)
    call plt % add_plot(be_position,chord_te,label='chord te',linestyle='b',markersize=5,linewidth=2)
    call plt % showfig()

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
