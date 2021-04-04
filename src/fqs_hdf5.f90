module fqs_hdf5

    use fqs_types,     only: wp 
    use fqs_constants, only: FQS_ERROR_H5FILE_NOT_EXIST
    use fqs_constants, only: FQS_ERROR_H5FILE_NDIMS
    use h5fortran,     only: hdf5_file, HSIZE_T

    implicit none

    public save_dataset
    public load_dataset


    interface save_dataset
        procedure :: save_real_3d_dataset
        procedure :: save_real_2d_dataset
        procedure :: save_real_1d_dataset
        procedure :: save_real_scalar_dataset
    end interface save_dataset


    interface load_dataset
        procedure :: load_real_2d_dataset
        procedure :: load_real_1d_dataset
        procedure :: load_real_scalar_dataset
    end interface load_dataset



contains


    subroutine save_real_3d_dataset(filename, dataname, dataset, ierr, status, action)
        character(*), intent(in)           :: filename
        character(*), intent(in)           :: dataname
        real(wp),     intent(in)           :: dataset(:,:,:)
        integer,      intent(out)          :: ierr
        character(*), intent(in), optional :: status
        character(*), intent(in), optional :: action 

        ! Local variables
        type(hdf5_file) :: h5file
        character(len=:), allocatable      :: status_loc
        character(len=:), allocatable      :: action_loc

        status_loc = 'unknown'
        if (present(status)) then
            status_loc = status
        end if

        action_loc = 'rw'
        if (present(action)) then
            action_loc = action
        end if

        call h5file % initialize(filename, ierr, status=status_loc, action=action_loc) 
        if ( ierr /= 0 ) return 

        call h5file % write(dataname, dataset, ierr)
        if ( ierr /= 0) return

    end subroutine save_real_3d_dataset


    subroutine save_real_2d_dataset(filename, dataname, dataset, ierr, status, action)
        character(*), intent(in)           :: filename
        character(*), intent(in)           :: dataname
        real(wp),     intent(in)           :: dataset(:,:)
        integer,      intent(out)          :: ierr
        character(*), intent(in), optional :: status
        character(*), intent(in), optional :: action 

        ! Local variables
        type(hdf5_file) :: h5file
        character(len=:), allocatable      :: status_loc
        character(len=:), allocatable      :: action_loc

        status_loc = 'unknown'
        if (present(status)) then
            status_loc = status
        end if

        action_loc = 'rw'
        if (present(action)) then
            action_loc = action
        end if

        call h5file % initialize(filename, ierr, status=status_loc, action=action_loc) 
        if ( ierr /= 0 ) return 

        call h5file % write(dataname, dataset, ierr)
        if ( ierr /= 0) return

    end subroutine save_real_2d_dataset


    subroutine save_real_1d_dataset(filename, dataname, dataset, ierr, status, action)
        character(*), intent(in)           :: filename
        character(*), intent(in)           :: dataname
        real(wp),     intent(in)           :: dataset(:)
        integer,      intent(out)          :: ierr
        character(*), intent(in), optional :: status
        character(*), intent(in), optional :: action 

        ! Local variables
        type(hdf5_file) :: h5file
        character(len=:), allocatable      :: status_loc
        character(len=:), allocatable      :: action_loc

        status_loc = 'unknown'
        if (present(status)) then
            status_loc = status
        end if

        action_loc = 'rw'
        if (present(action)) then
            action_loc = action
        end if

        call h5file % initialize(filename, ierr, status=status_loc, action=action_loc) 
        if ( ierr /= 0 ) return 

        call h5file % write(dataname, dataset, ierr)
        if ( ierr /= 0) return

    end subroutine save_real_1d_dataset

    subroutine save_real_scalar_dataset(filename, dataname, dataset, ierr, status, action)
        character(*), intent(in)           :: filename
        character(*), intent(in)           :: dataname
        real(wp),     intent(in)           :: dataset
        integer,      intent(out)          :: ierr
        character(*), intent(in), optional :: status
        character(*), intent(in), optional :: action 

        ! Local variables
        type(hdf5_file) :: h5file
        character(len=:), allocatable      :: status_loc
        character(len=:), allocatable      :: action_loc

        status_loc = 'unknown'
        if (present(status)) then
            status_loc = status
        end if

        action_loc = 'rw'
        if (present(action)) then
            action_loc = action
        end if

        call h5file % initialize(filename, ierr, status=status_loc, action=action_loc) 
        if ( ierr /= 0 ) return 

        call h5file % write(dataname, dataset, ierr)
        if ( ierr /= 0) return

    end subroutine save_real_scalar_dataset


    subroutine load_real_2d_dataset(filename, dataname, dataset, ierr)
        character(*), intent(in)           :: filename
        character(*), intent(in)           :: dataname
        real(wp), intent(out), allocatable :: dataset(:,:)
        integer, intent(out)               :: ierr

        ! Local variables 
        type(hdf5_file)               :: h5file
        logical                       :: exists
        integer                       :: ndims
        integer(HSIZE_T), allocatable :: dims(:)

        call h5file % initialize(filename, ierr, status='old', action='r') 
        if ( ierr /= 0 ) return 

        exists = h5file % exists(dataname)
        if (.not. exists) then
            ierr = FQS_ERROR_H5FILE_NOT_EXIST  
            return
        end if

        ndims = h5file % ndims(dataname)
        if (ndims /= 2) then
            ierr = FQS_ERROR_H5FILE_NDIMS 
            return
        end if
        
        call h5file % shape(dataname, dims, ierr)
        if ( ierr /= 0 ) return 

        if (allocated(dataset)) then
            deallocate(dataset)
        endif
        allocate(dataset(dims(1), dims(2)), stat=ierr)
        if (ierr /= 0 ) return

        call h5file % read(dataname, dataset, ierr)
        if ( ierr /=0 ) return 

        call h5file % finalize(ierr)
        if (ierr /=0 ) return 

    end subroutine load_real_2d_dataset

    subroutine load_real_1d_dataset(filename, dataname, dataset, ierr)
        character(*), intent(in)           :: filename
        character(*), intent(in)           :: dataname
        real(wp), intent(out), allocatable :: dataset(:)
        integer, intent(out)               :: ierr

        ! Local variables 
        type(hdf5_file)               :: h5file
        logical                       :: exists
        integer                       :: ndims
        integer(HSIZE_T), allocatable :: dims(:)

        call h5file % initialize(filename, ierr, status='old', action='r') 
        if ( ierr /= 0 ) return 

        exists = h5file % exists(dataname)
        if (.not. exists) then
            ierr = FQS_ERROR_H5FILE_NOT_EXIST  
            return
        end if

        ndims = h5file % ndims(dataname)
        if (ndims /= 1) then
            ierr = FQS_ERROR_H5FILE_NDIMS 
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
        character(*), intent(in) :: filename
        character(*), intent(in) :: dataname
        real(wp), intent(out)    :: dataset
        integer, intent(out)     :: ierr

        ! Local variables
        type(hdf5_file) :: h5file
        logical         :: exists
        integer         :: ndims

        call h5file % initialize(filename, ierr, status='old', action='r') 
        if ( ierr /= 0 ) return 

        exists = h5file % exists(dataname)
        if (.not. exists) then
            ierr = FQS_ERROR_H5FILE_NOT_EXIST  
            return
        end if

        ndims = h5file % ndims(dataname)
        if ( ndims /= 0 ) then 
            ierr = FQS_ERROR_H5FILE_NDIMS 
            return 
        end if

        call h5file % read(dataname, dataset, ierr)
        if ( ierr /=0 ) return 

        call h5file % finalize(ierr)
        if (ierr /=0 ) return 

    end subroutine load_real_scalar_dataset


end module fqs_hdf5
