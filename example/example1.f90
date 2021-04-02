program main

    use pyplot_module, only: pyplot
    use fqs,           only: wp 
    use fqs,           only: load_dataset

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
    call load_dataset(filename, dataname,  be_position, ierr)
    if ( ierr /= 0) error stop ': unable to load dataset: '//dataname

    dataname = '/wing/left/blade_element/chord_leading'
    call load_dataset(filename, dataname,  chord_le, ierr)
    if ( ierr /= 0) error stop ': unable to load dataset: '//dataname

    dataname = '/wing/left/blade_element/chord_trailing'
    call load_dataset(filename, dataname,  chord_te, ierr)
    if ( ierr /= 0) error stop ': unable to load dataset: '//dataname

    call plt % initialize(grid=.true.,xlabel='pos',ylabel='chord',axis_equal=.true.)
    call plt % add_plot(be_position,chord_le,label='chord le',linestyle='b',markersize=5,linewidth=2)
    call plt % add_plot(be_position,chord_te,label='chord te',linestyle='b',markersize=5,linewidth=2)
    call plt % showfig()


end program main
