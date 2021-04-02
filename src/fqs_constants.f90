module fqs_constants

    use fqs_types, only: wp

    implicit none
    
    private
    
    public pi, e_, i_ 

    public FQS_ERROR_H5FILE_EXISTS
    public FQS_ERROR_H5FILE_NDIMS
    
    real(wp),    parameter :: pi  = 3.1415926535897932384626433832795_wp
    real(wp),    parameter :: e_  = 2.7182818284590452353602874713527_wp
    complex(wp), parameter :: i_  = (0, 1)

    integer,     parameter :: FQS_ERROR_H5FILE_EXISTS = 100
    integer,     parameter :: FQS_ERROR_H5FILE_NDIMS  = 101

end module fqs_constants
