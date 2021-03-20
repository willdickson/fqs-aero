module fqs_constants

    use fqs_types, only: wp

    implicit none
    
    private
    
    public pi, e_, i_ 
    
    real(wp),    parameter :: pi  = 3.1415926535897932384626433832795_wp
    real(wp),    parameter :: e_  = 2.7182818284590452353602874713527_wp
    complex(wp), parameter :: i_  = (0, 1)

end module fqs_constants
