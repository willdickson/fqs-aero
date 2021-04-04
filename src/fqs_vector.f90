module fqs_vector

    use fqs_types,       only: wp
    use fqs_quaternion,  only: quat_t

    implicit none
    private

    type, public :: vect_t
        real(wp) :: x = 0.0_wp
        real(wp) :: y = 0.0_wp
        real(wp) :: z = 0.0_wp
    contains
        generic, public :: operator(+) => add 
        generic, public :: operator(-) => sub
        generic, public :: operator(*) => mul_left 
        generic, public :: operator(*) => mul_right 
        generic, public :: operator(/) => divide_by_scalar

        procedure, pass(self) :: add
        procedure, pass(self) :: sub 
        procedure, pass(self) :: mul_left
        procedure, pass(self) :: mul_right
        procedure, pass(self) :: divide_by_scalar

        procedure, pass(self) :: norm => normal
        procedure, pass(self) :: sum2 => sum_of_squares
        procedure, pass(self) :: to_array => to_array
    end type vect_t

    interface vect_t
        procedure :: new_from_values
    end interface vect_t

    interface vect_to_array
        procedure :: single_vect_to_array
        procedure :: array_1d_of_vect_to_array
        procedure :: array_2d_of_vect_to_array
    end interface vect_to_array

    public vect_dot, vect_cross, vect_to_array

contains

    ! vect_t constructors
    ! -------------------------------------------------------------------------

    elemental function new_from_values(x,y,z) result(v)
        real(wp), intent(in) :: x, y, z
        type(vect_t)         :: v
        v % x = x 
        v % y = y
        v % z = z 
    end function new_from_values

    ! vect_t methods
    ! -------------------------------------------------------------------------

    elemental function add(self,v) result(w)
        class(vect_t), intent(in) :: self
        type(vect_t),  intent(in) :: v
        type(vect_t)              :: w
        w % x = self % x + v % x
        w % y = self % y + v % y
        w % z = self % z + v % z
    end function add


    elemental function sub(self,v) result(w)
        class(vect_t), intent(in) :: self
        type(vect_t),  intent(in) :: v
        type(vect_t)              :: w
        w % x = self % x - v % x
        w % y = self % y - v % y
        w % z = self % z - v % z
    end function sub


    elemental function mul_left(scalar,self) result(w)
        class(vect_t), intent(in) :: self
        real(wp),      intent(in) :: scalar
        type(vect_t)              :: w
        w % x = scalar * (self % x)
        w % y = scalar * (self % y)
        w % z = scalar * (self % z)
    end function mul_left


    elemental function mul_right(self,scalar) result(w)
        class(vect_t), intent(in) :: self
        real(wp),      intent(in) :: scalar
        type(vect_t)              :: w
        w % x = scalar * (self % x)
        w % y = scalar * (self % y)
        w % z = scalar * (self % z)
    end function mul_right


    elemental function divide_by_scalar(self,scalar) result(v) 
        class(vect_t), intent(in) :: self
        real(wp), intent(in)      :: scalar 
        type(vect_t) :: v
        v%x = self%x / scalar
        v%y = self%y / scalar
        v%z = self%z / scalar
    end function divide_by_scalar


    elemental function normal(self) result(norm)
        class(vect_t), intent(in) :: self
        real(wp)                  :: norm
        norm = sqrt( self % sum2() )
    end function normal


    elemental function sum_of_squares(self) result(sum2)
        class(vect_t), intent(in) :: self
        real(wp)                  :: sum2
        sum2 = (self % x)**2 + (self % y)**2 + (self % z)**2
    end function sum_of_squares


    function to_array(self) result(a)
        class(vect_t), intent(in) :: self
        real(wp)                  :: a(3)
        a(1) = self % x
        a(2) = self % y
        a(3) = self % z
    end function to_array


    ! utility functions
    ! ---------------------------------------------------------------------------

    elemental function vect_dot(v,w) result(val)
        ! Computes the dot product of vectors v and w
        type(vect_t), intent(in) :: v
        type(vect_t), intent(in) :: w
        real(wp)                 :: val 
        val = (v % x)*(w % x) + (v % y)*(w % y) + (v % z)*(w % z)
    end function vect_dot


    elemental function vect_cross(v,w) result(u)
        ! Computes the cross product of vectors v and w
        type(vect_t), intent(in) :: v
        type(vect_t), intent(in) :: w
        type(vect_t)             :: u
        u % x = ((v % y) * (w % z)) - ((v % z)*(w % y))
        u % y = ((v % z) * (w % x)) - ((v % x)*(w % z))
        u % z = ((v % x) * (w % y)) - ((v % y)*(w % x))
    end function vect_cross


    function single_vect_to_array(v) result(a)
        type(vect_t), intent(in) :: v
        real(wp)                 :: a(3)
        a = v % to_array()
    end function single_vect_to_array


    function array_1d_of_vect_to_array(v) result(a)
        type(vect_t), intent(in) :: v(:)
        real(wp)                 :: a(size(v),3) 
        integer                  :: i
        do i=1,size(v)
            a(i,:) = v(i) % to_array() 
        end do
    end function array_1d_of_vect_to_array


    function array_2d_of_vect_to_array(v) result(a)
        type(vect_t), intent(in) :: v(:,:)
        real(wp)                 :: a(size(v,1),size(v,2),3) 
        integer                  :: i,j
        do i=1,size(v,1)
            do j=1,size(v,2)
                a(i,j,:) = v(i,j) % to_array()
            end do
        end do
    end function array_2d_of_vect_to_array

end module fqs_vector
