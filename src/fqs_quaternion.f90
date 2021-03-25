module fqs_quaternion

    use fqs_types,     only: wp

    implicit none
    private

    type, public :: quat_t
        real(wp) :: w = 0.0_wp
        real(wp) :: x = 0.0_wp
        real(wp) :: y = 0.0_wp
        real(wp) :: z = 0.0_wp
    contains
        generic, public :: operator(*) => multiply
        generic, public :: operator(*) => multiply_scalar_left
        generic, public :: operator(*) => multiply_scalar_right
        generic, public :: operator(/) => divide_by_scalar

        procedure, pass(self) :: multiply
        procedure, pass(self) :: multiply_scalar_left
        procedure, pass(self) :: multiply_scalar_right
        procedure, pass(self) :: divide_by_scalar

        procedure, pass(self) :: inv  => inverse 
        procedure, pass(self) :: conj => conjugate
        procedure, pass(self) :: norm => normal
        procedure, pass(self) :: sum2 => sum_of_squares
        procedure, pass(self) :: to_array => to_array
    end type quat_t

    interface quat_t
        procedure :: new_from_values
    end interface quat_t

contains


    ! quat_t class constructors
    ! -------------------------------------------------------------------------

    elemental function new_from_values(w,x,y,z) result(q)
        real(wp), intent(in) :: x, y, z, w
        type(quat_t) :: q
        q%w = w
        q%x = x
        q%y = y
        q%z = z
    end function new_from_values


    ! quaterion_t class methods
    ! -------------------------------------------------------------------------

    elemental function multiply(self,p) result(q) 
        class(quat_t), intent(in) :: self
        type(quat_t),  intent(in) :: p
        type(quat_t) :: q
        q%w = self%w * p%w - self%x * p%x - self%y * p%y - self%z * p%z
        q%x = self%w * p%x + self%x * p%w + self%y * p%z - self%z * p%y
        q%y = self%w * p%y - self%x * p%z + self%y * p%w + self%z * p%x
        q%z = self%w * p%z + self%x * p%y - self%y * p%x + self%z * p%w
    end function multiply


    elemental function multiply_scalar_right(self,scalar) result(q) 
        class(quat_t), intent(in) :: self
        real(wp),      intent(in) :: scalar 
        type(quat_t) :: q
        q%w = scalar * (self % w)
        q%x = scalar * (self % x)
        q%y = scalar * (self % y)
        q%z = scalar * (self % z)
    end function multiply_scalar_right


    elemental function multiply_scalar_left(scalar,self) result(q) 
        class(quat_t), intent(in) :: self
        real(wp),      intent(in) :: scalar 
        type(quat_t) :: q
        q%w = scalar * (self % w)
        q%x = scalar * (self % x)
        q%y = scalar * (self % y)
        q%z = scalar * (self % z)
    end function multiply_scalar_left


    elemental function divide_by_scalar(self,scalar) result(q) 
        class(quat_t), intent(in) :: self
        real(wp),      intent(in) :: scalar 
        type(quat_t) :: q
        q%w = self%w / scalar
        q%x = self%x / scalar
        q%y = self%y / scalar
        q%z = self%z / scalar
    end function divide_by_scalar


    elemental function inverse(self) result(inv)
        class(quat_t), intent(in) :: self
        type(quat_t)              :: inv 
        real(wp)                  :: sum2 
        ! ------------------------------------------------
        ! TODO: replace this with a safe division
        ! ------------------------------------------------
        sum2 = self % sum2()
        if ( sum2 >= epsilon(sum2) ) then ! avoid division by zero
            inv = ( self % conj() ) / sum2
        end if
    end function inverse


    elemental function conjugate(self) result(conj)
        class(quat_t), intent(in) :: self
        type(quat_t)              :: conj 
        conj%w =  self%w
        conj%x = -self%x
        conj%y = -self%y
        conj%z = -self%z
    end function conjugate


    elemental function normal(self) result(norm)
        class(quat_t), intent(in) :: self
        real(wp)                  :: norm
        norm = sqrt((self % w)**2 + (self % x)**2 + (self % y)**2 + (self % z)**2)
    end function normal


    elemental function sum_of_squares(self) result(sum2)
        class(quat_t), intent(in) :: self
        real(wp)                  :: sum2
        sum2 = (self % w)**2 + (self % x)**2 + (self % y)**2 + (self % z)**2
    end function sum_of_squares


    function to_array(self) result(a)
        class(quat_t), intent(in) :: self
        real(wp)                  :: a(4)
        a(1) = self % w
        a(2) = self % x
        a(3) = self % y
        a(4) = self % z
    end function to_array

end module fqs_quaternion
