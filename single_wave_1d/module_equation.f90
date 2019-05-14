module module_eqn

  !!!
  !!! 1D single_wave equation is solved
  !!!

  implicit none
  type eqn_t

    real(8) :: c !convectional velocity

  contains
    procedure :: SetC=>Set_c
    procedure :: GetRHS

  end type

contains

  subroutine Set_c(this, c)
    implicit none
    class(eqn_t), intent(inout) :: this
    real(8), intent(in) :: c

    this%c=c

  end subroutine Set_c

  subroutine GetRHS(this, u, dx, rhs)
    use module_waves
    implicit none
    class(eqn_t), intent(inout) :: this
    type(wave_t), intent(in) :: u
    real(8), intent(in) :: dx
    real(8), intent(out) :: rhs(1:u%n-1)
    integer :: n
    integer :: i

    n=u%n

    do i=1, n-1
      rhs(i)=-this%c*1.0d0/dx*(u%u(i)-u%u(i-1))
      !! rhs=-c/dx*(u_i-u_{i-1})
    enddo

  end subroutine GetRHS

end module module_eqn
