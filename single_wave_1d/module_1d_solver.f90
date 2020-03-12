module module_1d_solver

  use module_waves
  use module_eqn
  implicit none
  type solver_1d_type
    type(x_1d_t) :: space
    type(wave_t) :: wave
    type(eqn_t), pointer :: eqn
    real(8) :: dx
    real(8) :: cfl
    real(8) :: bc(2)
  contains
    procedure :: Initial
    procedure :: SetCFL
    procedure :: SetEqn
    procedure :: SetBC
    procedure :: Advance
    procedure :: Print=>print_x_u

    procedure, private :: GetDt

  end type solver_1d_type

contains

  subroutine SetBC(this,bcl, bcr)
    implicit none
    class(solver_1d_type), intent(inout) :: this
    real(8), intent(in) :: bcl
    real(8), intent(in) :: bcr

    this%bc(1)=bcl; this%bc(2)=bcr

  end subroutine

  subroutine initial(this, n, Lx, Xstart, Xend, Uvalue)
    implicit none
    class(solver_1d_type), intent(inout) :: this
    integer, intent(in) :: n
    real(8), intent(in) :: Lx
    real(8), intent(in) :: Xstart, Xend, Uvalue

    call this%space%Create(n, Xstart, LX)
    call this%wave%Create(n)
    call this%wave%Set(this%space, Xstart, Xend, Uvalue)
    this%dx=this%space%x(1)-this%space%x(0)

  end subroutine initial

  subroutine SetCFL(this, cfl)

    implicit none
    class(solver_1d_type), intent(inout) :: this
    real(8), intent(in) :: cfl

    this%cfl=cfl

  end subroutine

  subroutine SetEqn(this, Eqn)
    implicit none
    class(solver_1d_type), intent(inout) :: this
    type(eqn_t), target, intent(in) :: Eqn

    this%Eqn=>Eqn

  end subroutine

  function GetDt(this) result(dt)
    implicit none
    class(solver_1d_type), intent(inout) :: this
    real(8) :: dt

    dt=this%cfl*this%dx/this%eqn%c  !! dt=cfl*dx/c

  end function GetDt

  subroutine Advance(this, time)

    implicit none

    class(solver_1d_type), intent(inout) :: this
    real(8), intent(in) :: time
    real(8) :: t0, dt
    real(8) :: rhs(1:this%wave%n-1)
    integer :: n

    t0=0.d0
    rhs=0.0d0
    n=this%wave%n
    do while (t0<= time)
      call this%eqn%GetRHS(this%wave, this%dx, rhs) !计算右端项
      dt=this%GetDt() !时间步长
      !print*, dt
      !print*, rhs
      !pause

      !时间推进
      this%wave%u(1:n-1)=this%wave%u(1:n-1)+dt*rhs  !!! u^{n+1}=u^{n}+dt*RHS
      
      !this%wave%u(0)=this%bc(1) !! updating the b.c.
      !this%wave%u(n)=this%bc(2)
      t0=t0+dt
    enddo

  end subroutine Advance

  subroutine print_x_u(this)
    implicit none
    class(solver_1d_type), intent(inout) :: this
    integer :: ios
    integer :: i

    open(unit=10, file='x_u.plt', iostat=ios, action="write")
    if ( ios /= 0 ) stop "Error opening file "
      do i=0, this%space%n
        write(10, *)this%space%x(i), this%wave%u(i)
      enddo
    close(10)

  end subroutine print_x_u

end module module_1d_solver
