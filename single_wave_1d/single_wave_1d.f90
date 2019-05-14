program single_wave_1d

  use module_1d_solver
  use module_eqn
  implicit none
  type(solver_1d_type) :: solver
  type(eqn_t) :: eqn
  real(8), parameter :: LX=2.0d0     !!!设长度为2
  integer, parameter :: N=100     !!!设流向分成100个网格
  real(8), parameter :: Xstart=0.0d0
  real(8), parameter :: Xend=1.0d0
  real(8), parameter :: Uvalue=1.0d0
  real(8), parameter :: CFL=0.7d0
  real(8), parameter :: C=0.2d0  !对流项系数
  real(8), parameter :: TIME=1.0d0 !求解时间
  real(8), parameter :: ZERO=0.0d0

!!!!初始化
  call solver%Initial(N, LX, Xstart, Xend, Uvalue)
  call eqn%setC(C)
  call solver%SetEqn(Eqn)
  call solver%SetCFL(CFL)
  call solver%SetBC(Uvalue, ZERO)
  call solver%Advance(TIME)
  call solver%Print()

end program single_wave_1d
