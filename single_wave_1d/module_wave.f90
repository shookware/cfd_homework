module module_waves

  implicit none

  private
  public :: wave_t, x_1d_t

  type abstract_object
    integer :: n
  end type

  type, extends(abstract_object) :: wave_t

    real*8, allocatable ::  u(:)

  contains

    procedure :: Create=> Create_wave
    procedure :: Set=>Set_wave

  end type wave_t

  type, extends(abstract_object) :: x_1d_t

    real*8, allocatable :: x(:)

  contains

    procedure :: Create=> create_x

    !generic :: GetIndex=>get_index

    procedure, pass(this) :: GetIndex=>get_index_start_end

  end type x_1d_t


contains

  subroutine get_index_start_end(this, xstart, xend, istart, iend)

    implicit none
    class(x_1d_t), intent(in) :: this
    real(8), intent(in)::xstart, xend
    integer, intent(out) :: istart, iend
    integer :: i

    istart=-1; iend=-1
    do i=0, this%n-1
      if(this%x(i)<=xstart .and. this%x(i+1)>xstart)then
        istart=i
        exit
      endif
    enddo
    do i=0, this%n-1
      if(this%x(i)<=xend .and. this%x(i+1)>xend)then
        iend=i+1
        exit
      endif
    enddo

  end subroutine get_index_start_end

  subroutine Create_wave(this, n)

    implicit none
    class(wave_t), intent(inout) :: this
    integer, intent(in) :: n

    this%n=n
    if(.not. allocated(this%u)) then
      allocate(this%u(0:n))
    else
      if(n+1/=size(this%u))then
        deallocate(this%u)
        allocate(this%u(0:n))
      endif
    endif
    this%u=real(0, 8)

  end subroutine Create_wave

  subroutine Create_x(this, n, xstart, Lx)

    implicit none
    class(x_1d_t), intent(inout) :: this
    integer, intent(in) :: n
    real(8), intent(in) :: xstart, Lx
    integer :: i

    this%n=n
    if(.not. allocated(this%x)) then
      allocate(this%x(0:n))
    else
      if(n+1/=size(this%x))then
        deallocate(this%x)
        allocate(this%x(0:n))
      endif
    endif
    do i=0, n
      this%x(i)=real(i, 8)*Lx/real(n, 8)
    enddo

  end subroutine Create_x

  subroutine Set_wave(this, x, x_start, x_end, value)

    implicit none
    class(wave_t), intent(inout) :: this
    type(x_1d_t), intent(in) :: x
    real*8, intent(in) :: x_start
    real*8, intent(in) :: x_end
    real*8, intent(in) :: value
    integer :: istart, iend

    call x%GetIndex(x_start, x_end, istart, iend)

    if(istart==-1 .or. iend==-1) stop "please input a corrent x location"

    this%u(istart:iend)=value

  end subroutine Set_wave


end module module_waves
