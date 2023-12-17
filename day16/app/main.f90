program main
  use iso_fortran_env, only: r32 => real32
  implicit none

  integer, parameter :: N_ROWS = 110
  integer, parameter :: N_COLS = 110
  integer, parameter :: N_DIRS = 4
  integer, parameter :: N_COPIES = 2

  integer, parameter :: DIR_UP = 1
  integer, parameter :: DIR_DOWN = 2
  integer, parameter :: DIR_LEFT = 3
  integer, parameter :: DIR_RIGHT = 4

  character :: map(N_ROWS, N_COLS)

  integer :: io, i, j

  !open(newunit=io, file="data/example.txt", form="formatted", action="read")
  open(newunit=io, file="data/day16.txt", form="formatted", action="read")

  do i=1,N_ROWS
    read(io,'(110A)') map(i,:)
  end do

  close(io)

  !do i=1,N_ROWS
  !  write(*,*) map(i,:)
  !end do

  block
    integer :: part1_ans
    part1_ans = part1(map, 1, 0, DIR_RIGHT)
    write(*,*) "Part 1: ", part1_ans
  end block

  block
    integer :: part2_ans
    part2_ans = part2(map)
    write(*,*) "Part 2: ", part2_ans
  end block

contains

  integer function part2(map) result(max_energized)
    character, intent(in) :: map(N_ROWS,N_COLS)
    integer :: i, part1_ans

    max_energized = 0

    ! left side
    do i=1,N_ROWS
      part1_ans = part1(map, i, 0, DIR_RIGHT)
      max_energized = max(part1_ans, max_energized)
    end do

    ! right side
    do i=1,N_ROWS
      part1_ans = part1(map, i, N_COLS+1, DIR_LEFT)
      max_energized = max(part1_ans, max_energized)
    end do

    ! top side
    do i=1,N_COLS
      part1_ans = part1(map, 0, i, DIR_DOWN)
      max_energized = max(part1_ans, max_energized)
    end do

    ! bottom side
    do i=1,N_COLS
      part1_ans = part1(map, N_ROWS+1, i, DIR_UP)
      max_energized = max(part1_ans, max_energized)
    end do

  end function part2

  integer function part1(map, start_i, start_j, start_dir) result(total_energized)
    character, intent(in) :: map(N_ROWS,N_COLS)
    integer, intent(in) :: start_i, start_j, start_dir
    real(r32) :: grid(0:N_ROWS+1, 0:N_COLS+1, N_DIRS, N_COPIES)

    integer :: i,j, old, new
    real(r32) :: total_energy(2)

    old = 1
    new = 2

    total_energy(old) = -1
    total_energy(new) = 0

    grid = 0.0

    ! This is the laser input
    grid(start_i, start_j, start_dir, :) = 1.0

    ! yeah, I know I'm comparing floating point, this 
    ! works as long as we don't do stuff in parallel
    ! otherwise we have to check if each cell's total energy has changed
    do while (total_energy(old) /= total_energy(new))
      call stream_photons(grid, old, new)

      call collide_photons(map, grid, total_energy(new), old, new)

      call swap(old, new)
    end do

    total_energized = count( sum(grid(1:N_ROWS,1:N_COLS,:,new),3) > 0 )

  end function

  subroutine swap(a, b)
    integer, intent(inout) :: a, b
    integer :: tmp

    tmp = a
    a = b
    b = tmp
  end subroutine

  subroutine swap_r32(a, b)
    real(r32), intent(inout) :: a,b
    real(r32) :: tmp

    tmp = a
    a = b
    b = tmp
  end subroutine

  subroutine stream_photons(grid, old, new)
    real(r32), intent(inout) :: grid(0:N_ROWS+1, 0:N_COLS+1, N_DIRS, N_COPIES)
    integer, intent(in) :: old, new

    grid(:         , 1:N_COLS+1, DIR_RIGHT, new) = grid(:         , 0:N_COLS  , DIR_RIGHT, old)
    grid(:         , 0:N_COLS  , DIR_LEFT , new) = grid(:         , 1:N_COLS+1, DIR_LEFT , old)
    grid(0:N_ROWS  , :         , DIR_UP   , new) = grid(1:N_ROWS+1, :         , DIR_UP   , old)
    grid(1:N_ROWS+1, :         , DIR_DOWN , new) = grid(0:N_ROWS  , :         , DIR_DOWN , old)

  end subroutine

  subroutine collide_photons(map, grid, total_energy, old, new)
    character, intent(in) :: map(N_ROWS, N_COLS)
    real(r32), intent(inout) :: grid(0:N_ROWS+1, 0:N_COLS+1, N_DIRS, N_COPIES)
    real(r32), intent(out) :: total_energy
    integer, intent(in) :: old, new

    integer :: i,j

    total_energy = 0

    do concurrent (i=1:N_ROWS, j=1:N_COLS)
      associate( gdirs => grid(i,j,:,new) )
      total_energy = total_energy + sum(gdirs)

      select case (map(i,j))
      case ('/')
        call swap_r32( gdirs(DIR_RIGHT), gdirs(DIR_UP) )
        call swap_r32( gdirs(DIR_LEFT) , gdirs(DIR_DOWN) )
      case ('\')
        call swap_r32( gdirs(DIR_RIGHT), gdirs(DIR_DOWN) )
        call swap_r32( gdirs(DIR_LEFT) , gdirs(DIR_UP) )
      case ('-')
        gdirs(DIR_RIGHT) = gdirs(DIR_RIGHT) + 0.5*(gdirs(DIR_UP) + gdirs(DIR_DOWN))
        gdirs(DIR_LEFT)  = gdirs(DIR_LEFT)  + 0.5*(gdirs(DIR_UP) + gdirs(DIR_DOWN))
        gdirs(DIR_UP  )  = 0.0
        gdirs(DIR_DOWN)  = 0.0
      case ('|')
        gdirs(DIR_UP)    = gdirs(DIR_UP)    + 0.5*(gdirs(DIR_RIGHT) + gdirs(DIR_LEFT))
        gdirs(DIR_DOWN)  = gdirs(DIR_DOWN)  + 0.5*(gdirs(DIR_RIGHT) + gdirs(DIR_LEFT))
        gdirs(DIR_RIGHT) = 0.0
        gdirs(DIR_LEFT)  = 0.0
      case default
      end select
      end associate
    end do
  end subroutine

end program main
