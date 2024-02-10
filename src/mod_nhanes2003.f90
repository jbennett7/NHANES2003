module mod_nhanes2003
    use csv_module
    use iso_fortran_env, only : wp => real64

    implicit none
    private

    public :: get_females

contains

    subroutine get_females(filepath, fseqn)
        character(len=*) :: filepath
        integer, dimension(:), allocatable, intent(out) :: fseqn
        type(csv_file) :: f
        character(len=30), dimension(:), allocatable :: header
        integer, dimension(:), allocatable :: seqn, riagendr
        logical :: status_ok
        integer, dimension(:), allocatable :: itypes
        integer :: i, count

        call f%read(filepath, header_row=1, status_ok=status_ok)

        call f%get_header(header, status_ok)
        call f%variable_types(itypes, status_ok)

        call f%get(1, seqn, status_ok)
        call f%get(5, riagendr, status_ok)
        count = 0
        do i = 1, size(riagendr)
            if (riagendr(i) == 2) then
                count = count + 1
            end if
        end do
        allocate(fseqn(count))
        count = 0
        do i = 1, size(seqn)
            if (riagendr(i) == 2) then
                count = count + 1
                fseqn(count) = seqn(i)
            end if
        end do
        call f%destroy()
    end subroutine get_females
end module mod_nhanes2003
