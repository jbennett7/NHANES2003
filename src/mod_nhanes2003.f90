module mod_nhanes2003
    use csv_module
    use iso_fortran_env, only : wp => real64

    implicit none
    private

    public :: get_females

contains

    subroutine get_females(filepath, fseqn)
        character(len=*), intent(in) :: filepath
        integer, dimension(:), allocatable, intent(out) :: fseqn
        type(csv_file) :: f
        logical :: status_ok
        integer, dimension(:), allocatable :: seqn, riagendr

        call f%read(filepath, header_row=1, status_ok=status_ok)
        call f%get(1, seqn, status_ok)
        call f%get(5, riagendr, status_ok)
        fseqn = pack(seqn, riagendr == 2)
        call f%destroy()
    end subroutine get_females
end module mod_nhanes2003
