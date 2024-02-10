module test_nhanes2003
    use mod_nhanes2003
    use testdrive, only : error_type, unittest_type, new_unittest, check
    implicit none
    private

    public :: collect_nhanes2003

contains

    subroutine collect_nhanes2003(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)
        testsuite = [&
            new_unittest("Get Females", test_females) &
        ]
    end subroutine

    subroutine test_females(error)
        type(error_type), allocatable, intent(out) :: error
        integer, dimension(:), allocatable :: fseqn
        call get_females("./data/DEMO_C.CSV", fseqn)
        call check(error, size(fseqn) == 5152)
        if(allocated(fseqn)) deallocate(fseqn)
        if (allocated(error)) return
    end subroutine test_females

end module test_nhanes2003

