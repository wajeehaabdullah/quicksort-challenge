!> \file run_tests.f90
!> \brief Main test suite for the iterative Quicksort module.
PROGRAM run_tests
  USE sorting_module
  IMPLICIT NONE

  INTEGER :: total_tests = 0
  INTEGER :: passed_tests = 0
  INTEGER :: failed_tests = 0

  PRINT *, "============================================================"
  PRINT *, "                QUICKSORT MODULE TEST SUITE"
  PRINT *, "============================================================"

  CALL test_random_int(total_tests, passed_tests, failed_tests)
  CALL test_sorted_real(total_tests, passed_tests, failed_tests)
  CALL test_reverse_int(total_tests, passed_tests, failed_tests)
  CALL test_duplicates_int(total_tests, passed_tests, failed_tests)
  CALL test_single_element(total_tests, passed_tests, failed_tests)
  CALL test_empty_array(total_tests, passed_tests, failed_tests)

  PRINT *, "============================================================"
  PRINT *, "TEST SUMMARY"
  PRINT *, "============================================================"
  PRINT *, "Total Tests: ", total_tests
  PRINT *, "Passed:      ", passed_tests
  PRINT *, "Failed:      ", failed_tests
  PRINT *, "------------------------------------------------------------"
  IF (failed_tests == 0) PRINT *, "RESULT: SUCCESS"

CONTAINS

  ! --- Test 1: Random Integer Array ---
  SUBROUTINE test_random_int(total, passed, failed)
    INTEGER, INTENT(INOUT) :: total, passed, failed
    INTEGER, ALLOCATABLE :: arr(:), stack(:)
    INTEGER :: n = 10
    
    total = total + 1
    ALLOCATE(arr(n), stack(2*n))
    CALL fill_random(arr)
    
    PRINT *, "Test 1: Random Integer Array"
    PRINT *, "Before:", arr
    CALL quicksort(arr, stack)
    PRINT *, "After: ", arr
    
    CALL check_result(verify_int(arr), passed, failed)
    DEALLOCATE(arr, stack)
  END SUBROUTINE test_random_int

  ! --- Test 2: Already Sorted Real Array ---
  SUBROUTINE test_sorted_real(total, passed, failed)
    INTEGER, INTENT(INOUT) :: total, passed, failed
    REAL, ALLOCATABLE :: arr(:)
    INTEGER, ALLOCATABLE :: stack(:)
    INTEGER :: n = 5
    
    total = total + 1
    ALLOCATE(arr(n), stack(2*n))
    arr = [1.0, 2.0, 3.0, 4.0, 5.0]
    
    PRINT *, "Test 2: Already Sorted Real Array"
    PRINT *, "Before:", arr
    CALL quicksort(arr, stack)
    PRINT *, "After: ", arr
    
    CALL check_result(verify_real(arr), passed, failed)
    DEALLOCATE(arr, stack)
  END SUBROUTINE test_sorted_real

  ! --- Test 3: Reverse Sorted Integer Array ---
  SUBROUTINE test_reverse_int(total, passed, failed)
    INTEGER, INTENT(INOUT) :: total, passed, failed
    INTEGER, ALLOCATABLE :: arr(:), stack(:)
    INTEGER :: n = 5
    
    total = total + 1
    ALLOCATE(arr(n), stack(2*n))
    arr = [5, 4, 3, 2, 1]
    
    PRINT *, "Test 3: Reverse Sorted Integer"
    PRINT *, "Before:", arr
    CALL quicksort(arr, stack)
    PRINT *, "After: ", arr
    
    CALL check_result(verify_int(arr), passed, failed)
    DEALLOCATE(arr, stack)
  END SUBROUTINE test_reverse_int

  ! --- Test 4: Array with Duplicates ---
  SUBROUTINE test_duplicates_int(total, passed, failed)
    INTEGER, INTENT(INOUT) :: total, passed, failed
    INTEGER, ALLOCATABLE :: arr(:), stack(:)
    INTEGER :: n = 8
    
    total = total + 1
    ALLOCATE(arr(n), stack(2*n))
    arr = [3, 1, 4, 1, 5, 9, 2, 6]
    
    PRINT *, "Test 4: Duplicates"
    PRINT *, "Before:", arr
    CALL quicksort(arr, stack)
    PRINT *, "After: ", arr
    
    CALL check_result(verify_int(arr), passed, failed)
    DEALLOCATE(arr, stack)
  END SUBROUTINE test_duplicates_int

  ! --- Test 5: Single Element ---
  SUBROUTINE test_single_element(total, passed, failed)
    INTEGER, INTENT(INOUT) :: total, passed, failed
    INTEGER, ALLOCATABLE :: arr(:), stack(:)
    
    total = total + 1
    ALLOCATE(arr(1), stack(2))
    arr = [42]
    
    PRINT *, "Test 5: Single Element"
    PRINT *, "Before:", arr
    CALL quicksort(arr, stack)
    PRINT *, "After: ", arr
    
    CALL check_result(verify_int(arr), passed, failed)
    DEALLOCATE(arr, stack)
  END SUBROUTINE test_single_element

  ! --- Test 6: Empty Array ---
  SUBROUTINE test_empty_array(total, passed, failed)
    INTEGER, INTENT(INOUT) :: total, passed, failed
    INTEGER, ALLOCATABLE :: arr(:), stack(:)
    
    total = total + 1
    ALLOCATE(arr(0), stack(0))
    
    PRINT *, "Test 6: Empty Array"
    PRINT *, "Before:", arr
    CALL quicksort(arr, stack)
    PRINT *, "After: ", arr
    
    CALL check_result(verify_int(arr), passed, failed)
    DEALLOCATE(arr, stack)
  END SUBROUTINE test_empty_array

  ! --- Helpers ---

  SUBROUTINE fill_random(arr)
    INTEGER, INTENT(OUT) :: arr(:)
    REAL :: r(SIZE(arr))
    CALL RANDOM_NUMBER(r)
    arr = INT(r * 100)
  END SUBROUTINE fill_random

  FUNCTION verify_int(arr) RESULT(ok)
    INTEGER, INTENT(IN) :: arr(:)
    LOGICAL :: ok
    INTEGER :: i
    ok = .TRUE.
    IF (SIZE(arr) <= 1) RETURN
    DO i = 1, SIZE(arr)-1
       IF (arr(i) > arr(i+1)) THEN
          ok = .FALSE.
          RETURN
       END IF
    END DO
  END FUNCTION verify_int

  FUNCTION verify_real(arr) RESULT(ok)
    REAL, INTENT(IN) :: arr(:)
    LOGICAL :: ok
    INTEGER :: i
    ok = .TRUE.
    IF (SIZE(arr) <= 1) RETURN
    DO i = 1, SIZE(arr)-1
       IF (arr(i) > arr(i+1)) THEN
          ok = .FALSE.
          RETURN
       END IF
    END DO
  END FUNCTION verify_real

  SUBROUTINE check_result(success, passed, failed)
    LOGICAL, INTENT(IN) :: success
    INTEGER, INTENT(INOUT) :: passed, failed
    IF (success) THEN
       PRINT *, "Status: PASS"
       passed = passed + 1
    ELSE
       PRINT *, "Status: FAIL"
       failed = failed + 1
    END IF
    PRINT *
  END SUBROUTINE check_result

END PROGRAM run_tests