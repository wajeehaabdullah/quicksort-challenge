!> \file sorting_module.f90
!> \brief Module containing iterative Quicksort implementations.
!> \details Implements iterative Quicksort for Integer and Real arrays 
!> using a manual stack to avoid recursion and internal allocation.

MODULE sorting_module
  IMPLICIT NONE
  PRIVATE

  PUBLIC :: quicksort

  !> \interface quicksort
  !> \brief Generic interface for sorting arrays.
  INTERFACE quicksort
     MODULE PROCEDURE quicksort_int
     MODULE PROCEDURE quicksort_real
  END INTERFACE quicksort

CONTAINS

  !> \brief Iterative Quicksort for integer arrays.
  !> \param[in,out] array Integer array to sort in ascending order.
  !> \param[in,out] stack Work array for stack (must be size >= 2*N).
  PURE SUBROUTINE quicksort_int(array, stack)
    INTEGER, INTENT(INOUT) :: array(:)
    INTEGER, INTENT(INOUT) :: stack(:)
    
    INTEGER :: n, top, left, right, pivot_idx
    
    n = SIZE(array)
    IF (n <= 1) RETURN
    
    top = 0
    CALL stack_push_range(stack, top, 1, n)
    
    DO WHILE (top > 0)
       CALL stack_pop_range(stack, top, left, right)
       CALL partition_int(array, left, right, pivot_idx)
       CALL push_subranges(stack, top, left, right, pivot_idx)
    END DO
    
  END SUBROUTINE quicksort_int

  !> \brief Iterative Quicksort for real arrays.
  !> \param[in,out] array Real array to sort in ascending order.
  !> \param[in,out] stack Work array for stack (must be size >= 2*N).
  PURE SUBROUTINE quicksort_real(array, stack)
    REAL, INTENT(INOUT) :: array(:)
    INTEGER, INTENT(INOUT) :: stack(:)
    
    INTEGER :: n, top, left, right, pivot_idx
    
    n = SIZE(array)
    IF (n <= 1) RETURN
    
    top = 0
    CALL stack_push_range(stack, top, 1, n)
    
    DO WHILE (top > 0)
       CALL stack_pop_range(stack, top, left, right)
       CALL partition_real(array, left, right, pivot_idx)
       CALL push_subranges(stack, top, left, right, pivot_idx)
    END DO
    
  END SUBROUTINE quicksort_real

  !> \brief Partition helper for Integers (Lomuto scheme).
  PURE SUBROUTINE partition_int(array, left, right, pivot_idx)
    INTEGER, INTENT(INOUT) :: array(:)
    INTEGER, INTENT(IN) :: left, right
    INTEGER, INTENT(OUT) :: pivot_idx
    
    INTEGER :: pivot_value, i, j, temp
    
    pivot_value = array(right)
    i = left - 1
    
    DO j = left, right - 1
       IF (array(j) <= pivot_value) THEN
          i = i + 1
          temp = array(i)
          array(i) = array(j)
          array(j) = temp
       END IF
    END DO
    
    pivot_idx = i + 1
    temp = array(pivot_idx)
    array(pivot_idx) = array(right)
    array(right) = temp
  END SUBROUTINE partition_int

  !> \brief Partition helper for Reals (Lomuto scheme).
  PURE SUBROUTINE partition_real(array, left, right, pivot_idx)
    REAL, INTENT(INOUT) :: array(:)
    INTEGER, INTENT(IN) :: left, right
    INTEGER, INTENT(OUT) :: pivot_idx
    
    REAL :: pivot_value, temp
    INTEGER :: i, j
    
    pivot_value = array(right)
    i = left - 1
    
    DO j = left, right - 1
       IF (array(j) <= pivot_value) THEN
          i = i + 1
          temp = array(i)
          array(i) = array(j)
          array(j) = temp
       END IF
    END DO
    
    pivot_idx = i + 1
    temp = array(pivot_idx)
    array(pivot_idx) = array(right)
    array(right) = temp
  END SUBROUTINE partition_real

  ! --- Stack Utilities ---

  PURE SUBROUTINE stack_push_range(stack, top, left, right)
    INTEGER, INTENT(INOUT) :: stack(:)
    INTEGER, INTENT(INOUT) :: top
    INTEGER, INTENT(IN) :: left, right
    
    ! Safety check: Ensure we don't overflow the provided stack
    IF (top + 2 <= SIZE(stack)) THEN
       top = top + 1
       stack(top) = left
       top = top + 1
       stack(top) = right
    END IF
  END SUBROUTINE stack_push_range

  PURE SUBROUTINE stack_pop_range(stack, top, left, right)
    INTEGER, INTENT(INOUT) :: stack(:)
    INTEGER, INTENT(INOUT) :: top
    INTEGER, INTENT(OUT) :: left, right
    
    right = stack(top)
    top = top - 1
    left = stack(top)
    top = top - 1
  END SUBROUTINE stack_pop_range

  PURE SUBROUTINE push_subranges(stack, top, left, right, pivot_idx)
    INTEGER, INTENT(INOUT) :: stack(:)
    INTEGER, INTENT(INOUT) :: top
    INTEGER, INTENT(IN) :: left, right, pivot_idx
    
    IF (pivot_idx - 1 > left) THEN
       CALL stack_push_range(stack, top, left, pivot_idx - 1)
    END IF
    
    IF (pivot_idx + 1 < right) THEN
       CALL stack_push_range(stack, top, pivot_idx + 1, right)
    END IF
  END SUBROUTINE push_subranges

END MODULE sorting_module