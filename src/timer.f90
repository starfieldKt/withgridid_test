module timer_module
  implicit none
  private
  public :: start_timer, stop_timer, print_elapsed_time

  real :: start_cpu, end_cpu
  integer :: start_real, end_real
  integer :: count_rate

contains

  subroutine start_timer()
    call cpu_time(start_cpu)
    call system_clock(start_real, count_rate)
  end subroutine start_timer

  subroutine stop_timer()
    call cpu_time(end_cpu)
    call system_clock(end_real)
  end subroutine stop_timer

  subroutine print_elapsed_time()
    real :: cpu_time_elapsed, real_time_elapsed
    integer :: cpu_hours, cpu_minutes, real_hours, real_minutes
    real :: cpu_seconds, real_seconds

    cpu_time_elapsed = end_cpu - start_cpu
    real_time_elapsed = real(end_real - start_real)/real(count_rate)

    ! CPU時間を時間、分、秒に変換
    cpu_hours = int(cpu_time_elapsed/3600.0)
    cpu_minutes = int(mod(cpu_time_elapsed, 3600.0)/60.0)
    cpu_seconds = mod(cpu_time_elapsed, 60.0)

    ! 実時間を時間、分、秒に変換
    real_hours = int(real_time_elapsed/3600.0)
    real_minutes = int(mod(real_time_elapsed, 3600.0)/60.0)
    real_seconds = mod(real_time_elapsed, 60.0)

    print *, 'CPU Time Elapsed: ', cpu_hours, ' hours ', cpu_minutes, ' minutes ', cpu_seconds, ' seconds'
    print *, 'Real Time Elapsed: ', real_hours, ' hours ', real_minutes, ' minutes ', real_seconds, ' seconds'
  end subroutine print_elapsed_time

end module timer_module
