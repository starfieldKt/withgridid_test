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
    integer :: cpu_days, cpu_hours, cpu_minutes, real_days, real_hours, real_minutes
    real :: cpu_seconds, real_seconds

    cpu_time_elapsed = end_cpu - start_cpu
    real_time_elapsed = real(end_real - start_real)/real(count_rate)

    ! CPU時間を日、時間、分、秒に変換
    cpu_days = int(cpu_time_elapsed/86400.0)
    cpu_hours = int(mod(cpu_time_elapsed, 86400.0)/3600.0)
    cpu_minutes = int(mod(cpu_time_elapsed, 3600.0)/60.0)
    cpu_seconds = mod(cpu_time_elapsed, 60.0)

    ! 実時間を日、時間、分、秒に変換
    real_days = int(real_time_elapsed/86400.0)
    real_hours = int(mod(real_time_elapsed, 86400.0)/3600.0)
    real_minutes = int(mod(real_time_elapsed, 3600.0)/60.0)
    real_seconds = mod(real_time_elapsed, 60.0)

    write (*, '(A20, I3, A, I2, A, I2, A, F5.3, A)') 'CPU Time Elapsed: ', cpu_days, ' days ', cpu_hours, ' hours ', cpu_minutes, ' minutes ', cpu_seconds, ' seconds'
    write (*, '(A20, I3, A, I2, A, I2, A, F5.3, A)') 'Real Time Elapsed: ', real_days, ' days ', real_hours, ' hours ', real_minutes, ' minutes ', real_seconds, ' seconds'
  end subroutine print_elapsed_time
end module timer_module
