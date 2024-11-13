program withgridid_test

  use iric
  use timer_module
  implicit none

  !==========================================================================================
  ! 変数の定義
  !==========================================================================================

  !> ループカウンタ
  integer:: i, j, k

  !> error code
  integer:: ier
  !> CGNSファイル名を読み込む関数で使用する変数
  integer:: icount, istatus
  !> 計算が途中でキャンセルされたかを確認するための変数
  integer:: is_canceled

  !> CGNSファイル名
  character(len=256):: cgns_file_name

  !> 計算データ(CGNSファイル)のファイルIDなど
  integer:: fid

  !> 格子のid(2D)
  integer:: gid_2d = 1
  !> 格子のid(3D)
  integer:: gid_3d

  !> 格子点数 (2D)
  integer:: i_size, j_size, k_size
  !> 格子点数 (2D)
  integer:: n_node_2d
  !> 格子点数 (3D)
  integer:: n_node_3d

  !> 格子の座標 (2D)
  double precision, dimension(:, :), allocatable:: x_array_2d, y_array_2d
  !> 格子の座標 (3D)
  double precision, dimension(:, :, :), allocatable:: x_array_3d, y_array_3d, z_array_3d

  !> ノードの値を格納する2次元配列
  double precision, dimension(:, :), allocatable:: elevation_bottom_2d, elevation_top_2d, node_valeu_2d
  !> ノードの値を格納する3次元配列
  double precision, dimension(:, :, :), allocatable:: node_valeu_3d

  !> 時間
  double precision:: time = 0.0
  !> 時間ステップ
  double precision:: time_step
  !> 計算終了時間
  double precision:: time_end

  !> ステップ数
  integer:: num_steps
  !> 現在のステップ
  integer:: step
  !> 振動の周期
  double precision:: period

  !> 円周率
  double precision, parameter :: pi = 3.14159265358979323846

  !> 各座標軸の最小値と最大値
  double precision:: x_min, x_max, y_min, y_max, z_min, z_max
  !> 中心点の座標
  double precision:: x_center, y_center, z_center

  !> 基準点の座標
  double precision:: x_ref, y_ref, z_ref
  !> 各軸の振幅
  double precision:: x_amplitude, y_amplitude, z_amplitude
  !> 各軸の距離
  double precision:: dx, dy, dz

  write (*, '(a81)') '==================================program Start=================================='
  call start_timer()
  !==========================================================================================
  ! 計算データ(CGNSファイル)を開く
  !==========================================================================================
  !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  ! (Intel Fortran Compiler用)
  !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  write (*, *) '>>Start Opening CGNS File'

  ! コマンド名が数に含まれるので、引数が1つなら2を返す
  icount = nargs()

  ! ファイル名の取得
  if (icount == 2) then
    call getarg(1, cgns_file_name, istatus)
  else
    write (*, *) "Input File not specified."
    stop
  end if

  ! 計算データ(CGNSファイル)を開く
  call cg_iric_open(cgns_file_name, IRIC_MODE_MODIFY, fid, ier)
  if (ier /= 0) stop "*** Open error of CGNS file ***"

  !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  ! (debug用)
  !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

  ! cgns_file_name = "test.cgn"
  ! call cg_iric_open(cgns_file_name, IRIC_MODE_MODIFY, fid, ier)
  ! if (ier /= 0) stop "*** Open error of CGNS file ***"

  write (*, '(a)') '    >Completed Opening CGNS File'

  call cg_iRIC_Clear_Sol(fid, ier)

  !強制終了用の割り込み処理
  call iric_initoption(IRIC_OPTION_CANCEL, ier)

  !==========================================================================================
  ! 計算条件の読み込み
  !==========================================================================================
  write (*, *) '>>Start loading calculation conditions'

  call cg_iric_read_integer(fid, "k_size", k_size, ier)
  call cg_iric_read_real(fid, "time_step", time_step, ier)
  call cg_iric_read_real(fid, "time_end", time_end, ier)
  call cg_iric_read_real(fid, "period", period, ier)

  write (*, '(a)') '    >Completed loading calculation conditions'

  !==========================================================================================
  ! 格子の読み込み
  !==========================================================================================
  write (*, *) '>>Start loading Grid conditions'

  ! 格子のサイズを取得
  call cg_iric_read_grid2d_str_size(fid, i_size, j_size, ier)

  !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  ! メモリの確保
  !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  allocate (x_array_2d(i_size, j_size), y_array_2d(i_size, j_size))
  allocate (elevation_bottom_2d(i_size, j_size), elevation_top_2d(i_size, j_size))

  !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  ! 格子の座標と標高の読み込み
  !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  call cg_iric_read_grid2d_coords(fid, x_array_2d, y_array_2d, ier)
  call cg_iric_read_grid_real_node(fid, "elevation_bottom", elevation_bottom_2d, ier)
  call cg_iric_read_grid_real_node(fid, "elevation_top", elevation_top_2d, ier)

  ! トップの標高が底の標高よりも低い場合のエラーチェック
  if (minval(elevation_top_2d - elevation_bottom_2d) <= 0.0) then
    write (*, '(a)') 'Error: Top elevation is less than or equal to bottom elevation in some locations.'
    write (*, '(a)') 'Please ensure that the bottom elevation is lower than the top elevation.'
    stop "*** Elevation error ***"
  end if

  write (*, '(a)') '    >Completed loading Grid conditions'

  !==========================================================================================
  ! 3次元格子の作成と書き込み
  !==========================================================================================

  !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  ! メモリの確保
  !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  allocate (x_array_3d(i_size, j_size, k_size), y_array_3d(i_size, j_size, k_size), z_array_3d(i_size, j_size, k_size))
  allocate (node_valeu_3d(i_size, j_size, k_size))

  !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  ! 格子点座標の計算
  !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  do k = 1, k_size
    do j = 1, j_size
      do i = 1, i_size
        x_array_3d(i, j, k) = x_array_2d(i, j)
        y_array_3d(i, j, k) = y_array_2d(i, j)
        z_array_3d(i, j, k) = elevation_bottom_2d(i, j) + (elevation_top_2d(i, j) - elevation_bottom_2d(i, j))*(k - 1)/(k_size - 1)
      end do
    end do
  end do

  !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  ! 3D格子の書き込み
  ! 2Dの格子はすでに作成されているので、ここでは3Dの格子のみ書き込む
  ! gid_3dは自動で割り当てられる(最初からある2Dのgidが1なので次に書き込まれた3次元格子のgidは2になる)
  !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  call cg_iric_write_grid3d_coords_withgridid(fid, i_size, j_size, k_size, x_array_3d, y_array_3d, z_array_3d, gid_3d, ier)

  !==========================================================================================
  ! 各格子点の中心点からの距離を計算
  !==========================================================================================

  ! 各座標軸の最小値と最大値を取得
  x_min = minval(x_array_3d)
  x_max = maxval(x_array_3d)
  y_min = minval(y_array_3d)
  y_max = maxval(y_array_3d)
  z_min = minval(z_array_3d)
  z_max = maxval(z_array_3d)

  ! 中心点の座標を計算
  x_center = (x_min + x_max)/2.0
  y_center = (y_min + y_max)/2.0
  z_center = (z_min + z_max)/2.0

  ! 各軸の振幅を計算
  x_amplitude = (x_max - x_min)/2.0
  y_amplitude = (y_max - y_min)/2.0
  z_amplitude = (z_max - z_min)/2.0

  !==========================================================================================
  ! メインループ
  !==========================================================================================
  num_steps = int(time_end/time_step)

  do step = 0, num_steps

    !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    ! キャンセルされたかを確認
    !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    call iric_check_cancel(is_canceled)
    if (is_canceled == 1) then
      write (*, *) "Solver is stopped because the STOP button was clicked."
      write (*, '(a81)') '*********************************** Finish !! ***********************************'
      call cg_iric_close(fid, ier)
      call stop_timer()
      call print_elapsed_time()
      stop
    end if

    !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    ! 格子点の値を計算
    !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

    ! 現在の時間を計算
    time = step*time_step

    ! 基準点の座標を計算 (各軸ごとに異なる周期で変化)
    x_ref = x_center + x_amplitude*sin(6.0*pi*time/period)  ! x軸はz軸の3倍の周期
    y_ref = y_center + y_amplitude*sin(4.0*pi*time/period)  ! y軸はz軸の2倍の周期
    z_ref = z_center + z_amplitude*sin(2.0*pi*time/period)  ! z軸の周期

    ! 距離を計算して保存
    !$OMP PARALLEL DO PRIVATE(i, j, k, dx, dy, dz)
    do k = 1, k_size
      do j = 1, j_size
        do i = 1, i_size
          dx = x_array_3d(i, j, k) - x_ref
          dy = y_array_3d(i, j, k) - y_ref
          dz = z_array_3d(i, j, k) - z_ref
          node_valeu_3d(i, j, k) = sqrt(dx**2 + dy**2 + dz**2)
        end do
      end do
    end do
    !$OMP END PARALLEL DO

    ! 3Dグリッドの底の値を抽出してnode_valeu_2dに格納
    node_valeu_2d = node_valeu_3d(:, :, 1)

    ! 時間ごとの出力を開始
    call cg_iric_write_sol_start(fid, ier)
    ! 時間を出力
    call cg_iric_write_sol_time(fid, time, ier)

    ! 3Dグリッドの値を出力
    call cg_iric_write_sol_node_real_withgridid(fid, gid_3d, "NodeValue", node_valeu_3d, ier)

    ! 2Dグリッドの値を出力
    call cg_iric_write_sol_node_real_withgridid(fid, gid_2d, "NodeValue2D", node_valeu_2d, ier)
    ! 2Dグリッドの標高を出力
    call cg_iric_write_sol_node_real_withgridid(fid, gid_2d, "Elevation", elevation_bottom_2d, ier)

    ! 時間ごとの出力を終了
    call cg_iric_write_sol_end(fid, ier)

    ! コンソールにタイムステップと出力した旨を出力
    write (*, '(a, f8.2, a)') 'Time step: ', time, ' - Output written.'
    ! 計算結果の再読み込みが要求されていれば出力を行う
    call cg_iRIC_Check_Update(fid, ier)

  end do

  !==========================================================================================
  ! 終了処理
  !==========================================================================================
  write (*, '(a81)') '================================Completed process================================'
  ! 計算データファイルを閉じる
  call cg_iric_close(fid, ier)
  call stop_timer()
  call print_elapsed_time()

  write (*, '(a81)') '*********************************** Finish !! ***********************************'

end program withgridid_test
