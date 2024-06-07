!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!
!! Copyright (c) 2022  Neil N. Carlson
!!
!! Permission is hereby granted, free of charge, to any person obtaining a
!! copy of this software and associated documentation files (the "Software"),
!! to deal in the Software without restriction, including without limitation
!! the rights to use, copy, modify, merge, publish, distribute, sublicense,
!! and/or sell copies of the Software, and to permit persons to whom the
!! Software is furnished to do so, subject to the following conditions:
!!
!! The above copyright notice and this permission notice shall be included
!! in all copies or substantial portions of the Software.
!!
!! THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
!! IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
!! FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
!! THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
!! LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
!! FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
!! DEALINGS IN THE SOFTWARE.
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

module index_map_type

  use coarray_collectives
  implicit none
  private

  type, public :: index_map
    integer :: onp_size = 0    ! number of indices assigned to this process (on-process)
    integer :: offp_size = 0   ! number of off-process indices referenced from this process
    integer :: local_size = 0  ! number of local indices (on and off-process)
    integer :: global_size = 0 ! size of the global index set
    integer :: first        ! first global index of the range assigned to this process
    integer :: last         ! last global index of the range assigned to this process
    integer, allocatable :: offp_index(:), send_index(:)
    integer, allocatable :: send_displs(:), recv_displs(:), counts(:), send_images(:)
  contains
    procedure :: init
    procedure :: gather
    procedure :: global_index
  end type

contains

  subroutine init(this, bsize, offp_index)

    class(index_map), intent(out) :: this
    integer, intent(in) :: bsize, offp_index(:)

    integer :: nproc

    nproc = num_images()

    this%onp_size = bsize
    this%offp_size = 0
    this%local_size = this%onp_size + this%offp_size
    call co_sum_scan(bsize, this%last)
    this%first = this%last - this%onp_size + 1
    this%global_size = this%last
    call co_broadcast(this%global_size, nproc)

    call add_offp_index(this, offp_index)

  end subroutine init

  subroutine add_offp_index(this, offp_index)

    class(index_map), intent(inout), target :: this
    integer, intent(in) :: offp_index(:)

    integer :: nproc, i, j, j1, n
    integer, allocatable :: offp_image(:), send_counts(:), recv_displs(:)
    integer, allocatable :: last(:)[:], recv_counts(:)[:], send_displs(:)[:]

    type box
      integer, pointer :: data(:)
    end type
    type(box), allocatable :: buffer[:]

    !TODO: ensure offp_index is strictly increasing

    this%offp_index = offp_index
    this%offp_size  = size(this%offp_index)
    this%local_size = this%onp_size + this%offp_size

    nproc = num_images()

    !! Determine the image that owns each off-process index (OFFP_IMAGE).
    !! OFFP_IMAGE will be ordered if OFFP_INDEX is ordered (exploited later).
    allocate(last(nproc)[*], offp_image(this%offp_size))
    last(this_image())[1] = this%last
    sync all
    call co_broadcast(last, 1)
    i = 1
    do j = 1, size(this%offp_index)
      do while (this%offp_index(j) > last(i))
        i = i + 1
        !ASSERT(i <= nproc)
      end do
      if (i == this_image()) stop 1 !TODO: replace by assertion
      offp_image(j) = i
    end do

    !! We know which images we will need to receive indexed data from, but not
    !! the corresponding info of which images we will need to send indexed data.
    !! The next step is to communicate that info.

    !! Count the number of off-process indices received from each image
    !! (RECV_COUNTS). This relies on the OFFP_IMAGE array being ordered.
    call move_alloc(last, recv_counts)  ! reuse coarray
    j1 = 1
    do i = 1, nproc
      do j = j1, size(offp_image)
        if (offp_image(j) > i) exit
      end do
      recv_counts(i) = j - j1
      j1 = j
    end do
    deallocate(offp_image)

    !! Distribute the number of indices received from each image to that image.
    !! The result is the number of indices sent to each image (SEND_COUNTS).
    allocate(send_counts(nproc))
    sync all
    do i = 1, nproc
      send_counts(i) = recv_counts(this_image())[i]
    end do
    n = sum(send_counts)
    allocate(this%send_index(n))

    !! We now know how many indices are being sent to/received from each image.
    !! Generate the displacements into the index data arrays for the start of
    !! the data for each image. These are exclusive sum scans of the count arrays.
    allocate(recv_displs(nproc), send_displs(nproc)[*])
    recv_displs(1) = 0
    send_displs(1) = 0
    do i = 2, nproc
      recv_displs(i) = recv_displs(i-1) + recv_counts(i-1)
      send_displs(i) = send_displs(i-1) + send_counts(i-1)
    end do
    deallocate(send_counts)
    sync all

    !! Compress the RECV_COUNTS and RECV_DISPLS arrays, dropping images where
    !! no data is sent, and generate the corresponding list of images that
    !! send us data (SEND_IMAGES). SEND_DISPLS are the corresponding
    !! displacements into the send index data arrays *on the remote images*.
    n = count(recv_counts > 0)
    allocate(this%send_images(n), this%counts(n), this%recv_displs(n), this%send_displs(n))
    n = 0
    do i = 1, nproc
      if (recv_counts(i) > 0) then
        n = n + 1
        this%send_images(n) = i
        this%counts(n) = recv_counts(i)
        this%recv_displs(n) = recv_displs(i)
        this%send_displs(n) = send_displs(this_image())[i]
      end if
    end do
    deallocate(send_displs, recv_displs, recv_counts)

    !! The components %COUNTS, %SEND_DISPLS, %RECV_DISPLS, and %SEND_IMAGES
    !! initialized here establish the coarray communication pattern used by
    !! GATHER_AUX. The component %SEND_INDEX genererated next will be used to
    !! fill the send buffer; the receive buffer will be the off-process data
    !! array itself.

    !! Communicate the global off-process indices to their owning images.
    allocate(buffer[*])
    buffer%data => this%send_index
    sync all
    do j = 1, size(this%counts)
      associate (i => this%send_images(j), n => this%counts(j), &
                 sd => this%send_displs(j), rd => this%recv_displs(j))
        buffer[i]%data(sd+1:sd+n) = this%offp_index(rd+1:rd+n)
      end associate
    end do
    sync all
    this%send_index = this%send_index - this%first + 1  ! map to local indices
    !ASSERT(all(this%send_index >= 1 .and. this%send_index <= this%onp_size))

  end subroutine add_offp_index

  elemental function global_index(this, n) result(gid)
    class(index_map), intent(in) :: this
    integer, intent(in) :: n
    integer :: gid
    gid = -1
    if (n < 1) return
    if (n <= this%onp_size) then
      gid = this%first + n - 1
    else if (n <= this%local_size) then
      gid = this%offp_index(n-this%onp_size)
    end if
  end function

  subroutine gather(this, local_data)
    class(index_map), intent(in) :: this
    integer, intent(inout) :: local_data(:)
    call gather_aux(this, local_data(:this%onp_size), local_data(this%onp_size+1:))
  end subroutine

  subroutine gather_aux(this, onp_data, offp_data)

    class(index_map), intent(in) :: this
    integer, intent(in), target :: onp_data(:)
    integer, intent(out) :: offp_data(:)

    integer :: j

    !NB: An allocatable %data component is better but not possible due to bugs:
    ! https://github.com/nncarlson/fortran-compiler-tests/blob/master/gfortran-bugs/gfortran-20220128.f90
    ! https://github.com/nncarlson/fortran-compiler-tests/blob/master/nag-bugs/nag-20220127.f90

    type box
      !integer, allocatable :: data(:)
      integer, pointer :: data(:)
    end type
    type(box), allocatable :: send_buf[:]

    allocate(send_buf[*])
    allocate(send_buf%data(size(this%send_index)))
    send_buf%data = onp_data(this%send_index)
    sync all
    do j = 1, size(this%counts)
      associate (i  => this%send_images(j), n => this%counts(j), &
                 sd => this%send_displs(j), rd => this%recv_displs(j))
        offp_data(rd+1:rd+n) = send_buf[i]%data(sd+1:sd+n)
      end associate
    end do

    sync all
    deallocate(send_buf%data)

  end subroutine gather_aux

end module index_map_type
