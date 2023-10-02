module posix
    use iso_c_binding
    implicit none

    type, bind(c), public :: dirent
        integer(c_long) :: d_ino
        integer(c_long) :: d_off;
        integer(c_short) :: d_reclen
        character(kind=c_char) :: d_name(256)
    end type

    interface
        subroutine exit(code) bind(c, name='exit')
            import :: c_int
            implicit none
            integer(c_int), intent(in), value :: code
        end subroutine exit

        ! DIR* opendir(const char* dir_name)
        function opendir(dir_name) bind(c, name='opendir')
            import :: c_ptr, c_char
            implicit none
            character(kind=c_char), intent(in) :: dir_name ! dir_name(*)
            type(c_ptr) :: opendir
        end function opendir

        ! int closedir(DIR* dirp) 
        function closedir(dirp) bind(c, name='closedir')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr), intent(in), value :: dirp
            integer(c_int) :: closedir
        end function closedir
!
!        function readdir(dir) bind(c, name='readdir')
!            import
!            type(c_ptr), value :: dir
!            type(c_ptr) :: readdir
!        end function readdir
    end interface
end module posix

program fwhich
    use iso_fortran_env, only: error_unit
    use iso_c_binding
    use posix
    implicit none

    integer :: argc

    character(:), allocatable :: path
    integer :: path_len

    character(:), allocatable :: argv_file_name
    integer :: argv_file_name_len

    integer :: offset

    character(*), parameter :: file_path = 'build.sh' // c_null_char
    type(c_ptr) :: dir

    integer(c_int) :: rc

    argc = command_argument_count()
    if (argc .eq. 0) then
        write(unit=error_unit, fmt='(a)') 'error: missing required argument <FILE_NAME>'
        call exit(1)
    end if

    call get_command_argument(1, length=argv_file_name_len)
    allocate(character(argv_file_name_len) :: argv_file_name)
    call get_command_argument(1, value=argv_file_name)

    call get_environment_variable('PATH', length=path_len)
    allocate(character(path_len) :: path)
    call get_environment_variable('PATH', value=path)

    dir = opendir('build.sh' // c_null_char)
    rc = closedir(dir)

    do
        offset = scan(path, ':')
        if (offset .eq. 0) then
            print '(a)', path
            exit
        end if

        print '(a)', path(:offset - 1)
        path = path(offset + 1:)
    end do

    deallocate(path)
end program fwhich

