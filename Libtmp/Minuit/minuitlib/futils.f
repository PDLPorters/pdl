      subroutine abre(n,nombre,mode)

      integer*8 n
      character*(*) nombre
      character*(*) mode

      open(unit=n,file=nombre,status=mode)

      end

      subroutine cierra(n)

      integer*8 n
      
      close(n)

      end
