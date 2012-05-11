      subroutine abre(n,nombre,mode)

      integer n
      character*(*) nombre
      character*(*) mode

      open(unit=n,file=nombre,status=mode)

      end

      subroutine cierra(n)

      integer n
      
      close(n)

      end
