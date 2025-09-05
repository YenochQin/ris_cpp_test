module cre_I
      implicit none
      interface
            function cre (KAP1, K, KAP2)
            use, intrinsic :: iso_fortran_env, only: real64
            integer, INTENT(IN) :: KAP1
            integer :: K
            integer, INTENT(IN) :: KAP2
            !VAST...Calls: CLRX
            END FUNCTION
      END interface
end module
