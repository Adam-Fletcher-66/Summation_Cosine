! Name: Adam_Fletcher
! Date: 03/04/2022
! Purpose: To use an iterative do loop to calculate a particular summation      &
!          function with an independent variable and factorial denominator.


program summation_loop
  implicit none


                 ! Variable Dictionary
  integer :: i                    ! Index variable for loop
  integer :: N                    ! End parameter of index
  real :: Nfact                   ! Value of factorial for each increment
  real :: sum                     ! Value of the total sum of each loop
  real :: x                       ! Independent variable of the function
  

  write(*,*) "Enter upper limit N<=6 followed by a value of x>0:"
  read(*,*) N,x                   ! Reads end index and I.Variable

  sum = 0.0                       ! Initial sum = 0, each loop will add to it
  
  do i=0,N,1                      ! Initiates do loop from 0-N with             &
                                  ! increments of 1
     
    if(i==0) then                 ! Sets values of Nfact for each loop
       Nfact = 1.0
    elseif (i==1) then
       Nfact = 2.0
     elseif(i==2) then
       Nfact = 24.0
     elseif(i==3) then
       Nfact = 720.0
     elseif(i==4) then
       Nfact = 40320.0
     elseif(i==5) then
       Nfact = 3628800.0
     elseif(i==6) then
       Nfact = 479001600.0
     endif
                                  ! Calculates the sum of the function for each &
                                  ! loop and then adds the sums
        sum = sum + (((-1)**i * x**(2*i)) /  Nfact)
     

  enddo                           ! Exits do loop
  
 
  write(*,*) "The sum of the function is:", sum
  
  stop 0                          ! Stops program execution

end program summation_loop
