program Sudoku
integer::i,j,c
real,allocatable::k(:,:),ziro(:,:)
OPEN(UNIT=20,FILE='Sudoku.txt',STATUS='old',ACTION='read')
OPEN(UNIT=40,FILE='Sudoku-ANS.txt',STATUS='replace',ACTION='write')

ALLOCATE (k(9,9))
!---------------------------------------------------[Read the Sudoku]
do i=1,9
  do j = 1,9
read(20,*)k(i,j)
  end do
end do
!---------------------------------------------------[count the ziroes]
c=0
do i=1,9
  do j = 1,9
    if (k(i,j)==0)then
      c=c+1
      end if
  end do
end do
!---------------------------------------------------[Find the ziroes]
ALLOCATE (ziro(c,11))
m=1
do i=1,9
  do j=1,9
    if (k(i,j)==0)then
      ziro(m,1)=i
      ziro(m,2)=j
      do ii=1,9
        ziro(m,ii+2)=ii
        end do
        m=m+1
 end if
 end do
 end do
!---------------------------------------------------[Read the Sudoku]





end 