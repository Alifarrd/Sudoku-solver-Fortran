program Sudoku
integer::i,j,c,zi,zj
real,allocatable::k(:,:),ziro(:,:),hk(:),numbers(:)
OPEN(UNIT=20,FILE='Sudoku.txt',STATUS='old',ACTION='read')
OPEN(UNIT=40,FILE='Sudoku-ANS.txt',STATUS='replace',ACTION='write')

ALLOCATE (k(9,9),hk(9),numbers(9))
do i =1,9
  numbers(i)=i
  end do
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
ALLOCATE (ziro(c,12))
m=1
do i=1,9
  do j=1,9
    if (k(i,j)==0)then
      ziro(m,1)=i
      ziro(m,2)=j
      do ii=1,9
        ziro(m,ii+2)=ii
        ziro(m,12)=0
        end do
        m=m+1
 end if
 end do
 end do

!---------------------------------------------------[vertical Move]
do ix=1,c
zi=ziro(ix,1)
zj=ziro(ix,2)
hk(:)=k(zi,:) 
  do n=1,9
  do j =1,9
    if (hk(n)==ziro(ix,j+2))then
      ziro(ix,j+2)=0
      end if
      end do
          end do
end do

do i=1,c
 do ii=1,9
        if (ziro(i,ii+2)/=0)then
          ziro(i,12)=ziro(i,12)+1
          end if
          end do
          end do
!---------------------------------------------------[Horizental Move]

  
  
  
  
  
  
  do i=1,c
  write(40,*)ziro(i,:)
  end do


end 