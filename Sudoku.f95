program Sudoku
implicit none
integer::i,j,c,zi,zj,ki,kj,n,counter,ce,summ,ix,ii,m,it,cep
real,allocatable::k(:,:),ziro(:,:),hk(:),ziroh(:,:),ziro3(:,:)
!OPEN(UNIT=20,FILE='SudokuE.txt',STATUS='old',ACTION='read')
OPEN(UNIT=20,FILE='SudokuM.txt',STATUS='old',ACTION='read')
!OPEN(UNIT=20,FILE='SudokuH.txt',STATUS='old',ACTION='read')
OPEN(UNIT=40,FILE='Sudoku-ANS.txt',STATUS='replace',ACTION='write')

ALLOCATE (k(9,9),hk(9))

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

write(*,*)'Number of Unknowns= ',c
!---------------------------------------------------[Find the ziroes]
ALLOCATE (ziro(c,12),ziroh(c,12),ziro3(c,12))

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
ce=0
do !===================================================[solving loop]
cep=ce

ziro(:,12)=0
ziroh(:,:)=ziro(:,:)
ziro3(:,:)=ziro(:,:)
!---------------------------------------------------[vertical Move]
do ix=1,c
if (ziro(ix,1)/=0)then
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

          end if  
end do
!---------------------------------------------------[Horizental Move]
do ix=1,c
if (ziro(ix,1)/=0)then
zi=ziroh(ix,1)
zj=ziroh(ix,2)
hk(:)=k(:,zj) 
  do n=1,9
  do j =1,9
    if (hk(n)==ziroh(ix,j+2))then
      ziroh(ix,j+2)=0
      end if
      end do
          end do
end if

  end do
!---------------------------------------------------[3*3 Move]
do ix=1,c
if (ziro(ix,1)/=0)then  
zi=ziro3(ix,1)
zj=ziro3(ix,2)

!=====================================[find the block]
i=zi
j=zj

if ((1.ge.i.or.i.le.3).and.(1.ge.j.or.j.le.3))then
  ki=1
  kj=1
else if((1.ge.i.or.i.le.3).and.(4.ge.j.or.j.le.6))then 
  ki=1
  kj=4
else if((1.ge.i.or.i.le.3).and.(7.ge.j.or.j.le.9))then
  ki=1
  kj=7
!11111111111111111111111111111
else if((4.ge.i.or.i.le.6).and.(1.ge.j.or.j.le.3))then
  ki=4
  kj=1
else if((4.ge.i.or.i.le.6).and.(4.ge.j.or.j.le.6))then 
  ki=4
  kj=4
else if((4.ge.i.or.i.le.6).and.(7.ge.j.or.j.le.9))then
  ki=4
  kj=7
!2222222222222222222222222222222222222222222222
else if((7.ge.i.or.i.le.9).and.(1.ge.j.or.j.le.3))then
  ki=7
  kj=1
else if((7.ge.i.or.i.le.9).and.(4.ge.j.or.j.le.6))then 
  ki=7
  kj=4
else if((7.ge.i.or.i.le.9).and.(7.ge.j.or.j.le.9))then
  ki=7
  kj=7
end if
!=====================================[find the block]
counter=1
do i=ki,ki+2
  do j=kj,kj+2
    hk(counter)=k(i,j)
    counter=counter+1
    end do
    end do 

  do n=1,9
  do j =1,9
    if (hk(n)==ziro3(ix,j+2))then
      ziro3(ix,j+2)=0
      end if
      end do
          end do
end if


  end do

!---------------------------------------------------[compare 3 marrixes]       

do j=1,c  
  do i=1,9
  if (ziro(j,1)/=0)then  
  if ((ziro(j,i+2)==0).or.(ziroh(j,i+2)==0).or.(ziro3(j,i+2)==0))then
    ziro(j,i+2)=0
    ziroh(j,i+2)=0
    ziro3(j,i+2)=0
end if
end if
     end do
 end do     
      
do i=1,c
 do ii=1,9
   if (ziro(i,1)/=0)then
        if (ziro(i,ii+2)/=0)then
          ziro(i,12)=ziro(i,12)+1
         else if (ziroh(i,ii+2)/=0)then
          ziroh(i,12)=ziroh(i,12)+1         
         else if (ziro3(i,ii+2)/=0)then
          ziro3(i,12)=ziro3(i,12)+1 
end if
end if
          end do
          end do

!---------------------------------------------------[find the answers]   
do i=1,c
  summ=0
if (ziro(i,12)==1 .and. ziro(i,1)/=0)then
  do ii=1,9
    summ=summ+ziro(i,ii+2)
    end do
    zi=ziro(i,1)
    zj=ziro(i,2)
    k(zi,zj)= summ
!    write(*,*)zi,zj,summ
      ziro(i,1)=0
  	  ziro(i,2)=0
    end if
end do
!---------------------------------------------------[find the remain zeroes] 
ce=0
do i=1,9
  do j = 1,9
    if (k(i,j)==0)then
      ce=ce+1
      end if
  end do
end do

write(*,*)'Number of Unknowns= ',ce
!---------------------------------------------------[print the final Resilt]
if (ce==0)then
do i=1,9
  write(40,*)k(i,:)
  end do

  exit
else if (cep==ce)then
  write(*,*)'ERROR!'
  do i = 1,c
    write(*,*)ziro(i,1),ziro(i,2),ziro(i,12)
    end do
  exit
  end if

end do

end 