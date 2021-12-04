!!!  Muhammed Destan Chess-Code 08/08/2019
PROGRAM SATRANC
implicit none
integer x1,x2,y1,y2,p,donus
integer i,j,k
integer tahta(8,8)
logical dongudeKal
character baslat*6,virgul*1
dongudeKal=.true.
p=1
donus=999
print*,"Satranc Oyunumuza hosgeldiniz."
	open(11,file="tahta.dat")
	do i=1,8
		read(11,*) (tahta(i,j),j=1,8)
	end do

	print*,"Lutfen oyuncu oyuna baslasin."
	 do while (dongudeKal)
		9 print*,"-------------------------------Muhammed DESTAN 110160504 CHESS PROGRAM-----------------------------------"
                  print*,"---------------------------------------------------------------------------------------------------------"
                  call tahtaYazdir(tahta)
		  print*,"---------------------------------------------------------------------------------------------------------"
                10 print*,p," Oyuncu,lutfen oynamak istediginiz tasi istenilen formatta (x1,y1) giriniz"
		  read*, x1,y1
		  write(*,*) x1,y1
			if (x1.lt.1 .or. x1.gt.8 .or. y1.lt.1 .or. y1.gt.8) then
				print*,"Lutfen 8 den kucuk 1 den buyuk rakamlar giriniz"
				goto 97
			end if
			if ((tahta(x1,y1)*p).lt.1)then
				print*,"Lutfen kendi tasinizi seciniz"
				goto 97
			end if
		print*,"Hamle yapmak istediginiz yeri istenilen formatta (x2,y2) giriniz"
		read*,x2,y2
		write(*,*) x2,y2
			if (x2.lt.1 .or. x2.gt.8 .or. y2.lt.1 .or. y2.gt.8) then
                                  print*,"Lutfen 8 den kucuk 1 den buyuk rakamlar giriniz"
                                  goto 97
                        end if
			if(abs(tahta(x1,y1)).eq.1) then
				print*,"Piyon hamleye gidecek"
				call piyon(tahta,x1,x2,y1,y2,p,donus)
				if(donus.eq.1) then
					goto 99
				else
					goto 98
				endif
			else if(abs(tahta(x1,y1)).eq.2) then
				print*,"Kale hamleye gidecek"
				call kale(tahta,x1,x2,y1,y2,p,donus)
				if(donus.eq.1) then
					goto 99
				else
					goto 98
				endif
			else if(abs(tahta(x1,y1)).eq.3) then
				print*,"At hamleye gidecek"
				call at(tahta,x1,x2,y1,y2,p,donus)
				if(donus.eq.1) then
					goto 99
				else
					goto 98
				endif
			else if(abs(tahta(x1,y1)).eq.4) then
				print*,"Fil hamleye gidecek"
				call fil(tahta,x1,x2,y1,y2,p,donus)
				if(donus.eq.1) then
					goto 99
				else
					goto 98
				endif
			else if(abs(tahta(x1,y1)).eq.5) then
				print*,"Vezir hamleye gidecek"
				call vezir(tahta,x1,x2,y1,y2,p,donus)
				if(donus.eq.1) then
					goto 99
				else
					goto 98
				endif
			else if(abs(tahta(x1,y1)).eq.6) then
				print*,"Sah hamleye gidecek"
				call sah(tahta,x1,x2,y1,y2,p,donus)
				if(donus.eq.1) then
					goto 99
				else
					goto 98
				endif
			end if
		goto 10
		97 print*,"Yanlis Tas"
		goto 10
		98 print*,"Yanlis hamle"
		goto 10
		99 print*,"Hamle Basarili"
		p=p*(-1)
		goto 9
		dongudeKal=.false.
	end do
END PROGRAM SATRANC
subroutine tahtaYazdir(tahta)
implicit none
integer tahta(8,8)
integer i,j
	do i=1,8
		write(*,*) (tahta(i,j),j=1,8)
	end do
end
subroutine piyon(tahta,x1,x2,y1,y2,p,donus)
implicit none
integer x1,x2,y1,y2,p,donus,deltaX,deltaY
integer tahta(8,8)
deltaX = x1-x2
deltaY = y1-y2
if (x1.eq.7 .or. x1.eq.2) then !!ilk sira olmasi 
	if(deltaX.eq.p .or. deltaX.eq.2*p) then !! geriye gitmeyi engellemesi
		if(abs(deltaX).eq.1 .and. abs(deltaY).eq.1) then !! capraz gitmesi
			if(tahta(x2,y2)*p.lt.0) then !! karsi takim ile carpimi sifirdan kucuktur
				tahta(x2,y2)=tahta(x1,y1) !! ilk olarak hamle yerine eski tasi yaziyoruz
				tahta(x1,y1)=0 !!cikan tasin yerine 0 yazicak
				donus=1
				return
			end if
		else if(deltaY.eq.0) then !! duz g
			if(abs(deltaX).eq.2)then
				if(tahta(x1-p,y1).eq.0 .and. tahta(x2,y2).eq.0) then
					tahta(x2,y2)=tahta(x1,y1)
					tahta(x1,y1)=0
					donus=1
					return
				endif
			else if(abs(deltaX).eq.1)then
				if(tahta(x2,y2).eq.0) then
					tahta(x2,y2)=tahta(x1,y1)
					tahta(x1,y1)=0
					donus=1
					return
				endif
			endif
		end if
	endif
else
	if(abs(deltaX).eq.1 .and. abs(deltaY).eq.1) then
		if(tahta(x2,y2)*p.lt.0) then
			tahta(x2,y2)=tahta(x1,y1)
			tahta(x1,y1)=0
			donus=1
			return
		end if
	else if(deltaY.eq.0 .and. abs(deltaX).eq.1) then
		if(tahta(x2,y2).eq.0) then
			tahta(x2,y2)=tahta(x1,y1)
			tahta(x1,y1)=0
			donus=1
			return
		endif
	end if
	
endif
donus=0
end
  
subroutine kale(tahta,x1,x2,y1,y2,p,donus)
implicit none
integer i,x1,x2,y1,y2,p,donus,deltaX,deltaY,yon
integer tahta(8,8)
deltaX = x1-x2
deltaY = y1-y2
yon=0
	if(deltaX.eq.0 .and. deltaY.ne.0) then
		if(deltaY.gt.0) then
			yon=-1
		else
			yon=1
		endif
		do i=y1+yon,y2-yon,yon
			if(tahta(x1,i).ne.0)then
				print*,"Arada tas bulunmakta"
				donus=0
				return
			endif
		enddo	
		if(tahta(x2,y2)*p.lt.1) then
			tahta(x2,y2)=tahta(x1,y1)
			tahta(x1,y1)=0
			donus=1
			return
		endif
	else if (deltaX.ne.0 .and. deltaY.eq.0)then
		if(deltaX.gt.0) then
			yon=-1
		else
			yon=1
		endif
		do i=x1+yon,x2-yon,yon
			if(tahta(i,y1).ne.0)then
				print*,"Arada tas bulunmakta"
				donus=0
				return
			endif
		enddo
		if(tahta(x2,y2)*p.lt.1) then
			tahta(x2,y2)=tahta(x1,y1)
			tahta(x1,y1)=0
			donus=1
			return
		endif
	endif
donus=0
end

subroutine at(tahta,x1,x2,y1,y2,p,donus)
implicit none
integer x1,x2,y1,y2,p,donus,deltaX,deltaY
integer tahta(8,8)
deltaX = x1-x2
deltaY = y1-y2
if((abs(deltaX).eq.2 .and. abs(deltaY).eq.1) .or. (abs(deltaX).eq.1 .and. abs(deltaY).eq.2)) then
	if(tahta(x2,y2)*p.lt.1) then
		tahta(x2,y2)=tahta(x1,y1)
		tahta(x1,y1)=0
		donus=1
		return
	endif
end if
donus=0
end

subroutine fil(tahta,x1,x2,y1,y2,p,donus)
implicit none
integer i,x1,x2,y1,y2,p,donus,deltaX,deltaY,yonX,yonY,adim
integer tahta(8,8)
deltaX = x1-x2
deltaY = y1-y2
adim=abs(deltaX)-1
if(abs(deltaX).ne.abs(deltaY))then
	donus=0
	return
endif
if(deltaX.gt.0)then
	yonX=-1
else
	yonX=1
endif
if(deltaY.gt.0)then
	yonY=-1
else
	yonY=1
endif
print*,"yonY",yonY
print*,"yonX",yonX
	do i=1,adim
		if(tahta(x1+yonX*i,y1+yonY*i).ne.0)then
			print*,"Arada tas bulunmaktadir"
			donus=0
			return
		endif
	enddo	
	if(tahta(x2,y2)*p.lt.1) then
		tahta(x2,y2)=tahta(x1,y1)
		tahta(x1,y1)=0
		donus=1
		return
	endif
donus=0
end

subroutine vezir(tahta,x1,x2,y1,y2,p,donus)
implicit none
integer x1,x2,y1,y2,p,donus,deltaX,deltaY
integer tahta(8,8)
deltaX = x1-x2
deltaY = y1-y2
if(deltaX.eq.0 .or. deltaY.eq.0) then
	call kale(tahta,x1,x2,y1,y2,p,donus)
	return
else if (abs(deltaX).eq.abs(deltaY)) then
	call fil(tahta,x1,x2,y1,y2,p,donus)
	return
endif
donus=0
end
subroutine sah(tahta,x1,x2,y1,y2,p,donus)
implicit none
integer x1,x2,y1,y2,p,donus,deltaX,deltaY,yenen
integer tahta(8,8)
deltaX = x1-x2
deltaY = y1-y2
if(abs(deltaX).lt.2 .and. abs(deltaY).lt.2) then
	if(tahta(x2,y2)*p.lt.1) then
		tahta(x2,y2)=tahta(x1,y1)
		tahta(x1,y1)=0
		donus=1
		return
	endif
endif
donus=0
end
