cab : 
	cabal configure
	cabal install
roll : commit push
	make commit
	make push
commit :
	cd ..
	git add . -A
	git commit -m "Buld update!"
push :
	git push origin master
