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
	cd engines
push :
	git push origin master
