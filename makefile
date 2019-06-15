default: mdl
	@echo
	@echo rendering animation...
	./mdl phong.mdl

mdl: Main.hs Parser.hs Transform.hs Line.hs Solids.hs Screen.hs DrawMats.hs \
		Lighting.hs Lexer.hs Interpret.hs
	ghc -dynamic -O2 -threaded Main.hs -o mdl

Parser.hs: mdl.y
	happy -o Parser.hs -g mdl.y

Lexer.hs: mdl.x
	alex -o Lexer.hs -g mdl.x

clean:
	rm *.hi *.o Parser.hs Lexer.hs mdl .tempimg.ppm anim/*

imgclean:
	rm *.ppm *.png *.gif .tempimg.ppm anim/*

run:
	./mdl phong.mdl
