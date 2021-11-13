stack haddock --no-haddock-deps
cp -r $(stack path --local-install-root)/doc/dovetail-0.1.0.0/ docs/dovetail-0.1.0.0
cp -r $(stack path --local-install-root)/doc/dovetail-aeson-0.1.0.0/ docs/dovetail-aeson-0.1.0.0
