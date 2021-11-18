stack haddock --no-haddock-deps
cp -r $(stack path --local-install-root)/doc/dovetail-0.1.1.0/ docs/dovetail
cp -r $(stack path --local-install-root)/doc/dovetail-aeson-0.1.0.0/ docs/dovetail-aeson
