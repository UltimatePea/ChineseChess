FAILED_FILES=()
for file in $(find ./src -name "*.hs"); do
	echo ""
	echo " ===> Checking $file"
	echo ""

    echo liquid -i src -i src/AI  $file   --ghc-option="-XFlexibleContexts" --ghc-option="-XLambdaCase" --ghc-option="-XConstraintKinds" --no-totality --no-termination
    liquid -i src -i src/AI  $file   --ghc-option="-XFlexibleContexts" --ghc-option="-XLambdaCase" --ghc-option="-XConstraintKinds" --no-totality --no-termination
	if [ $? -ne 0 ]; then 
		FAILED_FILES+=($file)
	fi
done

echo "${#FAILED_FILES[@]} Filed Files: "
for file in ${FAILED_FILES[@]}; do
	echo $file
done



