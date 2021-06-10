doxygen -g
sed -i '35s/.*/PROJECT_NAME="Dogecoin"/' Doxyfile
echo $'INPUT= ../../src ../../ ../\nOUTPUT_DIRECTORY= ./out' >> Doxyfile
doxygen