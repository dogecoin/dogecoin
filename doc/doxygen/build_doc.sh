doxygen -g
sed -i '35s/.*/PROJECT_NAME="Dogecoin"/' Doxyfile
echo $'INPUT= ../../src ../../ ../\nOUTPUT_DIRECTORY= ./out\nGENERATE_TREEVIEW=YES\nHTML_EXTRA_STYLESHEET=theme.css' >> Doxyfile
doxygen