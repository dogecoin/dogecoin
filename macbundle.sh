/Applications/Qt/5.2.0/clang_64/bin/macdeployqt Dogecoin.app
cp /Applications/Qt/5.2.0/clang_64/lib/QtCore.framework/Contents/Info.plist Dogecoin.app/Contents/Frameworks/QtCore.framework/Resources/
cp /Applications/Qt/5.2.0/clang_64/lib/QtMacExtras.framework/Contents/Info.plist Dogecoin.app/Contents/Frameworks/QtMacExtras.framework/Resources/
cp /Applications/Qt/5.2.0/clang_64/lib/QtPrintSupport.framework/Contents/Info.plist Dogecoin.app/Contents/Frameworks/QtPrintSupport.framework/Resources/
cp /Applications/Qt/5.2.0/clang_64/lib/QtWidgets.framework/Contents/Info.plist Dogecoin.app/Contents/Frameworks/QtWidgets.framework/Resources/
cp /Applications/Qt/5.2.0/clang_64/lib/QtGui.framework/Contents/Info.plist Dogecoin.app/Contents/Frameworks/QtGui.framework/Resources/
codesign --deep --verify --verbose --sign "3rd Party Mac Developer Application: Alan Westbrook (SR7K2S8GE4)" Dogecoin.app/
zip -9ry Dogecoin.zip Dogecoin.app

