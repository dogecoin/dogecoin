/Applications/Qt/5.2.0/clang_64/bin/macdeployqt Dogecoin-Qt.app
cp /Applications/Qt/5.2.0/clang_64/lib/QtCore.framework/Contents/Info.plist Dogecoin-Qt.app/Contents/Frameworks/QtCore.framework/Resources/
cp /Applications/Qt/5.2.0/clang_64/lib/QtMacExtras.framework/Contents/Info.plist Dogecoin-Qt.app/Contents/Frameworks/QtMacExtras.framework/Resources/
cp /Applications/Qt/5.2.0/clang_64/lib/QtPrintSupport.framework/Contents/Info.plist Dogecoin-Qt.app/Contents/Frameworks/QtPrintSupport.framework/Resources/
cp /Applications/Qt/5.2.0/clang_64/lib/QtWidgets.framework/Contents/Info.plist Dogecoin-Qt.app/Contents/Frameworks/QtWidgets.framework/Resources/
cp /Applications/Qt/5.2.0/clang_64/lib/QtGui.framework/Contents/Info.plist Dogecoin-Qt.app/Contents/Frameworks/QtGui.framework/Resources/
codesign --deep --verify --verbose --sign "3rd Party Mac Developer Application" Dogecoin-Qt.app/
zip -9ry Dogecoin-Qt.zip Dogecoin-Qt.app

