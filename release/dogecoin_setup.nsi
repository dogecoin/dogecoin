; dogecoin_setup.nsi
; Copyright (c) 2014 PlexiLabs

;------------------------------

; Include Modern UI
!include "MUI2.nsh"

; The name of the installer
Name "Dogecoin Setup"

; The file to write
OutFile "dogecoin_setup.exe"

; The default Installation Directory
InstallDir $PROGRAMFILES\Dogecoin

; Registry key to check for directory (so if you install again, it will
; overwrite the old one automatically)
InstallDirRegKey HKLM "Software\Dogecoin" "Install_Dir"

; Request application privileges if available
RequestExecutionLevel admin

;------------------------------

; Pages
!insertmacro MUI_PAGE_LICENSE "eula.txt"
!insertmacro MUI_PAGE_COMPONENTS
!insertmacro MUI_PAGE_DIRECTORY
!insertmacro MUI_PAGE_INSTFILES

!insertmacro MUI_UNPAGE_CONFIRM
!insertmacro MUI_UNPAGE_INSTFILES

;------------------------------

; Languages
!insertmacro MUI_LANGUAGE "English"

; Interface Settings
!define MUI_ABORTWARNING

; The Stuff to Install
Section "Dogecoin Required Files"

  SectionIn RO
  
  ; Set the path to the installation directory
  SetOutPath $INSTDIR
  
  ; Install Dogecoin Files
  File "dogecoin.conf"
  File "dogecoin-qt.exe"
  File "libgcc_s_dw2-1.dll"
  File "libgmp-10.dll"
  File "libmpfr-1.dll"
  File "libqrencode.dll"
  File "libstdc++-6.dll"
  File "mingwm10.dll"
  File "pthreadGC2.dll"
  File "QtCore4.dll"
  File "QtGui4.dll"
  File "QtNetwork4.dll"
  File "README.txt"
  
  ; Write the installation path into the registry
  WriteRegStr HKLM SOFTWARE\Dogecoin "Install_Dir" "$INSTDIR"
  
  ; Write the uninstall keys for windows
  WriteRegStr HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\Dogecoin" "Dogecoin" "Dogecoin Team"
  WriteRegStr HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\Dogecoin" "UninstallString" '"$INSTDIR\uninstall.exe"'
  WriteRegDWORD HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\Dogecoin" "NoModify" 1
  WriteRegDWORD HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\Dogecoin" "NoRepair" 1
  WriteUninstaller "uninstall.exe"
	
  ; Start Menu Shortcuts
  CreateDirectory "$SMPROGRAMS\Dogecoin"
  CreateShortCut "$SMPROGRAMS\Dogecoin\Dogecoin.lnk" "$INSTDIR\dogecoin-qt.exe"  "" "$INSTDIR\dogecoin-qt.exe" 0
  CreateShortCut "$SMPROGRAMS\Dogecoin\Uninstall.lnk" "$INSTDIR\uninstall.exe" "" "$INSTDIR\uninstall.exe" 0

SectionEnd 

; Optional Desktop Icon

Section "Desktop Shortcuts"
	
	; Create Desktop Icons
	CreateShortCut "$DESKTOP\Dogecoin.lnk" "$INSTDIR\dogecoin-qt.exe" ""
	
SectionEnd

; Uninstaller

Section "Uninstall"

  ; Remove Registry keys
  DeleteRegKey HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\Dogecoin"
  DeleteRegKey HKLM SOFTWARE\Dogecoin
  
  ; Remove files and uninstaller
  Delete $INSTDIR\dogecoin.conf
  Delete $INSTDIR\dogecoin-qt.exe
  Delete $INSTDIR\libgcc_s_dw2-1.dll
  Delete $INSTDIR\libgmp-10.dll
  Delete $INSTDIR\libmpfr-1.dll
  Delete $INSTDIR\libqrencode.dll
  Delete $INSTDIR\libstdc++-6.dll
  Delete $INSTDIR\mingwm10.dll
  Delete $INSTDIR\pthreadGC2.dll
  Delete $INSTDIR\QtCore4.dll
  Delete $INSTDIR\QtGui4.dll
  Delete $INSTDIR\QtNetwork4.dll
  Delete $INSTDIR\README.txt
  Delete $INSTDIR\uninstall.exe
  
  ; Remove Shortcuts, if any
  Delete "$SMPROGRAMS\Dogecoin\*.*"
  Delete "$DESKTOP\Dogecoin.lnk"
	
  ; Remove direcotries used
  RMDir "$SMPROGRAMS\Dogecoin"
  RMDir "$INSTDIR"

SectionEnd