;
; Light PHP Edit
;
; @license    GPL 2 (http://www.gnu.org/licenses/gpl.html)
; @author    Zaher Dirkey <zaher at parmaja dot com>
;
[Setup]
AppName=PG Tools
AppVerName=PG Tools
AppPublisher=Creative Solutions
AppPublisherURL=http://www.cserp.org
AppSupportURL=http://www.cserp.org
AppUpdatesURL=http://www.cserp.org
DefaultDirName={pf}\Creative Solutions\PG Tools
DefaultGroupName=Creative Solutions
OutputBaseFilename=pgtools-setup-1.2
OutputDir=.\
VersionInfoTextVersion=pgtools-setup-1.2
VersionInfoVersion=1.2
VersionInfoCompany=cserp.org
VersionInfoDescription=PG Tools
Compression=lzma/ultra
SolidCompression=true
InternalCompressLevel=ultra
AppMutex=CS.PGTools
UninstallDisplayIcon={app}\pgtools32.exe
ShowLanguageDialog=yes

[Languages]
Name: eng; MessagesFile: compiler:Default.isl
;Name: ara; MessagesFile: compiler:Languages\Arabic.isl

[Tasks]
Name: desktopicon; Description: {cm:CreateDesktopIcon}; GroupDescription: {cm:AdditionalIcons}; Flags: unchecked
Name: quicklaunchicon; Description: {cm:CreateQuickLaunchIcon}; GroupDescription: {cm:AdditionalIcons}; Flags: unchecked

[Files]
Source: pgtools*.exe; DestDir: {app}; Flags: ignoreversion
Source: setup\*.dll; DestDir: {app}; Flags: ignoreversion
Source: setup\*.exe; DestDir: {app}; Flags: ignoreversion
Source: setup\*.ini; DestDir: {app}; Flags: ignoreversion

[INI]

[Icons]
Name: {group}\PG Tools 32; Filename: {app}\pgtools32.exe
Name: {group}\PG Tools 64; Filename: {app}\pgtools64.exe
Name: {group}\{cm:UninstallProgram,PG Tools}; Filename: {uninstallexe}
Name: {userdesktop}\Creative Solutions PG Tools; Filename: {app}\pgtools32.exe; Tasks: desktopicon
Name: {userdesktop}\Creative Solutions PG Tools; Filename: {app}\pgtools64.exe; Tasks: desktopicon

[UninstallDelete]
