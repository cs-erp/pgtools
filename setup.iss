;
; PGTools 
;
; @license    GPL 2 (http://www.gnu.org/licenses/gpl.html)
; @author    Zaher Dirkey <zaherdirkey at yahoo dot com>
;
#define ApplicationVersion GetFileVersion('.\bin\pgtools32.exe')
[Setup]
AppName=PG Tools
AppVerName=PG Tools
AppPublisher=Creative Solutions
AppPublisherURL=http://www.cserp.org
AppSupportURL=http://www.cserp.org
AppUpdatesURL=http://www.cserp.org
DefaultDirName={pf}\Creative Solutions\PG Tools
DefaultGroupName=Creative Solutions
OutputBaseFilename=pgtools-setup-{#ApplicationVersion}
OutputDir=./setup
VersionInfoTextVersion=pgtools-setup-{#ApplicationVersion}
VersionInfoVersion={#ApplicationVersion}
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
Source: bin\pgtools32.exe; DestDir: {app}; Flags: ignoreversion
Source: bin\bin32\*.*; DestDir: {app}\bin32; Flags: ignoreversion
Source: bin\pgtools64.exe; DestDir: {app}; Flags: ignoreversion
Source: bin\bin64\*.*; DestDir: {app}\bin64; Flags: ignoreversion
Source: bin\languages\*.po; DestDir: {app}\languages; Flags: ignoreversion
Source: setup\*.ini; DestDir: {app}; Flags: ignoreversion

[INI]

[Icons]
Name: {group}\PG Tools 32; Filename: {app}\pgtools32.exe
Name: {group}\PG Tools 64; Filename: {app}\pgtools64.exe
Name: {group}\{cm:UninstallProgram,PG Tools}; Filename: {uninstallexe}
Name: {commondesktop}\Creative Solutions PG Tools 32; Filename: {app}\pgtools32.exe; Tasks: desktopicon
Name: {commondesktop}\Creative Solutions PG Tools 64; Filename: {app}\pgtools64.exe; Tasks: desktopicon

[UninstallDelete]
