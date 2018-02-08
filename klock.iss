; Script generated by the Inno Setup Script Wizard.
; SEE THE DOCUMENTATION FOR DETAILS ON CREATING INNO SETUP SCRIPT FILES!

#define MyAppName "lazKlock"
#define MyAppVersion "71"
#define MyAppPublisher "keleven"
#define MyAppURL "www.keleven.co.uk"
#define MyAppExeName "lazklock.exe"

[Setup]
; NOTE: The value of AppId uniquely identifies this application.
; Do not use the same AppId value in installers for other applications.
; (To generate a new GUID, click Tools | Generate GUID inside the IDE.)
AppId={{9746E6DF-D0F2-4A97-80D2-0A8F478A785E}
AppName={#MyAppName}
AppVersion={#MyAppVersion}
;AppVerName={#MyAppName} {#MyAppVersion}
AppPublisher={#MyAppPublisher}
AppPublisherURL={#MyAppURL}
AppSupportURL={#MyAppURL}
AppUpdatesURL={#MyAppURL}

;  all source files here
SourceDir=D:\My\shed\Projects\pascal\lazklock

DefaultDirName={pf}\keleven\{#MyAppName}
DefaultGroupName={#MyAppName}
LicenseFile=GNU GENERAL PUBLIC LICENSE.txt
InfoAfterFile=help.txt
OutputDir=D:\My\shed\Projects\pascal
OutputBaseFilename={#MyAppName}_{#MyAppVersion}
SetupIconFile=klock.ico
Compression=lzma
SolidCompression=yes
DisableStartupPrompt=False
UsePreviousAppDir=False
SetupLogging=True

; "ArchitecturesInstallIn64BitMode=x64" requests that the install be done in "64-bit mode" 
; on x64, meaning it should use the native 64-bit Program Files directory and the 64-bit 
; view of the registry. On all other architectures it will install in "32-bit mode".
ArchitecturesInstallIn64BitMode=x64
; Note: We don't set ProcessorsAllowed because we want this installation to run on 
; all architectures (including Itanium,since it's capable of running 32-bit code too)

[Messages]
SetupLdrStartupMessage=This will install [%1 V2.2.3.71] on your computer.

[Languages]
Name: "english"; MessagesFile: "compiler:Default.isl"

[Tasks]
Name: "desktopicon"    ; Description: "{cm:CreateDesktopIcon}"    ; GroupDescription: "{cm:AdditionalIcons}"; Flags: unchecked
Name: "quicklaunchicon"; Description: "{cm:CreateQuickLaunchIcon}"; GroupDescription: "{cm:AdditionalIcons}"; Flags: unchecked; OnlyBelowVersion: 0,6.1


; NOTE: Don't use "Flags: ignoreversion" on any shared system files
; installs either klock_x64 or klock_x86 - but names them lazKlock.exe
[Files]
Source: "lazKlock_32.exe"               ; DestDir: "{app}"           ; Flags: ignoreversion; Check: not Is64BitInstallMode; DestName: {#MyAppExeName}
Source: "lazKlock_64.exe"               ; DestDir: "{app}"           ; Flags: ignoreversion; Check: Is64BitInstallMode    ; DestName: {#MyAppExeName}
Source: "help.txt"                      ; DestDir: "{app}"           ; Flags: ignoreversion
Source: "history.txt"                   ; DestDir: "{app}"           ; Flags: ignoreversion
Source: "GNU GENERAL PUBLIC LICENSE.txt"; DestDir: "{app}"           ; Flags: ignoreversion
Source: "sounds\*"                      ; DestDir: "{app}\sounds"    ; Flags: ignoreversion
Source: "fonts\*"                       ; DestDir: "{app}\fonts"     ; Flags: ignoreversion
Source: "help\*"                        ; DestDir: "{app}\help"      ; Flags: ignoreversion; Excludes: "klock.hnd"
Source: "help\html\*"                   ; DestDir: "{app}\help\html" ; Flags: ignoreversion

[Icons]
Name: "{group}\{#MyAppName}"                                               ; Filename: "{app}\{#MyAppExeName}"
Name: "{group}\{cm:UninstallProgram,{#MyAppName}}"                         ; Filename: "{uninstallexe}"
Name: "{commondesktop}\{#MyAppName}"                                       ; Filename: "{app}\{#MyAppExeName}"; Tasks: desktopicon
Name: "{userappdata}\Microsoft\Internet Explorer\Quick Launch\{#MyAppName}"; Filename: "{app}\{#MyAppExeName}"; Tasks: quicklaunchicon

[Run]
Filename: "{app}\{#MyAppExeName}"; Description: "{cm:LaunchProgram,{#StringChange(MyAppName, "&", "&&")}}"; Flags: nowait postinstall skipifsilent

