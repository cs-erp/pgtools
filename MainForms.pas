unit MainForms;

{**
*  This file is part of the "Creative Solutions PGTools http://www.cserp.org/"
 *
 * @license   mit(https://opensource.org/licenses/MIT)
 *
 * @author    Zaher Dirkey <zaher at parmaja dot com>
 *
 *  TODO: add clean with : --clean --if-exists
 *}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls, StrUtils,
  ComCtrls, Menus, IniFiles, Contnrs, SynEdit, SynHighlighterAny,
  LCLTranslator,
  mncPostgre, mnMsgBox, GUIMsgBox, process,
  ConsoleProcess, FileUtil, mnUtils, LazFileUtils;

type
  TRestoreOption = (
    roOverwrite,
    roDirectDatabase,
    roIgnoreError,
    roCreateMissedMetaData,
    roDropPublicSchema,
    roRestorePublicSchemaOnly,
    roRestroreDataOnly,
    roCreativeSolutions
  );

  TRestoreOptions = set of TRestoreOption;

  { TMainForm }

  TMainForm = class(TForm)
    BackFolderLbl: TLabel;
    BackupAllBtn: TButton;
    BackupBtn1: TButton;
    BackupBtn2: TButton;
    RestoreDataOnlyChk: TCheckBox;
    RestorePublicSchemaOnlyChk: TCheckBox;
    LangListCbo: TComboBox;
    CopyBtn: TButton;
    Label8: TLabel;
    QuickBackupChk: TCheckBox;
    RestoreCleanErrorChk: TCheckBox;
    CreateMissedMetaDataChk: TCheckBox;
    DropPublicSchemaChk: TCheckBox;
    RestoreFileOverwriteChk: TCheckBox;
    RestoreByDateBtn: TButton;
    RestoreFileIgnoreErrorChk: TCheckBox;
    RestoreLastBtn1: TButton;
    RestorFromFileBtn: TButton;
    OpenFolderBtn: TButton;
    StopBtn: TButton;
    DirectoryEdit: TEdit;
    ScrollMnu: TMenuItem;
    RestorePointBtn: TButton;
    SavePointBtn: TButton;
    DropAllTempsBtn: TButton;
    ChangeBtn: TButton;
    AddDatabaseBtn: TButton;
    RemoveDatabaseBtn: TButton;
    BackupFileNameEdit: TEdit;
    BackupDeviceIDLbl: TLabel;
    DropBtn: TButton;
    CSProductsChk: TCheckBox;
    RenameBtn: TButton;
    NewPasswordLbl: TLabel;
    NewPasswordEdit: TEdit;
    AdminPanel: TPanel;
    Image1: TImage;
    DBUserNameLbl: TLabel;
    DBPasswordLbl: TLabel;
    DBPortLbl: TLabel;
    RestoreFileToNewDatabaseLbl: TLabel;
    LogoTestLbl: TLabel;
    ClearLogMnu: TMenuItem;
    OptionsTab: TPageControl;
    PasswordEdit: TEdit;
    RestoreFilePageControl: TPageControl;
    LogPopupMenu: TPopupMenu;
    ListDatabasesBtn: TButton;
    DatabasesCbo: TComboBox;
    InfoPanel: TPanel;
    BackupDatabasesList: TListBox;
    PortEdit: TEdit;
    PublicSchemaChk: TCheckBox;
    RestoreAllBtn: TButton;
    RestoreLastBtn: TButton;
    RestoreNewFromFileBtn: TButton;
    BrowseBackupBtn: TButton;
    SavePasswordChk: TCheckBox;
    DatabaseTab: TTabSheet;
    RestoreFileTab: TTabSheet;
    ExportTab: TTabSheet;
    LogEdit: TSynEdit;
    ConnectionInfoTab: TTabSheet;
    SpecialOptionsTab: TTabSheet;
    AdminTab: TTabSheet;
    StatusTimer: TTimer;
    UserNameEdit: TEdit;
    procedure BackupBtn1Click(Sender: TObject);
    procedure BackupBtn2Click(Sender: TObject);
    procedure BackupAllBtnClick(Sender: TObject);
    procedure BackupDatabasesListClick(Sender: TObject);
    procedure BackupDatabasesListDblClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure ChangeBtnClick(Sender: TObject);
    procedure CopyBtnClick(Sender: TObject);
    procedure LangListCboChange(Sender: TObject);
    procedure LangListCboSelect(Sender: TObject);
    procedure ListDatabasesBtnClick(Sender: TObject);
    procedure AddDatabaseBtnClick(Sender: TObject);
    procedure RemoveDatabaseBtnClick(Sender: TObject);
    procedure DropBtnClick(Sender: TObject);
    procedure DropAllTempsBtnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ClearLogMnuClick(Sender: TObject);
    procedure OpenFolderBtnClick(Sender: TObject);
    procedure RenameBtnClick(Sender: TObject);
    procedure RestoreLastBtn1Click(Sender: TObject);
    procedure RestoreLastBtnClick(Sender: TObject);
    procedure RestoreNewFromFileBtnClick(Sender: TObject);
    procedure BrowseBackupBtnClick(Sender: TObject);
    procedure RestoreByDateBtnClick(Sender: TObject);
    procedure RestoreAllBtnClick(Sender: TObject);
    procedure RestorFromFileBtnClick(Sender: TObject);
    procedure RestorePointBtnClick(Sender: TObject);
    procedure SavePointBtnClick(Sender: TObject);
    procedure ScrollMnuClick(Sender: TObject);
    procedure StatusTimerTimer(Sender: TObject);
    procedure StopBtnClick(Sender: TObject);
  private
    PoolThread: TObjectList;
    ConsoleThread: TmnConsoleThread;
    procedure CheckLanguage;
    procedure ExploreFolder(AFolder, FileName: string);
    function GetPort: String;
    procedure BackupDatabase(DB: String; APointName: string = '');
    procedure RestoreDatabase(DB: String; APointName: String = '');
    procedure RestoreDatabaseFile(DB: String; AFileName: String = ''; Options: TRestoreOptions = [roOverwrite]);
    procedure Log(S: String; Kind: TmnLogKind = lgLog);
    procedure ConsoleTerminated(Sender: TObject);
  protected
    FDestroying: Boolean;
    FStop: Boolean;
    PGConn: TmncPGConnection;
    PGSession: TmncPGSession;
    Databases: TStringList;
    PGPathBin: String;
    IniPath: String;
    Portable: Boolean;
    procedure BringInfo;
    function GetBackupDBDirectory: string;
    function GetBackupDBDirectory(DB: string): string;
    function GetBackupDirectory: String;
    function GetPGDirectory: String;
    procedure EnumDatabases(vOld: Boolean);
    procedure DropDatabase(ADatabase: String);
    procedure RenameDatabase(ADatabase, AToName: String);
    procedure CopyDatabase(ADatabase, AToName: String);
    procedure OpenPG(vDatabase: String = 'postgres'; StartSession: Boolean = True);
    procedure ClosePG(StopSession: Boolean = True);
    procedure Launch(vMessage, vExecutable, vParameters, vPassword: String; vExecuteObject: TExecuteObject = nil; IgnoreError: Boolean = False);
    procedure Resume;
    procedure LoadIni;
    procedure DetectPGDirectory;
  public
    InternalPGDirectory: String;//detected when load
    ExportMode: Boolean;
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  MainForm: TMainForm;

function GetLocalName: string;

implementation

{$R *.lfm}

resourcestring
  sCleanDone = 'Clean Done';
  sExcludeTables = '"*.\"AppFiles\""';

function RO(B: Boolean; O: TRestoreOption): TRestoreOptions;
begin
  if B then
    Result := [O]
  else
    Result := [];
end;

function GetLocalName: string;
begin
  Result := Application.EnvironmentVariable['USERNAME']+'_'+Application.EnvironmentVariable['COMPUTERNAME'];
end;

{ TMainForm }

procedure TMainForm.DropAllTempsBtnClick(Sender: TObject);
var
  i: Integer;
begin
  OpenPG('postgres', False);
  try
    EnumDatabases(True);
    for i := 0 to Databases.Count - 1 do
    begin
      InfoPanel.Caption := 'Dropping database ' + Databases[i];
      PGConn.Execute('drop database "' + Databases[i] + '"');
      //if PGConn.Execute('drop database "' + Databases[i]+'"') then
      Log('Database Dropped: "' + Databases[i] + '"', lgStatus);
      //else
      //  Log('Database Dropped: "' + Databases[i]+'"');
      LogEdit.CaretY := LogEdit.Lines.Count - 1;
      Application.ProcessMessages;
    end;
  finally
    ClosePG(False);
  end;
  Log(sCleanDone, lgDone);
  Databases.Clear;
end;

procedure TMainForm.BackupAllBtnClick(Sender: TObject);
var
  i: Integer;
begin
  for i := 0 to BackupDatabasesList.Items.Count - 1 do
  begin
    BackupDatabase(BackupDatabasesList.Items[i]);
  end;
end;

procedure TMainForm.BackupDatabasesListClick(Sender: TObject);
begin
  BringInfo;
end;

procedure TMainForm.BackupDatabasesListDblClick(Sender: TObject);
begin

end;

procedure TMainForm.Button1Click(Sender: TObject);
begin

end;

procedure TMainForm.ChangeBtnClick(Sender: TObject);
var
  cmd: TmncPGCommand;
begin
  if BackupDatabasesList.ItemIndex >= 0 then
  begin
    OpenPG('postgres');
    try
      cmd := PGSession.CreateCommand as TmncPGCommand;
      try
        cmd.SQL.Text := 'ALTER ROLE ' + UserNameEdit.Text + ' WITH PASSWORD ''' + NewPasswordEdit.Text + '''';
        cmd.Execute;
        MsgBox.Show('Password changed successfully');
      finally
        cmd.Free;
      end;
    finally
      ClosePG;
    end;
  end;
end;

procedure TMainForm.CopyBtnClick(Sender: TObject);
var
  DB, ToName: string;
begin
  if DatabasesCbo.ItemIndex >= 0 then
  begin
    DB := DatabasesCbo.Items[DatabasesCbo.ItemIndex];
    ToName := DB;
    if MsgBox.Input(ToName, 'You want copy: ' + DB + ' to?') then
    begin
      if DB = ToName then
        MsgBox.Show('you cant copy on same database')
      else
      begin
        MsgBox.ShowStatus(Self, 'Coping ' + DB + ' to ' + ToName);
        OpenPG('postgres', False);
        try
          CopyDatabase(DB, ToName);
          DatabasesCbo.Items[DatabasesCbo.ItemIndex] := ToName;
        finally
          ClosePG(False);
          MsgBox.HideStatus(Self);
        end;
      end;
    end;
  end;
end;

procedure TMainForm.LangListCboChange(Sender: TObject);
begin

end;

procedure TMainForm.LangListCboSelect(Sender: TObject);
begin
  CheckLanguage;
end;

procedure TMainForm.CheckLanguage;
begin
  if LangListCbo.ItemIndex = 1 then
  begin
    SetDefaultLang('ar', '', Application.Name);
    if BiDiMode = bdLeftToRight then
    begin
      FlipChildren(True);
      BiDiMode := bdRightToLeft;
    end;
  end
  else
  begin
    SetDefaultLang('en', '', Application.Name);
    if BiDiMode = bdRightToLeft then
    begin
      FlipChildren(True);
      BiDiMode := bdLeftToRight;
    end;
  end;
end;

procedure TMainForm.BackupBtn1Click(Sender: TObject);
begin
  if BackupDatabasesList.ItemIndex >= 0 then
    BackupDatabase(BackupDatabasesList.Items[BackupDatabasesList.ItemIndex]);
end;

procedure TMainForm.BackupBtn2Click(Sender: TObject);
var
  cmd: TmncPGCommand;
  DB: String;
begin
  if BackupDatabasesList.ItemIndex >= 0 then
  begin
    DB := BackupDatabasesList.Items[BackupDatabasesList.ItemIndex];
    OpenPG(DB);
    try
      cmd := PGSession.CreateCommand as TmncPGCommand;
      try
        cmd.SQL.Text := 'select * from "System" where "SysSection" = ''Backup''';
        while cmd.Step do
          Log(cmd.Field['SysIdent'].AsString + ': ' + cmd.Field['SysValue'].AsString);
      finally
        cmd.Free;
      end;
    finally
      ClosePG;
    end;
  end;
end;

procedure TMainForm.ListDatabasesBtnClick(Sender: TObject);
begin
  OpenPG('postgres');
  try
    EnumDatabases(GetKeyShiftState = [ssCtrl]);
    DatabasesCbo.Items.Assign(Databases);
    if DatabasesCbo.Items.Count > 0 then
      DatabasesCbo.ItemIndex := 0;
    Databases.Clear;
  finally
    ClosePG;
  end;
end;

procedure TMainForm.AddDatabaseBtnClick(Sender: TObject);
begin
  if DatabasesCbo.ItemIndex >= 0 then
    BackupDatabasesList.Items.Add(DatabasesCbo.Items[DatabasesCbo.ItemIndex]);
end;

procedure TMainForm.RemoveDatabaseBtnClick(Sender: TObject);
begin
  if BackupDatabasesList.ItemIndex >= 0 then
    BackupDatabasesList.Items.Delete(BackupDatabasesList.ItemIndex);
end;

procedure TMainForm.DropBtnClick(Sender: TObject);
var
  DB: string;
begin
  if DatabasesCbo.ItemIndex >= 0 then
  begin
    DB := DatabasesCbo.Items[DatabasesCbo.ItemIndex];
    if not MsgBox.No('Are you sure you want to drop: ' + DB + '?') then
    begin
      OpenPG('postgres', False);
      MsgBox.ShowStatus(Self, 'Dropping ' + DB);
      try
        DropDatabase(DB);
        DatabasesCbo.Items.Delete(DatabasesCbo.ItemIndex);
      finally
        ClosePG(False);
        MsgBox.HideStatus(Self);
      end;
    end;
  end;
end;

type

  { TPGExecuteObject }

  TPGExecuteObject = class(TExecuteObject)
  public
    PGConn: TmncPGConnection;
    PGSession: TmncPGSession;
    UserName: String;
    Password: String;
    Port: String;
    Database: String;
    Directory: String;
    PointName: string;
    Suffix: String;
    Options: TRestoreOptions;
    procedure OpenPG(vDatabase: String = 'postgres');
    procedure ClosePG;
    constructor Create;
  end;

  { TBackupExecuteObject }

  TBackupExecuteObject = class(TPGExecuteObject)
  public
    FileName: String;
    procedure Prepare(const ConsoleThread: TmnConsoleThread); override;
    procedure Execute(const ConsoleThread: TmnConsoleThread); override;
  end;

  { TRestoreExecuteObject }

  TRestoreExecuteObject = class(TPGExecuteObject)
  public
    procedure Prepare(const ConsoleThread: TmnConsoleThread); override;
    procedure Execute(const ConsoleThread: TmnConsoleThread); override;
  end;

{ TBackupExecuteObject }

procedure TBackupExecuteObject.Prepare(const ConsoleThread: TmnConsoleThread);
var
  cmd: TmncPGCommand;
begin
{  FileName := Directory + Database + '.backup.' + Suffix;
  FileName := ExpandToPath(FileName, Application.Location);}
  {if FileExists(FileName) then
    RenameFile(FileName, FileName + '.' + Suffix);}
  if roCreativeSolutions in Options then
  begin
    OpenPG(Database);
    try
      cmd := PGSession.CreateCommand as TmncPGCommand;
      try
        cmd.SQL.Text := 'insert into "System" ("SysSection", "SysIdent", "SysValue") values (''Backup'', ''LastBeforeBackupDate'', ?SysValue)';
        cmd.SQL.Add('ON CONFLICT ("SysSection", "SysIdent") do update set "SysValue" = ?SysValue');
        cmd.Param['SysValue'].AsString := FormatDateTime('YYYY-MM-DD HH:MM:SS', Now) +  ' at ' + GetLocalName;
        cmd.Execute;
      finally
        cmd.Free;
      end;
    finally
      ClosePG;
    end;
  end;
end;

procedure TBackupExecuteObject.Execute(const ConsoleThread: TmnConsoleThread);
var
  cmd: TmncPGCommand;
  ini: TIniFile;
begin
  if roCreativeSolutions in Options then
  begin
    OpenPG(Database);
    try
      cmd := PGSession.CreateCommand as TmncPGCommand;
      try
        cmd.SQL.Text := 'insert into "System" ("SysSection", "SysIdent", "SysValue") values (''Backup'', ''LastBackupDate'', ?SysValue)';
        cmd.SQL.Add('ON CONFLICT ("SysSection", "SysIdent") DO UPDATE SET "SysValue" = ?SysValue');
        cmd.Param['SysValue'].AsString := FormatDateTime('YYYY-MM-DD HH:MM:SS', Now) +  ' at ' + GetLocalName;
        cmd.Execute;
      finally
        cmd.Free;
      end;
    finally
      ClosePG;
    end;
  end;

  ini := TIniFile.Create(Directory + Database + '.ini');
  try
    if PointName = '' then
    begin
      ini.WriteString('info', 'id', GetLocalName);
      ini.WriteString('info', 'last', ExtractFileName(FileName))
    end;
    ini.WriteString('info', 'lastfile', ExtractFileName(FileName))
  finally
    ini.Free;
  end;
end;

{ TPGExecuteObject }

procedure TPGExecuteObject.OpenPG(vDatabase: String);
begin
  if PGConn = nil then
    PGConn := TmncPGConnection.Create;
  PGConn.UserName := UserName;
  PGConn.Password := Password;
  PGConn.Port := Port;
  PGConn.Resource := vDatabase;
  PGConn.Connect;
  PGSession := PGConn.CreateSession as TmncPGSession;
end;

procedure TPGExecuteObject.ClosePG;
begin
  FreeAndNil(PGSession);
  FreeAndNil(PGConn);
end;

constructor TPGExecuteObject.Create;
begin
  inherited;
  Suffix := FormatDateTime('yyyymmddhhnnss', Now);
end;

procedure TRestoreExecuteObject.Prepare(const ConsoleThread: TmnConsoleThread);
var
  cmd: TmncPGCommand;
  aDatabase: string;
  CreateIt: Boolean;
begin
  if roDirectDatabase in Options then
    aDatabase := Database
  else
    aDatabase := Database + '_temp_' + Suffix;
  OpenPG('postgres');
  try
    cmd := PGSession.CreateCommand as TmncPGCommand;
    try
      cmd.SQL.Text := 'SELECT datname as name FROM pg_database';
      cmd.SQL.Add('WHERE datistemplate = false and datname = ''' + Database + '''');
      if cmd.Execute then
      begin
        if not (roOverwrite in Options) then
          raise Exception.Create('Can''t restore, database is exists ' + Database);
        CreateIt := not (roDirectDatabase in Options);
      end
      else
        CreateIt := True;

      if CreateIt then
      begin
        ConsoleThread.Log('Creating new Database ' + Database, lgStatus);
        cmd.SQL.Text := 'create database "' + aDatabase + '" OWNER = postgres ENCODING = ''UTF8'' CONNECTION LIMIT = -1';
        cmd.Execute;
      end;

    finally
      cmd.Free;
    end;
  finally
    ClosePG;
  end;

  if (roCreateMissedMetaData in Options) or (roDropPublicSchema in Options) then
  begin
    OpenPG(aDatabase);
    try
      cmd := PGSession.CreateCommand as TmncPGCommand;
      try
        if (roDropPublicSchema in Options) then
        begin
          ConsoleThread.Log('Drop Public MetaData ' + Database, lgStatus);
          cmd.SQL.Text := 'drop schema public cascade';
          cmd.Execute;
        end;

        if (roCreateMissedMetaData in Options) then
        begin
          ConsoleThread.Log('Creating Missed MetaData ' + Database, lgStatus);
          cmd.SQL.Text := 'create extension pg_trgm';
          cmd.Execute;
          cmd.SQL.Text := 'create extension intarray';
          cmd.Execute;
          cmd.SQL.Text := 'create extension postgres_fdw';
          cmd.Execute;
          cmd.SQL.Text := 'create extension pgcrypto';
          cmd.Execute;
        end;
      finally
        cmd.Free;
      end;
    finally
      ClosePG;
    end;
  end;

end;

procedure TRestoreExecuteObject.Execute(const ConsoleThread: TmnConsoleThread);
var
  cmd: TmncPGCommand;
  aDatabase: string;
begin
  if (roDirectDatabase in Options) then
    aDatabase := Database
  else
    aDatabase := Database + '_temp_' + Suffix;

  if not (roDirectDatabase in Options) then
  begin
    OpenPG('postgres');
    try
      cmd := PGSession.CreateCommand as TmncPGCommand;
      try
        ConsoleThread.Log('Renaming database ' + Database, lgStatus);
        cmd.SQL.Text := 'SELECT datname as name FROM pg_database';
        cmd.SQL.Add('WHERE datistemplate = false and datname = ''' + Database + '''');
        if cmd.Execute then
        begin
          if roOverwrite in Options then
          begin
            cmd.SQL.Text := 'alter database "' + Database + '" rename to "' + Database + '.old_' + Suffix + '"';
            cmd.Execute;
          end
          else
            raise Exception.Create('Can''t restore, database is exists ' + Database);
        end;
        ConsoleThread.Log('Rename new Database ' + Database, lgStatus);
        cmd.SQL.Text := 'alter database "' + aDatabase + '" rename to "' + Database + '"';
        ConsoleThread.Log('Renamed database ' + Database, lgStatus);
        cmd.Execute;
      finally
        cmd.Free;
      end;
    finally
      ClosePG;
    end;
  end;

  if roCreativeSolutions in Options then
  begin
    OpenPG(aDatabase);
    try
      cmd := PGSession.CreateCommand as TmncPGCommand;
      try
        cmd.SQL.Text := 'insert into "System" ("SysSection", "SysIdent", "SysValue") values (''Backup'', ''LastRestoreDate'', ?SysValue)';
        cmd.SQL.Add('on conflict ("SysSection", "SysIdent") do update set "SysValue" = ?SysValue');
        cmd.Param['SysValue'].AsString := FormatDateTime('YYYY-MM-DD HH:MM:SS', Now) +  ' at ' + GetLocalName;
        cmd.Execute;
      finally
        cmd.Free;
      end;
    finally
      ClosePG;
    end;
  end;
end;

procedure TMainForm.RestoreDatabase(DB: String; APointName: String);
var
  o: TRestoreExecuteObject;
  cmd: String;
  ini: TIniFile;
  filename: string;
  aDatabase: string;
begin
  o := TRestoreExecuteObject.Create;
  o.UserName := UserNameEdit.Text;
  o.Password := PasswordEdit.Text;
  o.Port := GetPort;
  o.Database := DB;
  o.Directory := GetBackupDBDirectory(o.Database);
  o.Options := [roOverwrite] + RO(CSProductsChk.Checked, roCreativeSolutions);
  o.PointName := APointName;

  if APointName <> '' then
    o.Directory := IncludeTrailingPathDelimiter(o.Directory + 'points');
  if APointName <> '' then
    filename := o.Directory + APointName +'.backup'
  else
  begin
    ini := TIniFile.Create(GetBackupDBDirectory(DB) + DB + '.ini');
    try
       filename := ini.ReadString('info', 'last', '');
    finally
      ini.Free;
    end;
    if filename = '' then
    begin
      MsgBox.Error('There is no last file to restore');
      exit;
    end;
    filename := o.Directory + filename;
  end;
  if roDirectDatabase in o.Options then
    aDatabase := DB
  else
    aDatabase := DB + '_temp_' + o.Suffix;
  cmd := '--host localhost --port ' + GetPort + ' --username "' + o.UserName + '" --dbname "' + aDatabase + '" --password --verbose "' + filename + '"';
  Launch('Restoring: ' + DB + ' file: ' + filename, 'pg_restore.exe', cmd, PasswordEdit.Text, o);
end;

procedure TMainForm.RestoreDatabaseFile(DB: String; AFileName: String; Options: TRestoreOptions);
var
  o: TRestoreExecuteObject;
  aParams: string;
  cmd: String;
  ini: TIniFile;
  aDatabase: string;
  procedure AddParam(s: string);
  begin
{    if aParams <> '' then
      aParams := aParams + ' ';}
    aParams := aParams + ' ' + S;
  end;
begin
  o := TRestoreExecuteObject.Create;
  o.UserName := UserNameEdit.Text;
  o.Password := PasswordEdit.Text;
  o.Port := GetPort;
  o.Database := DB;
  o.Directory := GetBackupDBDirectory(o.Database);
  o.Options := Options + RO(CSProductsChk.Checked, roCreativeSolutions);

  if AFileName = '' then
  begin
    ini := TIniFile.Create(GetBackupDBDirectory(DB) + DB + '.ini');
    try
       AFileName := ini.ReadString('info', 'last', '');
    finally
      ini.Free;
    end;
    if AFileName = '' then
    begin
      MsgBox.Error('There is no last file to restore');
      exit;
    end;
    AFileName := o.Directory + AFileName;
  end;

  if RestoreCleanErrorChk.Checked then
    AddParam('--clean --if-exists');

  if roRestorePublicSchemaOnly in Options then
    AddParam('--schema=public');

  if roRestroreDataOnly in Options then
    AddParam('--data-only');

  if roDirectDatabase in Options then
    aDatabase := DB
  else
    aDatabase := DB + '_temp_' + o.Suffix;
  cmd := '--host localhost --port ' + GetPort + ' --username "' + o.UserName + '" ' + aParams + ' --dbname "' + aDatabase + '" --password --verbose "' + AFileName + '"';

  Launch('Restoring: ' + DB + ' file: ' + AFileName, 'pg_restore.exe', cmd, PasswordEdit.Text, o, roIgnoreError in Options);
end;

procedure TMainForm.BackupDatabase(DB: String; APointName: string);
var
  cmd: String;
  o: TBackupExecuteObject;
begin
  //o.Overwrite := Overwrite;
  //"SET PGPASSWORD=<password>"
  o := TBackupExecuteObject.Create;
  o.UserName := UserNameEdit.Text;
  o.Password := PasswordEdit.Text;
  o.Port := GetPort;
  o.Options := RO(CSProductsChk.Checked, roCreativeSolutions);
  o.Database := DB;
  o.Directory := GetBackupDBDirectory(o.Database);
  if APointName <> '' then
    o.Directory := IncludeTrailingPathDelimiter(o.Directory + 'points');
  if APointName <> '' then
    o.FileName := o.Directory + APointName + '.backup'
  else
    o.FileName := o.Directory + DB + '.' + o.Suffix + '.backup';
  o.FileName := ExpandToPath(o.FileName, Application.Location);
  ForceDirectories(ExtractFilePath(o.FileName));
  cmd := '';
  cmd := cmd + ' -v --host localhost --port ' + GetPort + ' --password --username "' + o.UserName + '"';
  if PublicSchemaChk.Checked then
    cmd := cmd + ' --schema=public'
  else
    cmd := cmd + ' --blobs';
  if QuickBackupChk.Checked then
    cmd := cmd + ' -T ' + sExcludeTables;
  cmd := cmd + ' --format custom --compress=9 --file "' + o.FileName + '" "' + DB + '"';

  //cmd := cmd + ' --format tar --blobs --file "' + filename + '" "' + DB + '"';
  Launch('Backuping: ' + DB + ' file: ' + o.FileName, 'pg_dump.exe', cmd, PasswordEdit.Text, o);
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  OptionsTab.PageIndex := 0;
end;

procedure TMainForm.ClearLogMnuClick(Sender: TObject);
begin
  LogEdit.Clear;
end;

procedure TMainForm.ExploreFolder(AFolder, FileName: string);
var
  s: string;
begin
  s := '';
{$ifdef WINDOWS}
  //ShellExecute(0, 'open', 'explorer.exe', PChar('/select,"' + Engine.Files.Current.Name + '"'), nil, SW_SHOW);
  RunCommand('Explorer', ['/select,"' + AFolder + FileName + '"'], s);
{$else}
  RunCommand('xdg-open', [aDirectory + FileName], s);
{$endif}
end;

procedure TMainForm.OpenFolderBtnClick(Sender: TObject);
var
  aDatabase, aDirectory, s: string;
  ini: TIniFile;
begin
  if BackupDatabasesList.ItemIndex >= 0 then
  begin
    aDatabase := BackupDatabasesList.Items[BackupDatabasesList.ItemIndex];
    aDirectory := GetBackupDBDirectory(aDatabase);
    ini := TIniFile.Create(aDirectory + aDatabase + '.ini');
    try
      s := ini.ReadString('info', 'lastfile', '');
    finally
      ini.Free;
    end;
    ExploreFolder(aDirectory, s);
  end;
end;

procedure TMainForm.RenameBtnClick(Sender: TObject);
var
  DB, ToName: string;
begin
  if DatabasesCbo.ItemIndex >= 0 then
  begin
    DB := DatabasesCbo.Items[DatabasesCbo.ItemIndex];
    ToName := DB;
    if MsgBox.Input(ToName, 'You want to rename: ' + DB + ' to?') then
    begin
      if DB = ToName then
        MsgBox.Show('Nothing to do')
      else
      begin
        OpenPG('postgres', False);
        MsgBox.ShowStatus(Self, 'Dropping ' + DB + ' to ' + ToName);
        try
          RenameDatabase(DB, ToName);
          DatabasesCbo.Items[DatabasesCbo.ItemIndex] := ToName;
        finally
          ClosePG(False);
        end;
        MsgBox.HideStatus(Self);
      end;
    end;
  end;
end;

procedure TMainForm.RestoreLastBtn1Click(Sender: TObject);
var
  db: string;
begin
  if BackupDatabasesList.ItemIndex >= 0 then
  begin
    db := BackupDatabasesList.Items[BackupDatabasesList.ItemIndex];
    //if not MsgBox.No('Are you sure you want to restore database?') then
    if MsgBox.Input(DB, 'Enter then name of new Database to restore') then
      RestoreDatabaseFile(DB, '',
        [roDirectDatabase] +
        RO(RestoreFileOverwriteChk.Checked, roOverwrite) +
        RO(RestoreFileIgnoreErrorChk.Checked, roIgnoreError) +
        RO(CreateMissedMetaDataChk.Checked, roCreateMissedMetaData) +
        RO(DropPublicSchemaChk.Checked, roDropPublicSchema) +
        RO(RestorePublicSchemaOnlyChk.Checked, roRestorePublicSchemaOnly)
      );
  end;
end;

procedure TMainForm.RestoreLastBtnClick(Sender: TObject);
var
  db: string;
begin
  if BackupDatabasesList.ItemIndex >= 0 then
  begin
    db := BackupDatabasesList.Items[BackupDatabasesList.ItemIndex];
    if not MsgBox.No('Are you sure you want to restore database?') then
      RestoreDatabase(db);
   end;
end;

procedure TMainForm.RestoreNewFromFileBtnClick(Sender: TObject);
var
  DB: String;
begin
  DB := ExtractFileNameWithoutExt(ExtractFileName(BackupFileNameEdit.Text));
  if MsgBox.Input(DB, 'Enter then name of new Database to restore') then
    RestoreDatabaseFile(DB, BackupFileNameEdit.Text,
      [roDirectDatabase] +
      RO(RestoreFileOverwriteChk.Checked, roOverwrite) +
      RO(RestoreFileIgnoreErrorChk.Checked, roIgnoreError) +
      RO(CreateMissedMetaDataChk.Checked, roCreateMissedMetaData) +
      RO(DropPublicSchemaChk.Checked, roDropPublicSchema) +
      RO(RestoreDataOnlyChk.Checked, roRestroreDataOnly) +
      RO(RestorePublicSchemaOnlyChk.Checked, roRestorePublicSchemaOnly)
    );
end;

procedure TMainForm.BrowseBackupBtnClick(Sender: TObject);
begin
  with TOpenDialog.Create(Self) do
  begin
    Filter := '*.backup';
    FileName := '*.backup';
    if Execute then
      BackupFileNameEdit.Text := FileName;
    Free;
  end;
end;

procedure TMainForm.RestoreByDateBtnClick(Sender: TObject);
var
  files, names: TStringList;
  i: Integer;
  Dir, DB: string;
  s, fname: string;
begin
  if BackupDatabasesList.ItemIndex >= 0 then
  begin
    files := TStringList.Create;
    names := TStringList.Create;
    try
      DB := BackupDatabasesList.Items[BackupDatabasesList.ItemIndex];
      Dir := ExpandToPath(GetBackupDBDirectory(DB), Application.Location);
      i := -1;
      EnumFiles(files, Dir, DB + '.*.backup');
      files.Sort;
      for fname in files do
      begin
        s := SubStr(fname, '.', 1);
        if s = '' then
          s := 'Last Backup'
        else
          //yyyymmddhhnnss
          s := MidStr(s, 1, 4) + '-' + MidStr(s, 5, 2) + '-' + MidStr(s, 7, 2) + ' ' + MidStr(s, 9, 2) + ':' + MidStr(s, 11, 4) + ':' + MidStr(s, 13, 4);
        names.Add(s);
      end;

      if MsgBox.List(i, 'Select a point to restore', names) then
      begin
        RestoreDatabaseFile(DB, Dir + files[i], [roOverwrite]);
        files.CommaText
      end;
    finally
      files.Free;
      names.Free;
    end;
  end;
end;

procedure TMainForm.RestoreAllBtnClick(Sender: TObject);
var
  i: Integer;
begin
  if not MsgBox.No('Are you sure you want to RESTORE all databases?') then
    for i := 0 to BackupDatabasesList.Items.Count - 1 do
    begin
      RestoreDatabase(BackupDatabasesList.Items[i]);
    end;
end;

procedure TMainForm.RestorFromFileBtnClick(Sender: TObject);
var
  DB, Selected: String;
begin
  with TOpenDialog.Create(Self) do
  begin
    Filter := '*.backup';
    FileName := '*.backup';
    if Execute then
    begin
      DB := ExtractFileNameWithoutExt(ExtractFileName(FileName));
      if Pos('.', DB) > 0 then
        DB := ExtractFileNameWithoutExt(DB);

      if BackupDatabasesList.ItemIndex >= 0 then
      begin
        Selected := BackupDatabasesList.Items[BackupDatabasesList.ItemIndex];
        if not SameText(DB, Selected) then
          MsgBox.Error('Please select backup file with same name of database selected')
        else if not MsgBox.No('Are you sure you want to restore database?') then
          RestoreDatabaseFile(BackupDatabasesList.Items[BackupDatabasesList.ItemIndex], FileName, [roOverwrite]);
      end
      else
        if MsgBox.Input(DB, 'Enter then name of new Database to restore') then
          RestoreDatabaseFile(DB, FileName, []);
    end;
    Free;
  end;
end;

procedure EnumFiles(AList: TStrings; const SearchPath: String; SearchMask: String; SearchSubDirs: Boolean);
var
  Files: TStringList;
  i: Integer;
begin
  Files := TStringList.Create;
  try
    FindAllFiles(Files, SearchPath, SearchMask, SearchSubDirs);
    for i := 0 to Files.Count -1 do
      AList.Add(ExtractFileNameWithoutExt(ExtractFileName(Files[i])));
  finally
    Files.Free;
  end;
end;

procedure TMainForm.RestorePointBtnClick(Sender: TObject);
var
  files: TStringList;
  i: Integer;
  DB: string;
begin
  if BackupDatabasesList.ItemIndex >= 0 then
  begin
    files := TStringList.Create;
    try
      DB := BackupDatabasesList.Items[BackupDatabasesList.ItemIndex];
      i := -1;
      EnumFiles(files, GetBackupDBDirectory(DB) + 'points', '*.backup', false);
      if MsgBox.List(i, 'Select a point to restore', files) then
      begin
        RestoreDatabase(DB, files[i]);
      end;
    finally
      files.Free;
    end;
  end;
end;

procedure TMainForm.SavePointBtnClick(Sender: TObject);
var
  APoint: String;
begin
  if BackupDatabasesList.ItemIndex >= 0 then
  begin
    APoint := '';
    if MsgBox.Input(APoint, 'Enter then name of new Database to restore') and (APoint <> '') then
      BackupDatabase(BackupDatabasesList.Items[BackupDatabasesList.ItemIndex], APoint);
  end;
end;

procedure TMainForm.ScrollMnuClick(Sender: TObject);
begin

end;

procedure TMainForm.StatusTimerTimer(Sender: TObject);
begin
  InfoPanel.Caption := '';
  StatusTimer.Enabled := False;
end;

procedure TMainForm.StopBtnClick(Sender: TObject);
begin
  FStop := True;
  if ConsoleThread <> nil then
  begin
    ConsoleThread.Kill;
    ConsoleThread.Terminate;
    //ConsoleThread.WaitFor;
    //FreeAndNil(ConsoleThread);
  end;
end;

function TMainForm.GetPort: String;
begin
  Result := PortEdit.Text;
  if Result = '' then
    Result := '5432';
end;

procedure TMainForm.Log(S: String; Kind: TmnLogKind);
begin
  case Kind of
    lgLog: ;
    lgStatus:
    begin
      InfoPanel.Caption := S;
    end;
    lgDone:
    begin
      InfoPanel.Caption := S;
      StatusTimer.Enabled := True;
      LogEdit.Lines.Add('');
    end;
    lgMessage: MsgBox.Show(S);
  end;
  LogEdit.Lines.Add(Trim(S));
  if ScrollMnu.Checked then
    LogEdit.CaretY := LogEdit.Lines.Count;
end;

procedure TMainForm.ConsoleTerminated(Sender: TObject);
begin
  if not FDestroying then
  begin
    if ConsoleThread <> nil then
    begin
      if ConsoleThread.Status = 0 then
        Log(ConsoleThread.Message + ' Done', lgDone)
      else
        Log('Error look the log', lgMessage);
      ConsoleThread := nil;
      //FreeAndNil(ConsoleThread); //nop
      if not FStop then
        Resume;
    end;
  end;
end;

procedure TMainForm.BringInfo;
var
  db: string;
  ini: TIniFile;
begin
  if BackupDatabasesList.ItemIndex >= 0 then
  begin
      db := BackupDatabasesList.Items[BackupDatabasesList.ItemIndex];
      ini := TIniFile.Create(GetBackupDBDirectory(db) + db+ '.ini');
      try
         BackupDeviceIDLbl.Caption := ini.ReadString('info', 'id', '');
      finally
        ini.Free;
      end;
  end
  else
    BackupDeviceIDLbl.Caption := '';
end;

function TMainForm.GetBackupDBDirectory: string;
begin
  if BackupDatabasesList.ItemIndex >=0 then
    Result := GetBackupDBDirectory(BackupDatabasesList.Items[BackupDatabasesList.ItemIndex])
  else
    Result := GetBackupDirectory;
end;

function TMainForm.GetBackupDBDirectory(DB: string): string;
begin
  Result := GetBackupDirectory;
  Result := IncludeTrailingPathDelimiter(Result + DB);
end;

function TMainForm.GetBackupDirectory: String;
begin
  if DirectoryEdit.Text <> '' then
    Result := ExpandToPath(IncludeTrailingPathDelimiter(DirectoryEdit.Text), Application.Location)
  else
    Result := Application.Location;
end;

function TMainForm.GetPGDirectory: String;
begin
  Result := InternalPGDirectory;
end;

procedure TMainForm.EnumDatabases(vOld: Boolean);
var
  cmd: TmncPGCommand;
begin
  Databases.Clear;
  cmd := PGSession.CreateCommand as TmncPGCommand;
  try
    cmd.SQL.Text := 'SELECT datname as name FROM pg_database';
    cmd.SQL.Add('WHERE datistemplate = false and datname <> ''postgres''');
    if CSProductsChk.Checked then
      cmd.SQL.Add('and datname <> ''CreativeSolutions''');
    if vOld then
      cmd.SQL.Add('and ')
    else
      cmd.SQL.Add('and not ');
    cmd.SQL.Add('(datname like ''%_old%''');
    cmd.SQL.Add('or datname like ''%.old%''');
    cmd.SQL.Add('or datname like ''%.temp%''');
    cmd.SQL.Add('or datname like ''%_temp%'')');
    cmd.SQL.Add('order by datname');

    if cmd.Execute then
    begin
      while not cmd.Done do
      begin
        Databases.Add(cmd.Field['name'].AsString);
        //Log(cmd.Field['name'].AsString);
        cmd.Next;
      end;
    end;
  finally
    cmd.Free;
  end;
end;

procedure TMainForm.DropDatabase(ADatabase: String);
begin
  InfoPanel.Caption := 'Dropping database ' + ADatabase;
  PGConn.Execute('drop database "' + ADatabase + '"');
  Log('Database Dropped: "' + ADatabase + '"', lgStatus);
end;

procedure TMainForm.RenameDatabase(ADatabase, AToName: String);
begin
  InfoPanel.Caption := 'Renaming database ' + AToName;
  PGConn.Execute('alter database "' + ADatabase + '" rename to "' + AToName + '"');
  Log('Database Renamed: "' + ADatabase + '"', lgStatus);
end;

procedure TMainForm.CopyDatabase(ADatabase, AToName: String);
begin
  InfoPanel.Caption := 'Copying database ' + ADatabase + ' to ' + AToName;
  PGConn.Execute('CREATE DATABASE "' + AToName + '" TEMPLATE "' + ADatabase + '"');
  Log('Database copied: "' + ADatabase + ' to ' + AToName + '"', lgStatus);
end;

procedure TMainForm.OpenPG(vDatabase: String; StartSession: Boolean);
begin
  if PGConn = nil then
    PGConn := TmncPGConnection.Create;
  PGConn.UserName := UserNameEdit.Text;
  PGConn.Password := PasswordEdit.Text;
  PGConn.Port := GetPort;
  PGConn.Resource := vDatabase;

  PGConn.ClientEncoding := 'UNICODE';
  PGConn.ByteaOutput := 'escape';
  PGConn.DateStyle := 'iso, mdy';

  PGConn.Connect;
  //PGConn.AutoStart : = true;
  PGSession := PGConn.CreateSession as TmncPGSession;
  if StartSession then
    PGSession.Start;
end;

procedure TMainForm.ClosePG(StopSession: Boolean);
begin
  if PGConn <> nil then
  begin
    if StopSession then
      PGSession.Commit;
    FreeAndNil(PGSession);
    FreeAndNil(PGConn);
  end;
end;

procedure TMainForm.Launch(vMessage, vExecutable, vParameters, vPassword: String; vExecuteObject: TExecuteObject; IgnoreError: Boolean);
var
  aConsoleThread: TmnConsoleThread;
begin
  if GetPGDirectory <> '' then
    vExecutable := IncludeTrailingPathDelimiter(GetPGDirectory) + vExecutable;
  aConsoleThread := TmnConsoleThread.Create(vExecutable, GetBackupDirectory, vParameters, @Log);
  aConsoleThread.OnTerminate := @ConsoleTerminated;
  aConsoleThread.Password := vPassword;
  aConsoleThread.Message := vMessage;
  aConsoleThread.ExecuteObject := vExecuteObject;
  aConsoleThread.IgnoreError := IgnoreError;
  PoolThread.Add(aConsoleThread);
  Resume;
end;

procedure TMainForm.Resume;
begin
  if (PoolThread.Count > 0) then
  begin
    if (ConsoleThread = nil) then
    begin
      ConsoleThread := PoolThread.Extract(PoolThread.First) as TmnConsoleThread;
      //Log(ConsoleThread.Message);
      InfoPanel.Caption := ConsoleThread.Message;
      Application.ProcessMessages;
      ConsoleThread.Start;
    end;
  end
  else
  begin
    Log('Finished', lgMessage);
    InfoPanel.Caption := '';
  end;
end;

procedure TMainForm.LoadIni;
var
  ini: TIniFile;
begin
  ini := TIniFile.Create(IniPath + 'pgtools.ini');
  LangListCbo.Text := ini.ReadString('options', 'Language', 'English');
  CheckLanguage;
  UserNameEdit.Text := ini.ReadString('options', 'username', 'postgres');
  SavePasswordChk.Checked := ini.ReadBool('options', 'savepassword', False);
  if SavePasswordChk.Checked then
    PasswordEdit.Text := ini.ReadString('options', 'password', '')
  else
    PasswordEdit.Text := '';
  PortEdit.Text := ini.ReadString('options', 'port', '');
  DirectoryEdit.Text := ini.ReadString('options', 'directory', './');
  ExportMode := ini.ReadBool('options', 'expert', False);
  if ExportMode then
  begin
    ExportTab.TabVisible := True;
    AdminTab.TabVisible := True;
    AdminPanel.Visible := True;
    RestorFromFileBtn.Visible := True;
    RestoreByDateBtn.Visible := True;
  end;
  //PublicSchemaChk.Checked := ini.ReadBool('options', 'PublicSchema', False);
  RestoreFileOverwriteChk.Checked := ini.ReadBool('options', 'RestoreFileOverwriteChk', False);
end;

procedure TMainForm.DetectPGDirectory;
  function Check(f: string): Boolean;
  begin
    Result := FileExists(f);
    if Result then
      InternalPGDirectory := ExtractFilePath(f);
  end;
begin
  if not Check(Application.Location + 'libpq.dll') then
  {$ifdef win32}
    Check(Application.Location + 'bin32\libpq.dll');
  {$else}
    {$ifdef win64}
      Check(Application.Location + 'bin64\libpq.dll')
    {$endif}
  {$endif}
end;

constructor TMainForm.Create(TheOwner: TComponent);
var
  i: Integer;
  ini: TIniFile;
  s: String;
  aStrings: TStringList;
begin
  inherited;
  UserNameEdit.Text := 'postgres';
  DetectPGDirectory;
  Log('This Device: ' + GetLocalName);
  PoolThread := TObjectList.Create;
  ini := TIniFile.Create(Application.Location + 'pgtools.ini');
  Portable := ini.ReadBool('options', 'portable', True);
  ini.Free;
  if Portable then
    IniPath := Application.Location
  else
    IniPath := GetAppConfigDir(False);
  LoadIni;
  RestoreFilePageControl.TabIndex := 0;
  i := 0;

  //if ini.ReadInteger('options', 'version', 1) < 2 then
  aStrings := TStringList.Create;
  try
    ini.ReadSectionRaw('data', aStrings);
    for i :=0 to aStrings.Count -1 do
    begin
      s := aStrings.ValueFromIndex[i];
      if s <> '' then
        BackupDatabasesList.Items.Add(s);
    end;
  finally
    aStrings.Free;
    ini.Free;
  end;

  if BackupDatabasesList.Items.Count > 0 then
    BackupDatabasesList.ItemIndex := 0;
  Databases := TStringList.Create;
  BringInfo;
  if GetPGDirectory <> '' then
    SetCurrentDir(GetPGDirectory);
end;

destructor TMainForm.Destroy;
var
  i: Integer;
  ini: TIniFile;
begin
  FDestroying := True;
  FStop := True;
  PoolThread.Clear;
  if ConsoleThread <> nil then
  begin
    ConsoleThread.Kill;
    ConsoleThread.Terminate;
    //ConsoleThread.WaitFor;
    //FreeAndNil(ConsoleThread);
  end;
  ClosePG;
  FreeAndNil(Databases);

  ini := TIniFile.Create(IniPath + 'pgtools.ini');
  ini.WriteInteger('options', 'version', 2);
  ini.WriteString('options', 'Language', LangListCbo.Text);
  ini.WriteString('options', 'username', UserNameEdit.Text);
  ini.WriteBool('options', 'savepassword', SavePasswordChk.Checked);
  if SavePasswordChk.Checked then
    ini.WriteString('options', 'password', PasswordEdit.Text);
  ini.WriteString('options', 'port', PortEdit.Text);
  ini.WriteString('options', 'directory', DirectoryEdit.Text);
  ini.WriteBool('options', 'expert', ExportTab.TabVisible);
  ini.WriteBool('options', 'portable', Portable);
  //ini.WriteBool('options', 'PublicSchema', PublicSchemaChk.Checked); //do not save it, it is for special
  ini.WriteBool('options', 'RestoreFileOverwrite', RestoreFileOverwriteChk.Checked);
  ini.EraseSection('data');
  for i := 0 to BackupDatabasesList.Items.Count - 1 do
    ini.WriteString('data', 'data' + IntToStr(i), BackupDatabasesList.Items[i]);
  ini.Free;
  FreeAndNil(PoolThread);
  inherited;
   Screen.Width
end;

initialization
end.
