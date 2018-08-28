unit MainForms;

{**
*  This file is part of the "Creative Solutions PGTools http://www.cserp.org/"
 *
 * @license   mit(https://opensource.org/licenses/MIT)
 *
 * @author    Zaher Dirkey <zaher at parmaja dot com>
 *}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  ComCtrls, Menus, IniFiles, Contnrs, SynEdit, mncPostgre, MsgBox, GUIMsgBox,
  ConsoleProcess, FileUtil, mnUtils;

type
  { TMainForm }

  TMainForm = class(TForm)
    BackupBtn: TButton;
    BackupBtn1: TButton;
    BackupBtn2: TButton;
    CleanBtn: TButton;
    CleanBtn1: TButton;
    CleanBtn4: TButton;
    CleanBtn5: TButton;
    BackupFileNameEdit: TEdit;
    DropBtn: TButton;
    CSProductsChk: TCheckBox;
    DBDirectoryChk: TCheckBox;
    DirectoryEdit: TEdit;
    RenameBtn: TButton;
    Label3: TLabel;
    NewPasswordEdit: TEdit;
    AdminPanel: TPanel;
    PGDirectoryEdit: TEdit;
    Image1: TImage;
    Label1: TLabel;
    Label2: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label9: TLabel;
    MenuItem1: TMenuItem;
    OptionsPageControl: TPageControl;
    PasswordEdit: TEdit;
    PGPageControl: TPageControl;
    PopupMenu1: TPopupMenu;
    CleanBtn3: TButton;
    DatabasesCbo: TComboBox;
    InfoPanel: TPanel;
    BackupDatabasesList: TListBox;
    PortEdit: TEdit;
    PublicSchemaChk: TCheckBox;
    RestoreBtn: TButton;
    RestoreBtn1: TButton;
    RestoreBtn2: TButton;
    RestoreBtn3: TButton;
    SavePasswordChk: TCheckBox;
    SelectPGFolderBtn: TButton;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    ExportTab: TTabSheet;
    LogEdit: TSynEdit;
    TabSheet3: TTabSheet;
    TabSheet4: TTabSheet;
    TabSheet5: TTabSheet;
    AdminSheet: TTabSheet;
    UserNameEdit: TEdit;
    procedure BackupBtn1Click(Sender: TObject);
    procedure BackupBtn2Click(Sender: TObject);
    procedure BackupBtnClick(Sender: TObject);
    procedure CleanBtn1Click(Sender: TObject);
    procedure CleanBtn3Click(Sender: TObject);
    procedure CleanBtn4Click(Sender: TObject);
    procedure CleanBtn5Click(Sender: TObject);
    procedure DropBtnClick(Sender: TObject);
    procedure CleanBtnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure MenuItem1Click(Sender: TObject);
    procedure RenameBtnClick(Sender: TObject);
    procedure RestoreBtn1Click(Sender: TObject);
    procedure RestoreBtn2Click(Sender: TObject);
    procedure RestoreBtn3Click(Sender: TObject);
    procedure RestoreBtnClick(Sender: TObject);
    procedure SelectPGFolderBtnClick(Sender: TObject);
  private
    PoolThread: TObjectList;
    ConsoleThread: TmnConsoleThread;
    function GetPort: String;
    procedure BackupDatabase(DB: String);
    procedure RestoreDatabase(DB: String; filename: String = ''; Overwrite: Boolean = True);
    procedure Log(S: String; Kind: TmnLogKind = lgLog);
    procedure ConsoleTerminated(Sender: TObject);
  protected
    PGConn: TmncPGConnection;
    PGSession: TmncPGSession;
    Databases: TStringList;
    PGPathBin: String;
    IniPath: String;
    Portable: Boolean;
    function GetCurrentDirectory: String;
    procedure EnumDatabases(vOld: Boolean);
    procedure DropDatabase(ADatabase: String);
    procedure RenameDatabase(ADatabase, AToName: String);
    procedure OpenPG(vDatabase: String = 'postgres'; StartSession: Boolean = True);
    procedure ClosePG(StopSession: Boolean = True);
    procedure Launch(vMessage, vExecutable, vParameters, vPassword: String; vExecuteObject: TExecuteObject = nil);
    procedure Resume;
    procedure LoadIni;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

{ TMainForm }

procedure TMainForm.CleanBtnClick(Sender: TObject);
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
  Log('Clean Done', lgStatus);
  Databases.Clear;
end;

procedure TMainForm.BackupBtnClick(Sender: TObject);
var
  i: Integer;
begin
  for i := 0 to BackupDatabasesList.Items.Count - 1 do
  begin
    BackupDatabase(BackupDatabasesList.Items[i]);
  end;
end;

procedure TMainForm.CleanBtn1Click(Sender: TObject);
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
        Msg.Show('Password changed successfully');
      finally
        cmd.Free;
      end;
    finally
      ClosePG;
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

procedure TMainForm.CleanBtn3Click(Sender: TObject);
begin
  OpenPG('postgres');
  try
    EnumDatabases(False);
    DatabasesCbo.Items.Assign(Databases);
    if DatabasesCbo.Items.Count > 0 then
      DatabasesCbo.ItemIndex := 0;
    Databases.Clear;
  finally
    ClosePG;
  end;
end;

procedure TMainForm.CleanBtn4Click(Sender: TObject);
begin
  if DatabasesCbo.ItemIndex >= 0 then
    BackupDatabasesList.Items.Add(DatabasesCbo.Items[DatabasesCbo.ItemIndex]);
end;

procedure TMainForm.CleanBtn5Click(Sender: TObject);
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
    if not Msg.No('Are you sure you want to drop: ' + DB + '?') then
    begin
      OpenPG('postgres', False);
      try
        DropDatabase(DB);
        DatabasesCbo.Items.Delete(DatabasesCbo.ItemIndex);
      finally
        ClosePG(False);
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
    Overwrite: Boolean;
    Suffix: String;
    CSProducts: Boolean;
    procedure OpenPG(vDatabase: String = 'postgres');
    procedure ClosePG;
    constructor Create;
  end;

  { TBackupExecuteObject }

  TBackupExecuteObject = class(TPGExecuteObject)
  public
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
  filename: String;
begin
  filename := Directory + Database + '.backup';
  filename := ExpandToPath(filename, Application.Location);
  if FileExists(filename) then
    RenameFile(filename, filename + '.' + Suffix);
  if CSProducts then
  begin
    OpenPG(Database);
    try
      cmd := PGSession.CreateCommand as TmncPGCommand;
      try
        cmd.SQL.Text := 'insert into "System" ("SysSection", "SysIdent", "SysValue") values (''Backup'', ''LastBeforeBackupDate'', ?SysValue)';
        cmd.SQL.Add('ON CONFLICT ("SysSection", "SysIdent") do update set "SysValue" = ?SysValue');
        cmd.Param['SysValue'].AsString := FormatDateTime('YYYY-MM-DD HH:MM:SS', Now);
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
begin
  if CSProducts then
  begin
    OpenPG(Database);
    try
      cmd := PGSession.CreateCommand as TmncPGCommand;
      try
        cmd.SQL.Text := 'insert into "System" ("SysSection", "SysIdent", "SysValue") values (''Backup'', ''LastBackupDate'', ?SysValue)';
        cmd.SQL.Add('ON CONFLICT ("SysSection", "SysIdent") DO UPDATE SET "SysValue" = ?SysValue');
        cmd.Param['SysValue'].AsString := FormatDateTime('YYYY-MM-DD HH:MM:SS', Now);
        cmd.Execute;
      finally
        cmd.Free;
      end;
    finally
      ClosePG;
    end;
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
begin
  OpenPG('postgres');
  try
    cmd := PGSession.CreateCommand as TmncPGCommand;
    try
      cmd.SQL.Text := 'SELECT datname as name FROM pg_database';
      cmd.SQL.Add('WHERE datistemplate = false and datname = ''' + Database + '''');
      if cmd.Execute then
      begin
        if not Overwrite then
          raise Exception.Create('Can''t restore database is exists ' + Database);
      end;

      ConsoleThread.Log('Creating new Database ' + Database, lgStatus);
      cmd.SQL.Text := 'create database "' + Database + '_temp_' + Suffix + '"';
      cmd.Execute;
    finally
      cmd.Free;
    end;
  finally
    ClosePG;
  end;
end;

procedure TRestoreExecuteObject.Execute(const ConsoleThread: TmnConsoleThread);
var
  cmd: TmncPGCommand;
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
        if Overwrite then
        begin
          cmd.SQL.Text := 'alter database "' + Database + '" rename to "' + Database + '.old_' + Suffix + '"';
          cmd.Execute;
        end
        else
          raise Exception.Create('Can''t restore database is exists ' + Database);
      end;
      ConsoleThread.Log('Rename new Database ' + Database, lgStatus);
      cmd.SQL.Text := 'alter database "' + Database + '_temp_' + Suffix + '" rename to "' + Database + '"';
      ConsoleThread.Log('Renamed database ' + Database, lgStatus);
      cmd.Execute;
    finally
      cmd.Free;
    end;
  finally
    ClosePG;
  end;

  if CSProducts then
  begin
    OpenPG(Database);
    try
      cmd := PGSession.CreateCommand as TmncPGCommand;
      try
        cmd.SQL.Text := 'insert into "System" ("SysSection", "SysIdent", "SysValue") values (''Backup'', ''LastRestoreDate'', ?SysValue)';
        cmd.SQL.Add('on conflict ("SysSection", "SysIdent") do update set "SysValue" = ?SysValue');
        cmd.Param['SysValue'].AsString := FormatDateTime('YYYY-MM-DD HH:MM:SS', Now);
        cmd.Execute;
      finally
        cmd.Free;
      end;
    finally
      ClosePG;
    end;
  end;
end;

procedure TMainForm.RestoreDatabase(DB: String; filename: String; Overwrite: Boolean);
var
  o: TRestoreExecuteObject;
  cmd: String;
begin
  o := TRestoreExecuteObject.Create;
  o.UserName := UserNameEdit.Text;
  o.Password := PasswordEdit.Text;
  o.Port := GetPort;
  o.CSProducts := CSProductsChk.Checked;
  o.Database := DB;
  o.Directory := IncludeTrailingPathDelimiter(DirectoryEdit.Text);
  if DBDirectoryChk.Checked then
    o.Directory := IncludeTrailingPathDelimiter(o.Directory + DB);
  o.Overwrite := Overwrite;
  if filename = '' then
  begin
    filename := o.Directory + DB + '.backup';
    filename := ExpandToPath(filename, Application.Location);
  end;
  cmd := '--host localhost --port ' + GetPort + ' --username "' + o.UserName + '" --dbname "' + DB + '"_temp_' + o.Suffix + ' --password --verbose "' + filename + '"';
  Launch('Restoring: ' + DB, 'pg_restore.exe', cmd, PasswordEdit.Text, o);
end;

procedure TMainForm.BackupDatabase(DB: String);
var
  filename, cmd: String;
  o: TBackupExecuteObject;
begin
  o := TBackupExecuteObject.Create;
  o.UserName := UserNameEdit.Text;
  o.Password := PasswordEdit.Text;
  o.Port := GetPort;
  o.CSProducts := CSProductsChk.Checked;
  o.Database := DB;
  o.Directory := IncludeTrailingPathDelimiter(DirectoryEdit.Text);
  if DBDirectoryChk.Checked then
    o.Directory := IncludeTrailingPathDelimiter(o.Directory + DB);
  //o.Overwrite := Overwrite;
  //"SET PGPASSWORD=<password>"
  filename := o.Directory + DB + '.backup';
  filename := ExpandToPath(filename, Application.Location);
  ForceDirectories(ExtractFilePath(filename));
  cmd := '';
  cmd := cmd + ' -v --host localhost --port ' + GetPort + ' --password --username "' + o.UserName + '"';
  if PublicSchemaChk.Checked then
    cmd := cmd + ' --schema=public'
  else
    cmd := cmd + ' --blobs';
  if CSProductsChk.Checked then
    cmd := cmd + ' -T "*.\"AppFiles\""';
  cmd := cmd + ' --format custom --compress=9 --file "' + filename + '" "' + DB + '"';

  //cmd := cmd + ' --format tar --blobs --file "' + filename + '" "' + DB + '"';
  Launch('Backuping: ' + DB, 'pg_dump.exe', cmd, PasswordEdit.Text, o);
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  OptionsPageControl.PageIndex := 0;
end;

procedure TMainForm.MenuItem1Click(Sender: TObject);
begin
  LogEdit.Clear;
end;

procedure TMainForm.RenameBtnClick(Sender: TObject);
var
  DB, ToName: string;
begin
  if DatabasesCbo.ItemIndex >= 0 then
  begin
    DB := DatabasesCbo.Items[DatabasesCbo.ItemIndex];
    ToName := DB;
    if Msg.Input(ToName, 'You want to rename: ' + DB + ' to?') then
    begin
      if DB = ToName then
        Msg.Show('Nothing to do')
      else
      begin
        OpenPG('postgres', False);
        try
          RenameDatabase(DB, ToName);
          DatabasesCbo.Items[DatabasesCbo.ItemIndex] := ToName;
        finally
          ClosePG(False);
        end;
      end;
    end;
  end;
end;

procedure TMainForm.RestoreBtn1Click(Sender: TObject);
begin
  if BackupDatabasesList.ItemIndex >= 0 then
    RestoreDatabase(BackupDatabasesList.Items[BackupDatabasesList.ItemIndex]);
end;

procedure TMainForm.RestoreBtn2Click(Sender: TObject);
var
  DB: String;
begin
  DB := ExtractFileNameWithoutExt(ExtractFileName(BackupFileNameEdit.Text));
  if Msg.Input(DB, 'Enter then name of new Database to restore') then
    RestoreDatabase(DB, BackupFileNameEdit.Text, False);
end;

procedure TMainForm.RestoreBtn3Click(Sender: TObject);
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

procedure TMainForm.RestoreBtnClick(Sender: TObject);
var
  i: Integer;
begin
  for i := 0 to BackupDatabasesList.Items.Count - 1 do
  begin
    RestoreDatabase(BackupDatabasesList.Items[i]);
  end;
end;

procedure TMainForm.SelectPGFolderBtnClick(Sender: TObject);
var
  S: String;
begin
  if SelectDirectory('Select where PG bin folder', PGDirectoryEdit.Text, S) then
    PGDirectoryEdit.Text := S;
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
    lgStatus: InfoPanel.Caption := S;
    lgMessage: Msg.Show(S);
  end;
  LogEdit.Lines.Add(S);
  LogEdit.CaretY := LogEdit.Lines.Count;
end;

procedure TMainForm.ConsoleTerminated(Sender: TObject);
begin
  if ConsoleThread.Status = 0 then
    Log(ConsoleThread.Message + ' Done', lgStatus)
  else
    Log('Error look the log', lgMessage);
  FreeAndNil(ConsoleThread);
  Resume;
end;

function TMainForm.GetCurrentDirectory: String;
begin
  Result := IncludeTrailingPathDelimiter(PGDirectoryEdit.Text);
  if Result = '' then
    Result := Application.Location;
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
  Log('Database Renames: "' + ADatabase + '"', lgStatus);
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

procedure TMainForm.Launch(vMessage, vExecutable, vParameters, vPassword: String; vExecuteObject: TExecuteObject);
var
  aConsoleThread: TmnConsoleThread;
begin
  if PGDirectoryEdit.Text <> '' then
    vExecutable := IncludeTrailingPathDelimiter(PGDirectoryEdit.Text) + vExecutable;
  aConsoleThread := TmnConsoleThread.Create(vExecutable, GetCurrentDirectory, vParameters, @Log);
  aConsoleThread.OnTerminate := @ConsoleTerminated;
  aConsoleThread.Password := vPassword;
  aConsoleThread.Message := vMessage;
  aConsoleThread.ExecuteObject := vExecuteObject;
  PoolThread.Add(aConsoleThread);
  Resume;
end;

procedure TMainForm.Resume;
begin
  if (PoolThread.Count > 0) then
  begin
    if (ConsoleThread = nil) then
    begin
      ConsoleThread := PoolThread.Extract(PoolThread.Last) as TmnConsoleThread;
      Log(ConsoleThread.Message);
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
  CSProductsChk.Checked := ini.ReadBool('options', 'CSProducts', True);
  UserNameEdit.Text := ini.ReadString('options', 'username', 'postgres');
  SavePasswordChk.Checked := ini.ReadBool('options', 'savepassword', False);
  if SavePasswordChk.Checked then
    PasswordEdit.Text := ini.ReadString('options', 'password', '')
  else
    PasswordEdit.Text := '';
  PortEdit.Text := ini.ReadString('options', 'port', '');
  DirectoryEdit.Text := ini.ReadString('options', 'directory', './');
  PGDirectoryEdit.Text := ini.ReadString('options', 'PGDirecotry', '');
  ExportTab.TabVisible := ini.ReadBool('options', 'expert', False);
  if ExportTab.TabVisible then
  begin
    AdminSheet.TabVisible := True;
    AdminPanel.Visible := True;
  end;
  DBDirectoryChk.Checked := ini.ReadBool('options', 'DBDirectory', False);
  PublicSchemaChk.Checked := ini.ReadBool('options', 'PublicSchema', False);
end;

constructor TMainForm.Create(TheOwner: TComponent);
var
  i: Integer;
  ini: TIniFile;
  s: String;
begin
  inherited;
  PoolThread := TObjectList.Create;
  ini := TIniFile.Create(Application.Location + 'pgtools.ini');
  Portable := ini.ReadBool('options', 'portable', True);
  ini.Free;
  if Portable then
    IniPath := Application.Location
  else
    IniPath := GetAppConfigDir(False);
  LoadIni;
  PGPageControl.TabIndex := 0;
  i := 0;
  while True do
  begin
    s := ini.ReadString('data', 'data' + IntToStr(i), '');
    if s = '' then
      break;
    BackupDatabasesList.Items.Add(s);
    Inc(i);
  end;
  ini.Free;
  if BackupDatabasesList.Items.Count > 0 then
    BackupDatabasesList.ItemIndex := 0;
  Databases := TStringList.Create;
end;

destructor TMainForm.Destroy;
var
  i: Integer;
  ini: TIniFile;
begin
  PoolThread.Clear;
  if ConsoleThread <> nil then
  begin
    ConsoleThread.Terminate;
    ConsoleThread.WaitFor;
    ConsoleThread.Free;
  end;
  ClosePG;
  FreeAndNil(Databases);

  ini := TIniFile.Create(IniPath + 'pgtools.ini');
  ini.WriteBool('options', 'CSProducts', CSProductsChk.Checked);
  ini.WriteString('options', 'username', UserNameEdit.Text);
  ini.WriteBool('options', 'savepassword', SavePasswordChk.Checked);
  if SavePasswordChk.Checked then
    ini.WriteString('options', 'password', PasswordEdit.Text);
  ini.WriteString('options', 'PGDirecotry', PGDirectoryEdit.Text);
  ini.WriteString('options', 'port', PortEdit.Text);
  ini.WriteString('options', 'directory', DirectoryEdit.Text);
  ini.WriteBool('options', 'expert', ExportTab.TabVisible);
  ini.WriteBool('options', 'portable', Portable);
  ini.WriteBool('options', 'DBDirectory', DBDirectoryChk.Checked);
  ini.WriteBool('options', 'PublicSchema', PublicSchemaChk.Checked);
  ini.EraseSection('data');
  for i := 0 to BackupDatabasesList.Items.Count - 1 do
    ini.WriteString('data', 'data' + IntToStr(i), BackupDatabasesList.Items[i]);
  ini.Free;
  FreeAndNil(PoolThread);
  inherited;
end;

end.
