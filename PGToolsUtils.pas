unit PGToolsUtils;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms,
  mncPostgre, mnMsgBox, GUIMsgBox, process,
  Contnrs, IniFiles,
  ConsoleProcess, FileUtil, mnUtils, LazFileUtils;

type
  TRestoreOption = (
    roRestoreOverwrite,
    roDirectDatabase,
    roIgnoreError,
    roCreateMissedMetaData,
    roDropPublicSchema,
    roQuickBackup,
    roClean,
    roBackupPublicSchemaOnly,
    roRestorePublicSchemaOnly,
    roRestroreDataOnly,
    roCreativeSolutions
  );

  TRestoreOptions = set of TRestoreOption;

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

  { TPGToolClass }

  TPGTool = class(TObject)
  protected
    procedure OpenPG(vDatabase: String = 'postgres'; StartSession: Boolean = True);
    procedure ClosePG(StopSession: Boolean = True);
    procedure Launch(vMessage, vExecutable, vParameters, vPassword: String; vExecuteObject: TExecuteObject = nil; IgnoreError: Boolean = False);
    procedure Resume;

    procedure Log(S: String; Kind: TmnLogKind = lgLog); virtual;
  public
    ConnectCount: Integer;
    PoolThread: TObjectList;
    PGPathBin: String;
    FDestroying: Boolean;
    FStop: Boolean;
    PGConn: TmncPGConnection;
    PGSession: TmncPGSession;
    PGDirectory: String;//detected when load
    Database: string;
    BackupDirectory: string;
    ConsoleThread: TmnConsoleThread;

    UserName, Password, Port: string;
    constructor Create;
    destructor Destroy; override;
    procedure Stop;
    procedure Shutdown;
    function GetBackupDBDirectory(DB: string): string;
    procedure ConsoleTerminated(Sender: TObject);

    procedure DropDatabase(ADatabase: String);
    procedure RenameDatabase(ADatabase, AToName: String);
    procedure CopyDatabase(ADatabase, AToName: String);
    procedure ChangePassword(APassword: string);

    procedure BackupDatabase(DB: String; Options: TRestoreOptions; APointName: string = '');
    procedure RestoreDatabase(DB: String; Options: TRestoreOptions; APointName: String = '');
    procedure RestoreDatabaseFile(DB: String; AFileName: String = ''; Options: TRestoreOptions = [roRestoreOverwrite]);
    procedure DropAllTemps(Options: TRestoreOptions);

    procedure Info;
    procedure EnumDatabases(Databases: TStringList; Options: TRestoreOptions; vOld: Boolean);
  end;

function GetLocalName: string;

implementation

resourcestring
  sCleanDone = 'Clean Done';
  sExcludeTables = '"*.\"AppFiles\""';

function GetLocalName: string;
begin
  Result := Application.EnvironmentVariable['USERNAME']+'_'+Application.EnvironmentVariable['COMPUTERNAME'];
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
        if not (roRestoreOverwrite in Options) then
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
          if roRestoreOverwrite in Options then
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
{ TPGTool }

constructor TPGTool.Create;
begin
  inherited Create;
  PoolThread := TObjectList.Create;
end;

destructor TPGTool.Destroy;
begin
  FreeAndNil(PoolThread);
  inherited Destroy;
end;

procedure TPGTool.Stop;
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

procedure TPGTool.Shutdown;
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
end;

function TPGTool.GetBackupDBDirectory(DB: string): string;
begin
  Result := BackupDirectory;
  Result := IncludeTrailingPathDelimiter(Result + DB);
end;

procedure TPGTool.Log(S: String; Kind: TmnLogKind);
begin

end;

procedure TPGTool.ConsoleTerminated(Sender: TObject);
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

procedure TPGTool.BackupDatabase(DB: String; Options: TRestoreOptions; APointName: string);
var
  cmd: String;
  o: TBackupExecuteObject;
begin
  //o.Overwrite := Overwrite;
  //"SET PGPASSWORD=<password>"
  o := TBackupExecuteObject.Create;
  o.UserName := UserName;
  o.Password := Password;
  o.Port := Port;
  o.Options := Options;
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
  cmd := cmd + ' -v --host localhost --port ' + Port + ' --password --username "' + o.UserName + '"';
  if roBackupPublicSchemaOnly in Options then
    cmd := cmd + ' --schema=public'
  else
    cmd := cmd + ' --blobs';
  if roQuickBackup in Options then
    cmd := cmd + ' -T ' + sExcludeTables;
  cmd := cmd + ' --format custom --compress=9 --file "' + o.FileName + '" "' + DB + '"';

  //cmd := cmd + ' --format tar --blobs --file "' + filename + '" "' + DB + '"';
  Launch('Backuping: ' + DB + ' file: ' + o.FileName, 'pg_dump.exe', cmd, Password, o);
end;

procedure TPGTool.RestoreDatabase(DB: String; Options: TRestoreOptions; APointName: String);
var
  o: TRestoreExecuteObject;
  cmd: String;
  ini: TIniFile;
  filename: string;
  aDatabase: string;
begin
  o := TRestoreExecuteObject.Create;
  o.UserName := UserName;
  o.Password := Password;
  o.Port := Port;
  o.Database := DB;
  o.Directory := GetBackupDBDirectory(o.Database);
  o.Options := [roRestoreOverwrite] + Options;
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
  cmd := '--host localhost --port ' + Port + ' --username "' + o.UserName + '" --dbname "' + aDatabase + '" --password --verbose "' + filename + '"';
  Launch('Restoring: ' + DB + ' file: ' + filename, 'pg_restore.exe', cmd, Password, o);
end;

procedure TPGTool.RestoreDatabaseFile(DB: String; AFileName: String; Options: TRestoreOptions);
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
  o.UserName := UserName;
  o.Password := Password;
  o.Port := Port;
  o.Database := DB;
  o.Directory := GetBackupDBDirectory(o.Database);
  o.Options := Options;

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

  if roClean in Options then
    AddParam('--clean --if-exists');

  if roRestorePublicSchemaOnly in Options then
    AddParam('--schema=public');

  if roRestroreDataOnly in Options then
    AddParam('--data-only');

  if roDirectDatabase in Options then
    aDatabase := DB
  else
    aDatabase := DB + '_temp_' + o.Suffix;
  cmd := '--host localhost --port ' + Port + ' --username "' + o.UserName + '" ' + aParams + ' --dbname "' + aDatabase + '" --password --verbose "' + AFileName + '"';

  Launch('Restoring: ' + DB + ' file: ' + AFileName, 'pg_restore.exe', cmd, Password, o, roIgnoreError in Options);
end;

procedure TPGTool.DropAllTemps(Options: TRestoreOptions);
var
  i: Integer;
  Databases: TStringList;
begin
  OpenPG('postgres', False);
  Databases := TStringList.Create;
  try
    EnumDatabases(Databases, Options, True);
    for i := 0 to Databases.Count - 1 do
    begin
      Log('Dropping database ' + Databases[i], lgStatus);
      PGConn.Execute('drop database "' + Databases[i] + '"');
      //if PGConn.Execute('drop database "' + Databases[i]+'"') then
      Log('Database Dropped: "' + Databases[i] + '"', lgStatus);
      //else
      //  Log('Database Dropped: "' + Databases[i]+'"');
    end;
  finally
    Databases.Free;
    ClosePG(False);
  end;
  Log(sCleanDone, lgDone);
end;

procedure TPGTool.Info;
var
  cmd: TmncPGCommand;
begin
  OpenPG(Database);
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

procedure TPGTool.EnumDatabases(Databases: TStringList; Options: TRestoreOptions; vOld: Boolean);
var
  cmd: TmncPGCommand;
begin
  OpenPG('postgres');
  try
    Databases.Clear;
    cmd := PGSession.CreateCommand as TmncPGCommand;
    try
      cmd.SQL.Text := 'SELECT datname as name FROM pg_database';
      cmd.SQL.Add('WHERE datistemplate = false and datname <> ''postgres''');
      if roCreativeSolutions in Options then
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
  finally
    ClosePG;
  end;
end;


procedure TPGTool.OpenPG(vDatabase: String; StartSession: Boolean);
begin
  if ConnectCount = 0 then
  begin
    if PGConn <> nil then
      raise Exception.Create('Already connected check your code');
    PGConn := TmncPGConnection.Create;

    PGConn.UserName := UserName;
    PGConn.Password := Password;
    PGConn.Port := Port;
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
  Inc(ConnectCount);
end;

procedure TPGTool.ClosePG(StopSession: Boolean);
begin
  Dec(ConnectCount);
  if ConnectCount = 0 then
    if PGConn <> nil then
    begin
      if StopSession and (PGSession <> nil) then
        PGSession.Commit;
      FreeAndNil(PGSession);
      FreeAndNil(PGConn);
    end;
end;

procedure TPGTool.Launch(vMessage, vExecutable, vParameters, vPassword: String; vExecuteObject: TExecuteObject; IgnoreError: Boolean);
var
  aConsoleThread: TmnConsoleThread;
begin
  if PGDirectory <> '' then
    vExecutable := IncludeTrailingPathDelimiter(PGDirectory) + vExecutable;
  aConsoleThread := TmnConsoleThread.Create(vExecutable, BackupDirectory, vParameters, @Log);
  aConsoleThread.OnTerminate := @ConsoleTerminated;
  aConsoleThread.Password := vPassword;
  aConsoleThread.Message := vMessage;
  aConsoleThread.ExecuteObject := vExecuteObject;
  aConsoleThread.IgnoreError := IgnoreError;
  PoolThread.Add(aConsoleThread);
  Resume;
end;

procedure TPGTool.Resume;
begin
  if (PoolThread.Count > 0) then
  begin
    if (ConsoleThread = nil) then
    begin
      ConsoleThread := PoolThread.Extract(PoolThread.First) as TmnConsoleThread;
      //Log(ConsoleThread.Message);
      //InfoPanel.Caption := ConsoleThread.Message;
      Log(ConsoleThread.Message, lgStatus);
      Application.ProcessMessages;
      ConsoleThread.Start;
    end;
  end
  else
  begin
    Log('Finished', lgMessage);
    Log('', lgStatus);
  end;
end;

procedure TPGTool.DropDatabase(ADatabase: String);
begin
  Log('Dropping database ' + ADatabase, lgStatus);
  OpenPG('postgres', False);
  try
    PGConn.Execute('drop database "' + ADatabase + '"');
    Log('Database Dropped: "' + ADatabase + '"', lgStatus);
  finally
    ClosePG(False);
  end;
end;

procedure TPGTool.RenameDatabase(ADatabase, AToName: String);
begin
  Log('Renaming database ' + ADatabase, lgStatus);
  OpenPG('postgres', False);
  try
    PGConn.Execute('alter database "' + ADatabase + '" rename to "' + AToName + '"');
    Log('Database Renamed: "' + ADatabase + '"', lgStatus);
  finally
    ClosePG(False);
  end;
end;

procedure TPGTool.CopyDatabase(ADatabase, AToName: String);
begin
  Log('Copying database ' + ADatabase + ' to ' + AToName, lgStatus);
  OpenPG('postgres', False);
  try
    PGConn.Execute('CREATE DATABASE "' + AToName + '" TEMPLATE "' + ADatabase + '"');
    Log('Database copied: "' + ADatabase + ' to ' + AToName + '"', lgStatus);
  finally
    ClosePG(False);
  end;
end;

procedure TPGTool.ChangePassword(APassword: string);
var
  cmd: TmncPGCommand;
begin
  OpenPG('postgres');
  try
    cmd := PGSession.CreateCommand as TmncPGCommand;
    try
      cmd.SQL.Text := 'ALTER ROLE ' + UserName + ' WITH PASSWORD ''' + APassword + '''';
      cmd.Execute;
      Log('Password changed successfully', lgMessage);
    finally
      cmd.Free;
    end;
  finally
    ClosePG;
  end;
end;

end.

