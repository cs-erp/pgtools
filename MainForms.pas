unit MainForms;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  IniFiles, registry,
  SynEdit, mncPostgre, ConsoleProcess, process;

type

  { TMainForm }

  TMainForm = class(TForm)
    CleanBtn: TButton;
    CleanBtn1: TButton;
    CleanBtn2: TButton;
    InfoPanel: TPanel;
    UserNameEdit: TEdit;
    PasswordEdit: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    LogEdit: TSynEdit;
    procedure CleanBtn1Click(Sender: TObject);
    procedure CleanBtnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    ConsoleThread: TmnConsoleThread;
    procedure Log(S: String);
    procedure ConsoleTerminated(Sender: TObject);
  protected
    PGConn: TmncPGConnection;
    PGSession: TmncPGSession;
    Databases: TStringList;
    PGPathBin: String;
    procedure EnumDatabases(vOld: Boolean);
    procedure OpenPG;
    procedure ClosePG;
    procedure CreateConsole(vExecutable, vParameters, vPassword: String);
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
  OpenPG;
  EnumDatabases(True);
  for i := 0 to Databases.Count - 1 do
  begin
    InfoPanel.Caption := 'Dropping database ' + Databases[i];
    PGConn.Execute('drop database "' + Databases[i] + '"');
    //if PGConn.Execute('drop database "' + Databases[i]+'"') then
    Log('Database Dropped: "' + Databases[i] + '"');
    //else
    //  Log('Database Dropped: "' + Databases[i]+'"');
    LogEdit.CaretY := LogEdit.Lines.Count - 1;
    Application.ProcessMessages;
  end;
  ClosePG;
  Log('Clean Done');
end;

procedure TMainForm.CleanBtn1Click(Sender: TObject);
var
  DB, FileName, cmd: String;
begin
  //C:\Program Files (x86)\PostgreSQL\9.5\bin\pg_restore.exe --host localhost --port 5432 --username "postgres" --dbname "isko2017" --no-password  --verbose "D:\backups\isko\isko2017.backup"
  DB := 'test';
  FileName := DB + '.backup';
  cmd := '';
  cmd := cmd + ' -v --host localhost --port 5432 --password --username "' + UserNameEdit.Text + '"';
  cmd := cmd + ' --format custom --compress=9 --blobs --section pre-data --section data --file "' + Application.Location + FileName + '" "' + DB + '"';
  //RunCommand(cmd, output);
  Log(cmd);
  //"SET PGPASSWORD=<password>"
  //CreateConsole('pg_dump.exe', cmd, 'SET PGPASSWORD=' + PasswordEdit.Text);
  CreateConsole('pg_dump.exe', cmd, PasswordEdit.Text);
  //CreateConsole('cmd.exe', ' /c echo "zaza"', 'SET PGPASSWORD=' + PasswordEdit.Text);
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin

end;

procedure TMainForm.Log(S: String);
begin
  LogEdit.Lines.Add(S);
  LogEdit.CaretY := LogEdit.Lines.Count;
end;

procedure TMainForm.ConsoleTerminated(Sender: TObject);
begin
  if ConsoleThread.Status = 0 then
    Log('Database is dump')
  else
    Log('Dump error');
  FreeAndNil(ConsoleThread);
end;

procedure TMainForm.EnumDatabases(vOld: Boolean);
var
  cmd: TmncPGCommand;
begin
  Databases.Clear;
  cmd := PGSession.CreateCommand as TmncPGCommand;
  try
    cmd.SQL.Text := 'SELECT datname as name FROM pg_database';
    cmd.SQL.Add('WHERE datistemplate = false');
    if vOld then
    begin
      cmd.SQL.Add('and (datname like ''%_old%''');
      cmd.SQL.Add('or datname like ''%.old%'')');
    end;
    if cmd.Execute then
    begin
      while not cmd.Done do
      begin
        Databases.Add(cmd.Field['name'].AsString);
        Log(cmd.Field['name'].AsString);
        cmd.Next;
      end;
    end;
  finally
    cmd.Free;
  end;
end;

procedure TMainForm.OpenPG;
begin
  if PGConn = nil then
    PGConn := TmncPGConnection.Create;
  PGConn.UserName := UserNameEdit.Text;
  PGConn.Password := PasswordEdit.Text;
  PGConn.Resource := 'postgres';
  PGConn.Connect;
  PGSession := PGConn.CreateSession as TmncPGSession;
end;

procedure TMainForm.ClosePG;
begin
  FreeAndNil(PGSession);
  FreeAndNil(PGConn);
end;

procedure TMainForm.CreateConsole(vExecutable, vParameters, vPassword: String);
begin
  if ConsoleThread = nil then
  begin
    ConsoleThread := TmnConsoleThread.Create(vExecutable, vParameters, @Log);
    ConsoleThread.OnTerminate := @ConsoleTerminated;
    ConsoleThread.Password := vPassword;
    ConsoleThread.Start;
  end;
end;

constructor TMainForm.Create(TheOwner: TComponent);
var
  reg: TRegistry;
  ini: TIniFile;
begin
  inherited Create(TheOwner);
  reg := TRegistry.Create(KEY_READ);
  reg.RootKey := HKEY_LOCAL_MACHINE;
  //if reg.OpenKey('SOFTWARE\PostgreSQL\', False)
  reg.Free;

  ini := TIniFile.Create(Application.Location + 'pgtools.ini');
  UserNameEdit.Text := ini.ReadString('options', 'username', 'postgres');
  PasswordEdit.Text := ini.ReadString('options', 'password', '');
  ini.Free;

  Databases := TStringList.Create;
end;

destructor TMainForm.Destroy;
var
  ini: TIniFile;
begin
  if ConsoleThread <> nil then
  begin
    ConsoleThread.Terminate;
    ConsoleThread.WaitFor;
    ConsoleThread.Free;
  end;
  ClosePG;
  FreeAndNil(Databases);

  ini := TIniFile.Create(Application.Location + 'pgtools.ini');
  ini.WriteString('options', 'username', UserNameEdit.Text);
  ini.WriteString('options', 'password', PasswordEdit.Text);
  ini.Free;
  inherited;
end;

end.

