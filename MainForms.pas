unit MainForms;

{**
*  This file is part of the "Creative Solutions PGTools http://www.cserp.org/"
 *
 * @license   mit(https://opensource.org/licenses/MIT)
 *
 * @author    Zaher Dirkey 
 *
 *  TODO: add clean with : --clean --if-exists
 *}

{$ifdef FPC}
{$mode delphi}
{$endif}
{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls, StrUtils,
  ComCtrls, Menus, IniFiles, SynEdit, SynHighlighterAny,
  LCLTranslator, PGToolsUtils,
  mncPostgre, mnMsgBox, GUIMsgBox, process,
  ConsoleProcess, FileUtil, mnUtils, LazFileUtils;

type

  { TMyPGTool }

  TMyPGTool = class(TPGTool)
  protected
    procedure Log(S: String; Kind: TmnLogKind = lgLog); override;
  public
  end;

  { TMainForm }

  TMainForm = class(TForm)
    BackFolderLbl: TLabel;
    BackupAllBtn: TButton;
    BackupBtn1: TButton;
    InfoBtn: TButton;
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
    RestoreFromFileBtn: TButton;
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
    procedure InfoBtnClick(Sender: TObject);
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
    procedure RestoreFromFileBtnClick(Sender: TObject);
    procedure RestorePointBtnClick(Sender: TObject);
    procedure SavePointBtnClick(Sender: TObject);
    procedure ScrollMnuClick(Sender: TObject);
    procedure StatusTimerTimer(Sender: TObject);
    procedure StopBtnClick(Sender: TObject);
  private
    procedure CheckLanguage;
    procedure ExploreFolder(AFolder, FileName: string);
    function GetPort: String;
    procedure Log(S: String; Kind: TmnLogKind = lgLog);
  protected
    PGObject: TMyPGTool;
    Databases: TStringList;
    procedure SetInfo;
    procedure BringInfo;
    function GetRestoreOptions: TRestoreOptions;
    function GetRestoreFileOptions: TRestoreOptions;
    procedure LoadIni;
    procedure SaveIni;
    procedure DetectPGPath;
    procedure DetectPortable;
  public
    IniPath: String;
    ExportMode: Boolean;
    Portable: Boolean;
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

function RO(B: Boolean; O: TRestoreOption): TRestoreOptions;
begin
  if B then
    Result := [O]
  else
    Result := [];
end;

{ TMyPGTool }

procedure TMyPGTool.Log(S: String; Kind: TmnLogKind);
begin
  MainForm.Log(S, Kind);
end;

{ TMainForm }

procedure TMainForm.DropAllTempsBtnClick(Sender: TObject);
begin
  SetInfo;
  PGObject.DropAllTemps(GetRestoreOptions);
  Databases.Clear;
end;

procedure TMainForm.BackupAllBtnClick(Sender: TObject);
var
  i: Integer;
begin
  for i := 0 to BackupDatabasesList.Items.Count - 1 do
  begin
    SetInfo;
    PGObject.BackupDatabase(BackupDatabasesList.Items[i], RO(CSProductsChk.Checked, roCreativeSolutions));
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
begin
  if BackupDatabasesList.ItemIndex >= 0 then
  begin
    SetInfo;
    PGObject.ChangePassword(NewPasswordEdit.Text)
  end;
end;

procedure TMainForm.CopyBtnClick(Sender: TObject);
var
  DB, ToName: string;
begin
  if DatabasesCbo.ItemIndex >= 0 then
  with PGObject do
  begin
    DB := DatabasesCbo.Items[DatabasesCbo.ItemIndex];
    ToName := DB;
    if MsgBox.Input(ToName, 'You want copy: ' + DB + ' to?') then
    begin
      SetInfo;
      if DB = ToName then
        MsgBox.Show('you cant copy on same database')
      else
      begin
        MsgBox.ShowStatus(Self, 'Coping ' + DB + ' to ' + ToName);
        try
          PGObject.CopyDatabase(DB, ToName);
          DatabasesCbo.Items[DatabasesCbo.ItemIndex] := ToName;
        finally
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
  begin
    SetInfo;
    PGObject.BackupDatabase(BackupDatabasesList.Items[BackupDatabasesList.ItemIndex], GetRestoreOptions);
  end;
end;

procedure TMainForm.InfoBtnClick(Sender: TObject);
begin
  Log('PQLib Version: ' + IntToStr(PQlibVersion()));
  if BackupDatabasesList.ItemIndex >= 0 then
  begin
    SetInfo;
    PGObject.Info;
  end;
end;

procedure TMainForm.ListDatabasesBtnClick(Sender: TObject);
begin
  SetInfo;
  PGObject.EnumDatabases(Databases, GetRestoreOptions, GetKeyShiftState = [ssCtrl]);
  DatabasesCbo.Items.Assign(Databases);
  if DatabasesCbo.Items.Count > 0 then
    DatabasesCbo.ItemIndex := 0;
  Databases.Clear;
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
      SetInfo;
      MsgBox.ShowStatus(Self, 'Dropping ' + DB);
      try
        PGObject.DropDatabase(DB);
        DatabasesCbo.Items.Delete(DatabasesCbo.ItemIndex);
      finally
        MsgBox.HideStatus(Self);
      end;
    end;
  end;
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
    SetInfo;
    aDatabase := BackupDatabasesList.Items[BackupDatabasesList.ItemIndex];
    aDirectory := PGObject.GetBackupDBDirectory(aDatabase);
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
        SetInfo;
        MsgBox.ShowStatus(Self, 'Renaming ' + DB + ' to ' + ToName);
        try
          PGObject.RenameDatabase(DB, ToName);
        finally
          MsgBox.HideStatus(Self);
        end;
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
    begin
      SetInfo;
      PGObject.RestoreDatabaseFile(DB, '', [roDirectDatabase] + GetRestoreOptions);
    end;
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
    begin
      SetInfo;
      PGObject.RestoreDatabase(db, GetRestoreOptions);
    end;
  end;
end;

procedure TMainForm.RestoreNewFromFileBtnClick(Sender: TObject);
var
  DB: String;
begin
  DB := ExtractFileNameWithoutExt(ExtractFileName(BackupFileNameEdit.Text));
  if MsgBox.Input(DB, 'Enter then name of new Database to restore') then
  begin
    SetInfo;
    PGObject.RestoreDatabaseFile(DB, BackupFileNameEdit.Text, [roDirectDatabase] + GetRestoreFileOptions);
  end;
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

function StringListAnsiCompare(List: TStringList; Index1, Index: Integer): Integer;
begin
  Result := CompareText(List[Index], List[Index1]);
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
      SetInfo;
      DB := BackupDatabasesList.Items[BackupDatabasesList.ItemIndex];
      Dir := ExpandToPath(PGObject.GetBackupDBDirectory(DB), Application.Location);
      i := -1;
      EnumFiles(files, Dir, DB + '.*.backup', [efFile]);
      files.CustomSort(@StringListAnsiCompare);
      for fname in files do
      begin
        s := SubStr(fname, '.', 1);
        //yyyymmddhhnnss
        s := MidStr(s, 1, 4) + '-' + MidStr(s, 5, 2) + '-' + MidStr(s, 7, 2) + ' ' + MidStr(s, 9, 2) + ':' + MidStr(s, 11, 4) + ':' + MidStr(s, 13, 4);
        names.Add(s);
      end;

      files.Add('');
      names.Add('Last Backup');

      if MsgBox.List(i, 'Select a point to restore', names) then
      begin
        SetInfo;
        PGObject.RestoreDatabaseFile(DB, Dir + files[i], [roRestoreOverwrite]);
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
  begin
    SetInfo;
    for i := 0 to BackupDatabasesList.Items.Count - 1 do
    begin
      PGObject.RestoreDatabase(BackupDatabasesList.Items[i], GetRestoreOptions);
    end;
  end;
end;

procedure TMainForm.RestoreFromFileBtnClick(Sender: TObject);
var
  DB, Selected: String;
begin
  SetInfo;
  with TOpenDialog.Create(Self) do
  begin
    Filter := '*.backup; *.sql';
    FileName := '*.backup; *.sql';
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
          PGObject.RestoreDatabaseFile(BackupDatabasesList.Items[BackupDatabasesList.ItemIndex], FileName, [roRestoreOverwrite]);
      end
      else
        if MsgBox.Input(DB, 'Enter then name of new Database to restore') then
          PGObject.RestoreDatabaseFile(DB, FileName, []);
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
  SetInfo;
  if BackupDatabasesList.ItemIndex >= 0 then
  begin
    SetInfo;
    files := TStringList.Create;
    try
      DB := BackupDatabasesList.Items[BackupDatabasesList.ItemIndex];
      i := -1;
      EnumFiles(files, PGObject.GetBackupDBDirectory(DB) + 'points', '*.backup', false);
      if MsgBox.List(i, 'Select a point to restore', files) then
      begin
        PGObject.RestoreDatabase(DB, GetRestoreOptions, files[i]);
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
    SetInfo;
    APoint := '';
    if MsgBox.Input(APoint, 'Enter then name of new Database to restore') and (APoint <> '') then
      PGObject.BackupDatabase(BackupDatabasesList.Items[BackupDatabasesList.ItemIndex], GetRestoreOptions, APoint);
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
  PGObject.Stop;
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

procedure TMainForm.SetInfo;
begin
  PGObject.UserName := UserNameEdit.Text;
  PGObject.Password := PasswordEdit.Text;
  PGObject.Port := GetPort;

  if BackupDatabasesList.ItemIndex >= 0 then
    PGObject.Database := BackupDatabasesList.Items[BackupDatabasesList.ItemIndex]
  else
    PGObject.Database := '';

  if DirectoryEdit.Text <> '' then
    PGObject.BackupDirectory := ExpandToPath(IncludeTrailingPathDelimiter(DirectoryEdit.Text), Application.Location)
  else
    PGObject.BackupDirectory := Application.Location;

end;

procedure TMainForm.BringInfo;
var
  db: string;
  ini: TIniFile;
begin
  if BackupDatabasesList.ItemIndex >= 0 then
  begin
      db := BackupDatabasesList.Items[BackupDatabasesList.ItemIndex];
      ini := TIniFile.Create(PGObject.GetBackupDBDirectory(db) + db+ '.ini');
      try
         BackupDeviceIDLbl.Caption := ini.ReadString('info', 'id', '');
      finally
        ini.Free;
      end;
  end
  else
    BackupDeviceIDLbl.Caption := '';
end;

function TMainForm.GetRestoreOptions: TRestoreOptions;
begin
  Result :=
    RO(RestoreFileOverwriteChk.Checked, roRestoreOverwrite) +
    RO(RestoreFileIgnoreErrorChk.Checked, roIgnoreError) +
    RO(CreateMissedMetaDataChk.Checked, roCreateMissedMetaData) +
    RO(DropPublicSchemaChk.Checked, roDropPublicSchema) +
    RO(RestoreDataOnlyChk.Checked, roRestroreDataOnly) +
    RO(PublicSchemaChk.Checked, roBackupPublicSchemaOnly);
end;

function TMainForm.GetRestoreFileOptions: TRestoreOptions;
begin
  Result :=
    RO(RestoreFileOverwriteChk.Checked, roRestoreOverwrite) +
    RO(RestoreFileIgnoreErrorChk.Checked, roIgnoreError) +
    RO(CreateMissedMetaDataChk.Checked, roCreateMissedMetaData) +
    RO(DropPublicSchemaChk.Checked, roDropPublicSchema) +
    RO(RestoreDataOnlyChk.Checked, roRestroreDataOnly) +
    RO(PublicSchemaChk.Checked, roBackupPublicSchemaOnly);
    RO(RestorePublicSchemaOnlyChk.Checked, roRestorePublicSchemaOnly);
end;

procedure TMainForm.LoadIni;
var
  ini: TIniFile;
begin
  ini := TIniFile.Create(IniPath + 'pgtools.ini');
  try
    LangListCbo.Text := ini.ReadString('options', 'Language', 'English');
    CheckLanguage;
    SavePasswordChk.Checked := ini.ReadBool('options', 'savepassword', False);
    if SavePasswordChk.Checked then
    begin
      UserNameEdit.Text := ini.ReadString('options', 'username', ''); //postgres
      PasswordEdit.Text := ini.ReadString('options', 'password', '')
    end
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
      RestoreFromFileBtn.Visible := True;
      RestoreByDateBtn.Visible := True;
    end;
    //PublicSchemaChk.Checked := ini.ReadBool('options', 'PublicSchema', False);
    RestoreFileOverwriteChk.Checked := ini.ReadBool('options', 'RestoreFileOverwriteChk', False);
    PGObject.PGPath := ExpandToPath(ini.ReadString('options', 'lib', PGObject.PGPath), Application.Location);
  finally
    ini.Free;
  end;
end;

procedure TMainForm.SaveIni;
var
  i: Integer;
  ini: TIniFile;
begin
  ini := TIniFile.Create(IniPath + 'pgtools.ini');
  try
    ini.WriteInteger('options', 'version', 2);
    ini.WriteString('options', 'Language', LangListCbo.Text);
    ini.WriteBool('options', 'savepassword', SavePasswordChk.Checked);
    if SavePasswordChk.Checked then
    begin
      ini.WriteString('options', 'username', UserNameEdit.Text);
      ini.WriteString('options', 'password', PasswordEdit.Text);
    end;
    ini.WriteString('options', 'port', PortEdit.Text);
    ini.WriteString('options', 'directory', DirectoryEdit.Text);
    ini.WriteBool('options', 'expert', ExportTab.TabVisible);
    ini.WriteBool('options', 'portable', Portable);
    //ini.WriteBool('options', 'PublicSchema', PublicSchemaChk.Checked); //do not save it, it is for special
    ini.WriteBool('options', 'RestoreFileOverwrite', RestoreFileOverwriteChk.Checked);
    ini.EraseSection('data');
    for i := 0 to BackupDatabasesList.Items.Count - 1 do
      ini.WriteString('data', 'data' + IntToStr(i), BackupDatabasesList.Items[i]);
  finally
    ini.Free;
  end;
end;

procedure TMainForm.DetectPGPath;
  function Check(f: string): Boolean;
  begin
    Result := FileExists(f);
    if Result then
      PGObject.PGPath := ExtractFilePath(f);
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

procedure TMainForm.DetectPortable;
var
  ini: TIniFile;
begin
  ini := TIniFile.Create(Application.Location + 'pgtools.ini');
  try
    Portable := ini.ReadBool('options', 'portable', True);
  finally
    ini.Free;
  end;
  if Portable then
    IniPath := Application.Location
  else
    IniPath := GetAppConfigDir(False);
end;

constructor TMainForm.Create(TheOwner: TComponent);
var
  i: Integer;
  ini: TIniFile;
  s: String;
  aStrings: TStringList;
begin
  inherited;
  PGObject := TMyPGTool.Create;
  UserNameEdit.Text := '';
  DetectPortable;
  DetectPGPath;
  Log('This Device: ' + GetLocalName);

  LoadIni;

  RestoreFilePageControl.TabIndex := 0;
  i := 0;

  aStrings := TStringList.Create;
  ini := TIniFile.Create(IniPath + 'pgtools.ini');
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
  if PGObject.PGPath <> '' then
    SetCurrentDir(PGObject.PGPath);
end;

destructor TMainForm.Destroy;
begin
  PGObject.Shutdown;
  SaveIni;
  FreeAndNil(Databases);
  FreeAndNil(PGObject);
  inherited;
end;

initialization
end.
