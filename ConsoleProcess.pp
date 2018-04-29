unit ConsoleProcess;

{$mode objfpc}{$H+}

interface

uses
  Forms, Classes, SysUtils, process, pipes,
  mnStreams;

type
  TmnOnWriteString = procedure(S: String) of object;

  { TmnConsoleThread }

  TmnConsoleThread = class(TThread)
  private
    FOnWriteString: TmnOnWriteString;
    FPassword: string;
    FProcess: TProcess;
  protected
    Buffer: String;
    procedure DoOnWriteString; virtual; //To Sync
    procedure WriteString(S: String);
  public
    Status: Integer;
    constructor Create(vExecutable, vParameters: string; AOnWriteString: TmnOnWriteString = nil);
    destructor Destroy; override;
    procedure Execute; override;
    procedure Read; virtual;
    procedure ReadStream; virtual;
    property OnWriteString: TmnOnWriteString read FOnWriteString write FOnWriteString;
    property Password: string read FPassword write FPassword;
  end;

implementation

type
  THackThread = class(TThread)
  end;

{ TmnConsoleThread }

procedure TmnConsoleThread.DoOnWriteString;
begin
  if Assigned(FOnWriteString) then
    FOnWriteString(Buffer);
end;

procedure TmnConsoleThread.WriteString(S: String);
begin
  Buffer := S;
  Synchronize(@DoOnWriteString);
  Buffer := '';
end;

constructor TmnConsoleThread.Create(vExecutable, vParameters: string; AOnWriteString: TmnOnWriteString);
begin
  inherited Create(True);
  FOnWriteString := AOnWriteString;
  FProcess := TProcess.Create(nil);
  FProcess.Executable := vExecutable;
  CommandToList(vParameters, FProcess.Parameters);
  FProcess.Options := [poUsePipes, poStderrToOutPut, poNoConsole];
  FProcess.ShowWindow := swoHide;
  FProcess.ConsoleTitle := 'PG Console';
  FProcess.InheritHandles := True;
  FProcess.CurrentDirectory := Application.Location;
  FProcess.StartupOptions := [suoUseShowWindow]; //<- need it in linux to show window
end;

destructor TmnConsoleThread.Destroy;
begin
  if FProcess <> nil then
  begin
    FProcess.WaitOnExit;
    FreeAndNil(FProcess);
  end;
  inherited Destroy;
end;

procedure TmnConsoleThread.Read;
var
  T: String;
  aBuffer: array[0..79] of AnsiChar;

  function ReadNow(out C: DWORD): Boolean;
  begin
    if (FProcess.Output.NumBytesAvailable > 0) then
      C := FProcess.Output.Read(aBuffer, SizeOf(aBuffer))
    else
      C := 0;
    Result := C > 0;
  end;

var
  C: DWORD;
begin
  aBuffer := '';
  while FProcess.Running and not Terminated do
  begin
    if ReadNow(C) then
    begin
      SetString(T, aBuffer, C);
      if T <> '' then
        WriteString(T);
    end;
  end;
  //WriteString('------exit--------');
end;

procedure TmnConsoleThread.ReadStream;
var
  aWrapper: TmnWrapperStream;
  b: Boolean;
begin
  if FProcess.Output <> nil then
  begin
    try
      aWrapper := TmnWrapperStream.Create(FProcess.Output, False);
      aWrapper.EndOfLine := #13;

      while not Terminated do
      begin
        b := aWrapper.ReadLine(Buffer, False);
        WriteString(Buffer);
        if not (FProcess.Running) or not b then
          break;
      end;
    aWrapper.Free;
  except
    on e: Exception do
    begin
      if FProcess.Running and Terminated then
        FProcess.Terminate(0);
    end;
  end;
  end;
end;

procedure TmnConsoleThread.Execute;
begin
  FProcess.Execute;
  {if (FProcess.Input <> nil) and (Password <> '') then
    FProcess.Input.WriteAnsiString(Password + #13);}
  //FProcess.CloseInput;
  ReadStream;
  Status := FProcess.ExitStatus;
  FreeAndNil(FProcess);
end;

end.
