unit ConsoleProcess;
{-----------------------------------------------------------------------------
  Author:   Zaher Dirkey
  Purpose:
  License:   mit(https://opensource.org/licenses/MIT)
-----------------------------------------------------------------------------}
{$mode objfpc}{$H+}

interface

uses
  Forms, Classes, SysUtils, process,
  mnUtils, mnStreams;

type
  TmnConsoleThread = class;

  { TExecuteObject }

  TExecuteObject =class(TObject)
  public
    procedure Prepare(const ConsoleThread: TmnConsoleThread); virtual; abstract;
    procedure Execute(const ConsoleThread: TmnConsoleThread); virtual; abstract;
    procedure Unprepare(const ConsoleThread: TmnConsoleThread); virtual;
  end;

  TmnLogKind = (lgLog, lgStatus, lgDone, lgMessage);

  TmnOnLog = procedure(S: String; Kind: TmnLogKind) of object;

  { TmnConsoleThread }

  TmnConsoleThread = class(TThread)
  private
    FEnvironment: TStrings;
    FExecutable: string;
    FCurrentDirectory: string;
    FParameters: string;
    FExecuteObject: TExecuteObject;
    FOnLog: TmnOnLog;
    FPassword: string;
    FProcess: TProcess;
    procedure SetExecuteObject(AValue: TExecuteObject);
  protected
    StreamWrapper: TmnWrapperStream;
    FString: String;
    FKind: TmnLogKind;
    procedure DoOnLog; virtual; //To Sync
  public
    Status: Integer;
    IgnoreError: Boolean;
    Message: string;
    constructor Create(vExecutable, vCurrentDirectory, vParameters: string; vOnLog: TmnOnLog = nil);
    destructor Destroy; override;
    procedure Kill;
    procedure Execute; override;
    procedure ReadPrompt; virtual;
    procedure ReadStream; virtual;
    procedure Log(S: String; Kind: TmnLogKind = lgLog);
    property OnLog: TmnOnLog read FOnLog write FOnLog;
    property Password: string read FPassword write FPassword;
    property Environment: TStrings read FEnvironment;
    property ExecuteObject: TExecuteObject read FExecuteObject write SetExecuteObject;
  end;

implementation

{ TExecuteObject }

procedure TExecuteObject.Unprepare(const ConsoleThread: TmnConsoleThread);
begin
end;

{ TmnConsoleThread }

procedure TmnConsoleThread.SetExecuteObject(AValue: TExecuteObject);
begin
  if FExecuteObject <> AValue then
    FExecuteObject :=AValue;
end;

procedure TmnConsoleThread.DoOnLog;
begin
  if Assigned(FOnLog) then
    FOnLog(FString, FKind);
end;

procedure TmnConsoleThread.Log(S: String; Kind: TmnLogKind);
begin
  FString := S;
  FKind := Kind;
  Synchronize(@DoOnLog);
  FString := '';
end;

constructor TmnConsoleThread.Create(vExecutable, vCurrentDirectory, vParameters: string; vOnLog: TmnOnLog);
begin
  inherited Create(True);
  FreeOnTerminate := True;
  FOnLog := vOnLog;
  FEnvironment := TStringList.Create();
  FExecutable:= vExecutable;
  FCurrentDirectory := vCurrentDirectory;
  FParameters := vParameters;
end;

destructor TmnConsoleThread.Destroy;
begin
  FreeAndNil(FExecuteObject);
  FreeAndNil(FEnvironment);
  inherited Destroy;
end;

procedure TmnConsoleThread.Kill;
begin
  if FProcess <> nil then
  begin
    //FProcess. //send sginal ctrl+c
    FProcess.Terminate(1);
  end;
end;

{procedure TmnConsoleThread.Read;
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
        Log(T);
    end;
  end;
end;}

procedure TmnConsoleThread.ReadPrompt;
var
  T: String;
  aBuffer: array[0..79] of AnsiChar;
  C: DWORD;
begin
  aBuffer := '';
  C := FProcess.Output.Read(aBuffer, SizeOf(aBuffer));
  if C <> 0 then
  begin
    SetString(T, aBuffer, C);
    Log(T);
  end;
end;

procedure TmnConsoleThread.ReadStream;
var
  S: utf8string;
  b: Boolean;
begin
  if FProcess.Output <> nil then
  begin
    try
      while not Terminated do
      begin
        b := StreamWrapper.ReadUTF8Line(S, False);
        if not b and not (FProcess.Running) then
          break;
        Log(S);
      end;
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
  procedure StreamWriteLn(S: string);
  var
    i : integer;
  begin
    for i := 1 to Length(S) do
      FProcess.Input.WriteByte(Byte(S[i]));
    FProcess.Input.WriteByte(13);
    FProcess.Input.WriteByte(10);
  end;
var
  i: Integer;
begin
  FProcess := TProcess.Create(nil);
  FProcess.CurrentDirectory := FCurrentDirectory;
  FProcess.Executable := FExecutable;
  CommandToList(FParameters, FProcess.Parameters);
  FProcess.Options := [poUsePipes, poStderrToOutPut, poNoConsole{, poDetached}]; //do not use poDetached with pg_ctl
  FProcess.ShowWindow := swoHide;
  FProcess.ConsoleTitle := 'Console';
  FProcess.InheritHandles := True;
  FProcess.CurrentDirectory := Application.Location;
  FProcess.StartupOptions := [suoUseShowWindow]; //<- need it in linux to hide window
  if (Environment.Count > 0) then //if there is one, we need to copy system environments
  begin
    for i := 0 to GetEnvironmentVariableCount -1 do
       FProcess.Environment.Add(GetEnvironmentString(i));
    FProcess.Environment.AddStrings(Environment);
    // Examples
    //ConsoleThread.Environment.Add('PGPASSWORD=sss');
    //ConsoleThread.Environment.Add('LANG=ru_RU.UTF-8');
    //ConsoleThread.Environment.Add('PYTHONIOENCODING=utf-8');
  end;

  Status := 0;
  try
    try
      if FExecuteObject <> nil then
        FExecuteObject.Prepare(Self);
      Log(Message, lgStatus);
      Log(FProcess.Executable + ' ' + StringReplace(FProcess.Parameters.Text, #13#10, ' ', [rfReplaceAll]));
      FProcess.Execute;
      StreamWrapper := TmnWrapperStream.Create(FProcess.Output, False);
      StreamWrapper.EndOfLine := #10;
      ReadPrompt;
      if FProcess.Running and (FProcess.Input <> nil) and (Password <> '') then
      begin
        StreamWriteLn(Password);
//        StreamWriteLn(Password);
      end;
      FProcess.CloseInput;
      if FProcess.Running then
        ReadStream;
      Status := FProcess.ExitStatus;
      FreeAndNil(FProcess);
      if (IgnoreError or (Status = 0)) and (FExecuteObject <> nil) then
        FExecuteObject.Execute(Self);

    except
      on E: Exception do
      begin
        Log(E.Message, lgMessage);
        raise;
      end;
    end;
  finally
    //Log('Execute Time: "' + TicksToString(GetTickCount64 - d) + '" with Status: ' + IntToStr(Status) );
    Log(Message + ' return: ' + IntToStr(Status) );
  end;

  if FProcess <> nil then
  begin
    FProcess.WaitOnExit;
    FreeAndNil(StreamWrapper);
    FreeAndNil(FProcess);
  end;
end;

end.
