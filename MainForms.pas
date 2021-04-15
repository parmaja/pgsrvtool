unit MainForms;
{$mode objfpc}{$H+}
{-----------------------------------------------------------------------------
  Author:   Zaher Dirkey
  Purpose:
  License:   mit(https://opensource.org/licenses/MIT)
-----------------------------------------------------------------------------}
interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Menus,
  {$ifdef WINDOWS}
  Windows, //TODO, i hate include it
  {$endif}
  ImgList,
  mnUtils,
  ExtCtrls, ActnList, SynEdit, IniFiles, simpleipc,
  mnMsgBox, GUIMsgBox,
  ConsoleProcess;

const
  sApplicationID = 'PARMAJA.PGServer';

type

  { TMainForm }

  TMainForm = class(TForm)
    ApplicationProperties: TApplicationProperties;
    CheckAct: TAction;
    CloseBtn: TButton;
    ImageList: TImageList;
    IPCServer: TSimpleIPCServer;
    Panel1: TPanel;
    StartAct: TAction;
    StartBtn: TButton;
    StopAct: TAction;
    CloseAct: TAction;
    ActionList: TActionList;
    ClearLogMnu: TMenuItem;
    InfoPanel: TPanel;
    LogPopupMenu: TPopupMenu;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    StopBtn: TButton;
    CheckBtn1: TButton;
    TrayPopupMenu: TPopupMenu;
    ScrollMnu: TMenuItem;
    LogEdit: TSynEdit;
    StatusTimer: TTimer;
    TrayIcon: TTrayIcon;
    procedure ApplicationPropertiesEndSession(Sender: TObject);
    procedure CheckActExecute(Sender: TObject);
    procedure ClearLogMnuClick(Sender: TObject);
    procedure CloseActExecute(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormWindowStateChange(Sender: TObject);
    procedure IPCServerMessage(Sender: TObject);
    procedure IPCServerMessageQueued(Sender: TObject);
    procedure StartActExecute(Sender: TObject);
    procedure StopActExecute(Sender: TObject);
    procedure TrayIconClick(Sender: TObject);
    procedure TrayIconDblClick(Sender: TObject);
  private
    FDestroying: Boolean;
    UserName: string;
    Password: string;
    Port: string;
    PGPath: string;
    DataPath: string;
    StartMinimized: Boolean;
    AutoStart: Boolean;
    ShowTray: Boolean;
    procedure CheckServer;
    procedure ConsoleTerminated(Sender: TObject);
    function ExpandFile(FileName: string): string;
    procedure ShowApp;
    procedure HideApp;
    procedure ForceForegroundWindow;
    procedure Log(S: String; Kind: TmnLogKind = lgLog);
    procedure Launch(AddIt: Boolean; vMessage, vExecutable, vParameters, vPassword: String; vExecuteObject: TExecuteObject = nil; IgnoreError: Boolean = False; WaitIt: Boolean = False);
    procedure LoadIni;
    procedure Stop(WaitIt: Boolean = False);
  protected
    procedure DoShow; override;
  public
    ConsoleThread: TmnConsoleThread;
    destructor Destroy; override;
    constructor Create(TheOwner: TComponent); override;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

function IncludePathDelimiter(Const Path: string): string;
begin
  if Path <> '' then
    Result := IncludeTrailingPathDelimiter(Path)
  else
    Result := Path;
end;

{ TMainForm }

function TMainForm.ExpandFile(FileName: string): string;
begin
  if FileName <> '' then
    Result := ExpandFileName(ExpandToPath(FileName, Application.Location))
  else
    Result := '';
end;

procedure TMainForm.LoadIni;
var
  ini: TIniFile;
  f: string;
begin
  f := Application.Location + 'pgserver.ini';
  if FileExists(f) then
  begin
    ini := TIniFile.Create(Application.Location + 'pgserver.ini');
    try
      ini.BoolFalseStrings := ['false', 'False', 'off'];
      ini.BoolTrueStrings := ['true', 'True', 'on'];
      UserName := ini.ReadString('options', 'username', 'postgres');
      Password := ini.ReadString('options', 'password', '');
      Port := ini.ReadString('options', 'port', '');
      PGPath := IncludePathDelimiter(ExpandFile(ini.ReadString('options', 'pgpath', '')));
      DataPath := IncludePathDelimiter(ExpandFile(ini.ReadString('options', 'DataPath', '')));
      StartMinimized := ini.ReadBool('options', 'minimized', false);
      AutoStart := ini.ReadBool('options', 'start', false);
      ShowTray := ini.ReadBool('options', 'tray', true);
    finally
      ini.Free;
    end;
    Log('PGPath=' + PGPath);
    Log('DataPath=' + DataPath);
    Log('Info loaded');
  end;
end;

procedure TMainForm.Stop(WaitIt: Boolean);
var
  cmd: String;
begin
  if (PGPath = '') or (DataPath = '') then
  begin
    Log('Paths is empty, please edit pgserver.ini');
    exit;
  end;
  if WaitIt and (ConsoleThread <> nil) then
  begin
    ConsoleThread.FreeOnTerminate := False;
    ConsoleThread.OnTerminate := nil;
  end;
  //runservice
  cmd := '-s -D "' + DataPath + '" -w stop';
  Launch(False, 'Stopping server', 'pg_ctl.exe', cmd, Password, nil, false, WaitIt);
  if ConsoleThread <> nil then
  begin
    ConsoleThread.Terminate;
    if WaitIt then
    begin
      ConsoleThread.WaitFor;
      FreeAndNil(ConsoleThread);
    end;
  end;
end;

procedure TMainForm.DoShow;
begin
  inherited DoShow;
  if StartMinimized then
  begin
    Hide;
    StartMinimized := False;//do not use it in next show
  end;
end;

destructor TMainForm.Destroy;
begin
  IPCServer.StopServer;
  FDestroying := True;
  inherited Destroy;
end;

constructor TMainForm.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  IPCServer.ServerID := sApplicationID;
  IPCServer.StartServer;
  LoadIni;
  CheckServer;
  if StartMinimized then
    HideApp
  else if ShowTray then
    TrayIcon.Show;
  if AutoStart then
    StartAct.Execute;
end;

procedure TMainForm.TrayIconDblClick(Sender: TObject);
begin
  ShowApp;
end;

procedure GetIcon(ImageList: TCustomImageList; Index: Integer; aImage: TIcon; AllResolutions: Boolean);
var
  i: Integer;
  aResolution: TCustomImageListResolution;
begin
  with ImageList do
  begin
    aImage.Clear;
    for i :=0 to ResolutionCount - 1 do
    begin
      aResolution := ResolutionByIndex[i];
      aImage.Add(pf32bit, aResolution.Height, aResolution.Width);
      aImage.Current := i;
      aResolution.GetIcon(Index, aImage);
    end;
  end;
end;

procedure TMainForm.CheckServer;
begin
  if ConsoleThread <> nil then
  begin
    ImageList.GetIcon(0, TrayIcon.Icon);
    ImageList.GetIcon(0, Icon);
    StartAct.Enabled := False;
    StopAct.Enabled := True;
  end
  else
  begin
    ImageList.GetIcon(1, TrayIcon.Icon);
    ImageList.GetIcon(1, Icon);
    StartAct.Enabled := True;
    //StopAct.Enabled := False; //no, maybe we need to force stop
  end;
  //Application.Icon.Assign(TrayIcon.Icon);
end;

procedure TMainForm.Launch(AddIt: Boolean; vMessage, vExecutable, vParameters, vPassword: String; vExecuteObject: TExecuteObject; IgnoreError: Boolean; WaitIt: Boolean);
var
  aConsoleThread: TmnConsoleThread;
begin
  vExecutable := IncludeTrailingPathDelimiter(PGPath) + vExecutable;
  aConsoleThread := TmnConsoleThread.Create(vExecutable, PGPath, vParameters, @Log);
  aConsoleThread.OnTerminate := @ConsoleTerminated;
  aConsoleThread.Password := vPassword;
  aConsoleThread.Message := vMessage;
  aConsoleThread.ExecuteObject := vExecuteObject;
  aConsoleThread.IgnoreError := IgnoreError;

  if AddIt and (ConsoleThread = nil) then
    ConsoleThread := aConsoleThread;

  aConsoleThread.Start;
  if WaitIt then
    aConsoleThread.WaitFor;
end;

procedure TMainForm.ConsoleTerminated(Sender: TObject);
begin
  if not FDestroying then
  begin
    if ConsoleThread <> nil then
    begin
      {if ConsoleThread.Status = 0 then
        Log(ConsoleThread.Message + ' Done', lgDone)
      else
        Log('Error look the log', lgMessage);}
      ConsoleThread := nil;
      //FreeAndNil(ConsoleThread); //nop
    end;
    CheckServer;
  end;
end;

procedure TMainForm.ShowApp;
begin
  Visible := True;
  WindowState := wsNormal;
  ShowInTaskBar := stDefault;
  Show;
  if ShowTray then
    TrayIcon.Show
  else
    TrayIcon.Hide;
end;

procedure TMainForm.HideApp;
begin
  ShowInTaskBar := stNever;
  Hide;
  TrayIcon.Show;
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
//    lgMessage: MsgBox.Show(S);
  end;
  LogEdit.Lines.Add(Trim(S));
  if ScrollMnu.Checked then
    LogEdit.CaretY := LogEdit.Lines.Count;
end;

procedure TMainForm.ClearLogMnuClick(Sender: TObject);
begin
  LogEdit.Clear;
end;

procedure TMainForm.CheckActExecute(Sender: TObject);
var
  cmd: string;
begin
  if (PGPath = '') or (DataPath = '') then
  begin
    Log('Paths is empty, please edit pgserver.ini');
    exit;
  end;
  //runservice
  cmd := '-D "' + DataPath + '" -w status';
  Launch(False, 'Check Server:', 'pg_ctl.exe', cmd, Password);
end;

procedure TMainForm.ApplicationPropertiesEndSession(Sender: TObject);
begin
  FDestroying := True;
end;

procedure TMainForm.CloseActExecute(Sender: TObject);
begin
  FDestroying := True;
  Stop(True);
  Close;
end;

procedure TMainForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  if FDestroying then
    CloseAction := caFree
  else
  begin
    //Hide;
    CloseAction := caHide;
    HideApp;
  end;
end;

procedure TMainForm.FormWindowStateChange(Sender: TObject);
begin
  if WindowState = wsMinimized then
    HideApp;
end;

procedure TMainForm.ForceForegroundWindow;
{$ifdef windows}
var
  aForeThread, aAppThread: DWORD;
  aProcessID: DWORD;
  {$endif}
begin
  ShowApp;
  {$ifdef windows}
  aProcessID := 0;
  aForeThread := GetWindowThreadProcessId(GetForegroundWindow(), aProcessID);
  aAppThread := GetCurrentThreadId();

  if (aForeThread <> aAppThread) then
  begin
    AttachThreadInput(aForeThread, aAppThread, True);
    BringWindowToTop(Handle);
    AttachThreadInput(aForeThread, aAppThread, False);
  end
  else
    BringWindowToTop(Handle);
  {$endif}
  BringToFront;
end;

procedure TMainForm.IPCServerMessage(Sender: TObject);
var
  c: integer;
begin
  c := IPCServer.MsgType;
  case c of
    0: Show;
    1: ForceForegroundWindow;
    2: CloseAct.Execute;
  end;
end;

procedure TMainForm.IPCServerMessageQueued(Sender: TObject);
begin
  IPCServer.PeekMessage(IPCServer.ThreadTimeOut, True); //not sure if it a bug in FPC
end;

procedure TMainForm.StartActExecute(Sender: TObject);
var
  cmd: String;
begin
  if (PGPath = '') or (DataPath = '') then
  begin
    Log('Paths is empty, please edit pgserver.ini');
    exit;
  end;
  //runservice
  cmd := '-s -D "' + DataPath + '"';
  //cmd := cmd + ' -l "' + DataPath + 'pgserver_log.log' + '"';
  cmd := cmd + ' -w';
  cmd := cmd + ' start';
  Launch(true, 'Run server', 'pg_ctl.exe', cmd, Password);
  CheckServer;
end;

procedure TMainForm.StopActExecute(Sender: TObject);
begin
  Stop;
end;

procedure TMainForm.TrayIconClick(Sender: TObject);
begin
  ForceForegroundWindow;
end;

end.

