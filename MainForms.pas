unit MainForms;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Menus,
  {$ifdef WINDOWS}
  Windows, //TODO, i hate include it
  {$endif}
  ExtCtrls, ActnList, SynEdit, IniFiles, simpleipc,
  mnMsgBox, GUIMsgBox,
  ConsoleProcess;

const
  sApplicationID = 'PARMAJA.PGServer';

type

  { TMainForm }

  TMainForm = class(TForm)
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
    TrayPopupMenu: TPopupMenu;
    ScrollMnu: TMenuItem;
    LogEdit: TSynEdit;
    StatusTimer: TTimer;
    TrayIcon: TTrayIcon;
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
    procedure ForceForegroundWindow;
    procedure Log(S: String; Kind: TmnLogKind = lgLog);
    procedure Launch(AddIt: Boolean; vMessage, vExecutable, vParameters, vPassword: String; vExecuteObject: TExecuteObject = nil; IgnoreError: Boolean = False);
    procedure LoadIni;
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
      PGPath := IncludePathDelimiter(ini.ReadString('options', 'pgpath', ''));
      DataPath := IncludePathDelimiter(ini.ReadString('options', 'DataPath', ''));
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
  TrayIcon.Icon.Assign(Application.Icon);
  if StartMinimized then
  begin
    //WindowState := wsMinimized;
    Visible := False;
    TrayIcon.Show;
  end
  else if ShowTray then
    TrayIcon.Show;
  if AutoStart then
    StartAct.Execute;
end;

procedure TMainForm.TrayIconDblClick(Sender: TObject);
begin
  WindowState := wsNormal;
  Visible := True;
  Show;
  ShowInTaskBar := stDefault;
  if not ShowTray then
    TrayIcon.Hide;
end;

procedure TMainForm.CheckServer;
begin
  if ConsoleThread <> nil then
    ImageList.GetIcon(0, TrayIcon.Icon)
  else
    ImageList.GetIcon(1, TrayIcon.Icon);
end;

procedure TMainForm.Launch(AddIt: Boolean; vMessage, vExecutable, vParameters, vPassword: String; vExecuteObject: TExecuteObject; IgnoreError: Boolean);
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
    end;
    CheckServer;
  end;
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

procedure TMainForm.CloseActExecute(Sender: TObject);
begin
  FDestroying := True;
  StopAct.Execute;
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
    ShowInTaskBar := stNever;
    TrayIcon.Show;
  end;
end;

procedure TMainForm.FormWindowStateChange(Sender: TObject);
begin
  if WindowState = wsMinimized then
  begin
//    WindowState := wsNormal;
    Hide;
    ShowInTaskBar := stNever;
    TrayIcon.Show;
  end;
end;

procedure TMainForm.ForceForegroundWindow;
{$ifdef windows}
var
  aForeThread, aAppThread: DWORD;
  aProcessID: DWORD;
  {$endif}
begin
  Visible := True;
  WindowState := wsNormal;
  Show;
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
  //runservice
  cmd := '-D "' + DataPath + '"';
  cmd := cmd + ' -l "' + DataPath + 'pgserver_log.log' + '"';
  cmd := cmd + ' -w';
  cmd := cmd + ' start';
  Launch(true, 'Run server', 'pg_ctl.exe', cmd, Password);
  CheckServer;
end;

procedure TMainForm.StopActExecute(Sender: TObject);
var
  cmd: String;
begin
  //runservice
  cmd := '-D "' + DataPath + '" -w stop';
  Launch(False, 'Stopping server', 'pg_ctl.exe', cmd, Password);

  if ConsoleThread <> nil then
  begin
    ConsoleThread.Kill;
    ConsoleThread.Terminate;
    //ConsoleThread.WaitFor;
    //FreeAndNil(ConsoleThread);
  end;
  CheckServer;
end;

procedure TMainForm.TrayIconClick(Sender: TObject);
begin

end;

end.

