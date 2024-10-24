unit MainForms;
{$mode objfpc}{$H+}
{-----------------------------------------------------------------------------
  Author:   Zaher Dirkey
  Purpose:
  License:   mit(https://opensource.org/licenses/MIT)
-----------------------------------------------------------------------------}

{
  TODO Upgrade
  Change hba method to trust
  Change ports in postgresql.conf
  set PGUSER=postgres
  set PGPASSWORD=
  pg_upgrade.exe -p 5433 -d "d:\data\pg13" -b "D:\programs\pg13-64\bin" -P 5432 -D "d:\data\pg14" -B "D:\programs\pg14-64\bin"

  psql -p 5432 -f c:\temp\update_extensions.sql
}

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
    InitDataAct: TAction;
    HideAct: TAction;
    ApplicationProperties: TApplicationProperties;
    CheckAct: TAction;
    CloseBtn: TButton;
    CloseBtn1: TButton;
    ImageList: TImageList;
    IPCServer: TSimpleIPCServer;
    Panel1: TPanel;
    StartAct: TAction;
    StartBtn: TButton;
    InitDataBtn: TButton;
    StopAct: TAction;
    ExitAct: TAction;
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
    procedure ExitActExecute(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormWindowStateChange(Sender: TObject);
    procedure HideActExecute(Sender: TObject);
    procedure InitDataActExecute(Sender: TObject);
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

//Ported from Trim functions

function TrimEOL(const S: string): string;
var Ofs, Len: integer;
begin
  len := Length(S);
  while (Len>0) and (S[Len] in [#13, #10]) do
   dec(Len);
  Ofs := 1;
  while (Ofs<=Len) and (S[Ofs] in [#13, #10]) do
   Inc(Ofs);
  result := Copy(S, Ofs, 1 + Len - Ofs);
end ;

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
  f := Application.Location + Application.Name + '.ini';
  if FileExists(f) then
  begin
    ini := TIniFile.Create(Application.Location + Application.Name + '.ini');
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
      SetBounds(Left, Top, ini.ReadInteger('Size', 'Width', Width), ini.ReadInteger('Size', 'Height', Height));
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

  //runservice
  cmd := '-s -D "' + DataPath + '" -w stop';
  Launch(False, 'Stopping server', 'pg_ctl.exe', cmd, Password, nil, false, WaitIt);

  if (ConsoleThread <> nil) then
  begin
    if WaitIt then
    begin
      ConsoleThread.FreeOnTerminate := False;
      ConsoleThread.OnTerminate := nil;
    end;

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

procedure GetIcon(ImageList: TCustomImageList; Index: Integer; aImage: TIcon);
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
    ImageList.GetIcon(0, Application.Icon);
    //Icon.LoadFromFile(Application.Location + 'pgserver.ico');
    //Application.Icon.LoadFromFile(Application.Location + 'pgserver.ico');
    StartAct.Enabled := False;
    StopAct.Enabled := True;
  end
  else
  begin
    ImageList.GetIcon(1, TrayIcon.Icon);
    ImageList.GetIcon(1, Icon);
    ImageList.GetIcon(1, Application.Icon);
    StartAct.Enabled := True;
    //StopAct.Enabled := False; //no, maybe we need to force stop
  end;
end;

procedure TMainForm.Launch(AddIt: Boolean; vMessage, vExecutable, vParameters, vPassword: String; vExecuteObject: TExecuteObject; IgnoreError: Boolean; WaitIt: Boolean);
var
  aConsoleThread: TmnConsoleThread;
begin
  vExecutable := IncludeTrailingPathDelimiter(PGPath) + vExecutable;
  aConsoleThread := TmnConsoleThread.Create(vExecutable, PGPath, vParameters, @Log);
  if WaitIt then
    aConsoleThread.FreeOnTerminate := False
  else if AddIt then
    aConsoleThread.OnTerminate := @ConsoleTerminated;
  aConsoleThread.Password := vPassword;
  aConsoleThread.Message := vMessage;
  aConsoleThread.ExecuteObject := vExecuteObject;
  aConsoleThread.IgnoreError := IgnoreError;

  if AddIt then
  begin
    if (ConsoleThread <> nil) then
      raise Exception.Create('Running Process is already exists');
    ConsoleThread := aConsoleThread;
  end;

  aConsoleThread.Start;
  if WaitIt then
  begin
    aConsoleThread.WaitFor;
    FreeAndNil(aConsoleThread)
  end;
end;

procedure TMainForm.ConsoleTerminated(Sender: TObject);
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
  if not FDestroying then
  begin
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
    else ;
//    lgMessage: MsgBox.Show(S);
  end;
  LogEdit.Lines.Add(TrimEOL(S));
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
  Launch(False, 'Checking Server:', 'pg_ctl.exe', cmd, Password);
end;

procedure TMainForm.ApplicationPropertiesEndSession(Sender: TObject);
begin
  FDestroying := True;
end;

procedure TMainForm.ExitActExecute(Sender: TObject);
begin
  FDestroying := True;
  Stop(True);
  Close;
end;

procedure TMainForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
var
  ini: TIniFile;
begin
  if FDestroying then
  begin
    CloseAction := caFree;
    ini := TIniFile.Create(Application.Location + Application.Name + '.ini');
    try
      ini.WriteInteger('Size', 'Width', Width);
      ini.WriteInteger('Size', 'Height', Height);
    finally
      ini.Free;
    end;
  end
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

procedure TMainForm.HideActExecute(Sender: TObject);
begin
  HideApp;
end;

procedure TMainForm.InitDataActExecute(Sender: TObject);
var
  cmd: String;
  aPassword, aPath: string;
  //C:\programs\pg17-64\bin\initdb -D D:\data\pg17 -U postgres --encoding=UTF8 -W --no-locale --auth-host=md5
begin
  aPath := DataPath;
  //if SelectDirectory(aPath, [sdAllowCreate, sdPerformCreate, sdPrompt], 0) then
  begin
    if Password = '' then
      MsgBox.Password(Password, 'Enter postgre password');
    ForceDirectories(aPath);
    cmd := ' -D "' + aPath + '" --encoding=UTF8 --no-locale --auth-host=md5';
    if Password <> ''  then
    begin
      cmd := cmd+ ' --username=' + UserName + ' --pwprompt';
      aPassword := Password;
    end
    else
    begin
      cmd := cmd+ ' --username=' + UserName;
      aPassword := '';
    end;
    Launch(true, 'Init database folder', 'initdb', cmd, aPassword); //TODO twice of password
{
    cmd := '"pg_ctl.exe initdb -D ''' + aPath + ''' -w -U ' + UserName + ' -P ' + Password + ' --encoding=UTF8"';
    Launch(true, 'Init database folder', 'runas /user:' + UserName, cmd, password);
}
  end;
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
    2: ExitAct.Execute;
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
  Launch(true, 'Runing server', 'pg_ctl.exe', cmd, Password);
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

