unit MainForms;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Menus,
  ExtCtrls, SynEdit, IniFiles,
  mnMsgBox, GUIMsgBox,
  ConsoleProcess;

type

  { TMainForm }

  TMainForm = class(TForm)
    ClearLogMnu: TMenuItem;
    InfoPanel: TPanel;
    LogPopupMenu: TPopupMenu;
    ScrollMnu: TMenuItem;
    StartBtn: TButton;
    LogEdit: TSynEdit;
    CloseBtn: TButton;
    StatusTimer: TTimer;
    StopBtn: TButton;
    TrayIcon: TTrayIcon;
    procedure ClearLogMnuClick(Sender: TObject);
    procedure CloseBtnClick(Sender: TObject);
    procedure FormWindowStateChange(Sender: TObject);
    procedure StartBtnClick(Sender: TObject);
    procedure StopBtnClick(Sender: TObject);
    procedure TrayIconDblClick(Sender: TObject);
  private
    FStop: Boolean;
    FDestroying: Boolean;
    UserName: string;
    Password: string;
    Port: string;
    PGPath: string;
    DataPath: string;
    procedure ConsoleTerminated(Sender: TObject);
    procedure Log(S: String; Kind: TmnLogKind = lgLog);
    procedure Launch(vMessage, vExecutable, vParameters, vPassword: String; vExecuteObject: TExecuteObject = nil; IgnoreError: Boolean = False);
    procedure LoadIni;
  public
    ConsoleThread: TmnConsoleThread;
    destructor Destroy; override;
    constructor Create(TheOwner: TComponent); override;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

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
      UserName := ini.ReadString('options', 'username', 'postgres');
      Password := ini.ReadString('options', 'password', '');
      Port := ini.ReadString('options', 'port', '');
      PGPath := ini.ReadString('options', 'pgpath', '');
      DataPath := ini.ReadString('options', 'DataPath', '');
    finally
      ini.Free;
    end;
    Log('Info loaded');
  end;
end;

destructor TMainForm.Destroy;
begin
  FDestroying := True;
  FStop := True;
  inherited Destroy;
end;

constructor TMainForm.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  LoadIni;
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

procedure TMainForm.TrayIconDblClick(Sender: TObject);
begin
  WindowState := wsNormal;
  Show;
  ShowInTaskBar := stDefault;
  TrayIcon.Hide;
end;

procedure TMainForm.Launch(vMessage, vExecutable, vParameters, vPassword: String; vExecuteObject: TExecuteObject; IgnoreError: Boolean);
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
    lgMessage: MsgBox.Show(S);
  end;
  LogEdit.Lines.Add(Trim(S));
  if ScrollMnu.Checked then
    LogEdit.CaretY := LogEdit.Lines.Count;
end;

procedure TMainForm.StartBtnClick(Sender: TObject);
var
  cmd: String;
begin
  //runservice
  cmd := '-D "' + DataPath + '" -w start';
  Launch('Starting server', 'pg_ctl.exe', cmd, Password);
end;

procedure TMainForm.ClearLogMnuClick(Sender: TObject);
begin
  LogEdit.Clear;
end;

procedure TMainForm.CloseBtnClick(Sender: TObject);
begin
  Close;
end;

procedure TMainForm.FormWindowStateChange(Sender: TObject);
begin
  if WindowState = wsMinimized then
  begin
    WindowState := wsNormal;
    Hide;
    ShowInTaskBar := stNever;
    TrayIcon.Show;
  end;
end;

end.

