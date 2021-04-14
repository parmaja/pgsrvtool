program pgserver;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  {$IFDEF HASAMIGA}
  athreads,
  {$ENDIF}
  SysUtils, StrUtils,
  Interfaces, // this includes the LCL widgetset
  simpleipc,
  Forms, MainForms
  { you can add units after this };

{$R *.res}

function IsAnotherInstance: Boolean;
var
  aClient: TSimpleIPCClient;
begin
  aClient := TSimpleIPCClient.Create(nil);
  try
    aClient.ServerID := sApplicationID;
    Result := aClient.ServerRunning;//There is another instance
    if Result then
    begin
      aClient.Connect;
      try
        aClient.SendStringMessage(1, 'Show');
      finally
        aClient.Disconnect;
      end;
    end
  finally
    aClient.Free;
  end;
end;

begin
  RequireDerivedFormResource :=True;
  if not IsAnotherInstance then
  begin
  Application.Scaled :=True;
    Application.Initialize;
    Application.CreateForm(TMainForm, MainForm);
    Application.Run;
  end;
end.

