program pgserver;
{$mode objfpc}{$H+}
{-----------------------------------------------------------------------------
  Author:   Zaher Dirkey
  Purpose:
  License:   mit(https://opensource.org/licenses/MIT)
-----------------------------------------------------------------------------}

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
  Forms, MainForms;

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
  if IsAnotherInstance then
    exit;
  Application.Name := 'pgsrvtool';
  Application.Title:='PG Server GUI';
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.

