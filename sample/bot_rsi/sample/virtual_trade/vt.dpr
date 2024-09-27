program vt;

uses
  System.StartUpCopy,
  FMX.Forms,
  UnitMainForm in 'UnitMainForm.pas' {MainForm},
  Lb.VirtualTrade in '..\..\lib\Lb.VirtualTrade.pas',
  Lb.Logger in '..\..\..\..\library\Lb.Logger.pas',
  Lb.VirtualTrade.V2 in '..\..\lib\Lb.VirtualTrade.V2.pas';

{$R *.res}

begin
  TLogger.ClearLog;

  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
