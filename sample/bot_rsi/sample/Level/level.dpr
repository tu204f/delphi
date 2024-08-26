program level;

uses
  System.StartUpCopy,
  FMX.Forms,
  UnitMainForm in 'UnitMainForm.pas' {MainForm},
  Lb.Level in '..\..\lib\Lb.Level.pas',
  Lb.Trader.V2 in '..\..\lib\Lb.Trader.V2.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
