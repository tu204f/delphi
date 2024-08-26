program trader;

uses
  System.StartUpCopy,
  FMX.Forms,
  UnitMainForm in 'UnitMainForm.pas' {MainForm},
  Lb.Trade in 'lb\Lb.Trade.pas',
  Lb.ReadPrice in '..\lib\Lb.ReadPrice.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
